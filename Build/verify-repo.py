#!/usr/bin/env python3
"""Repository policy and security gate for AltCover.

These checks deliberately depend on nothing but the Python 3 standard library so
that they can run on any developer machine and on any CI runner *before* the
.NET/Mono toolchain is restored.  They exist because a coverage/instrumentation
tool is routinely run inside CI pipelines where a mistake is far more expensive
than a unit-test failure, and because the legacy build (FAKE + VS2017 + Mono +
.NET Core 2.1) is not available on every runner.

Run with:

    python Build/verify-repo.py            # human readable, exit 0 == pass
    python Build/verify-repo.py --quiet    # summary only

Add ``--list`` to enumerate the individual checks.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ET

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# The only package source AltCover is allowed to restore from.  Anything else is
# an unreviewed supply-chain hop: see SECURITY.md ("Package sources").
ALLOWED_NUGET_SOURCES = (
    "https://api.nuget.org/v3/index.json",
    "https://api.nuget.org/v3-flatcontainer/",
)

# Per-file exceptions to the allow-list above, each one deliberate and each one
# tracked.  A new entry here is a security decision: justify it in the pull
# request and give it an owner.
#
# Visualizer/AltCover.Avalonia.fsproj pins Avalonia 0.6.2-build5768-beta, a CI
# build that was published to a MyGet feed and never to nuget.org (which goes
# 0.6.1 -> 0.7.0-build445-beta).  The visualizer therefore cannot be moved onto
# nuget.org without also recompiling it against a different Avalonia API, which
# is not a change to make blind.  Removing this entry is a tracked follow-up,
# recorded in Visualizer/nuget.config and in SECURITY.md.
NUGET_SOURCE_EXCEPTIONS = {
    "Visualizer/nuget.config": frozenset(
        {
            "https://www.myget.org/F/avalonia-ci/api/v2",
        }
    ),
}

# Strong-name keys that are already public in git history.  They are kept only so
# that the FAKE build keeps working; see SECURITY.md ("Strong-name keys") for the
# required rotation.  Any *new* key material must fail this gate.
KNOWN_PUBLIC_KEYS = frozenset(
    {
        "Build/Infrastructure.snk",
        "Build/Recorder.snk",
        "Build/SelfTest.snk",
    }
)

# Files whose contents are checked for obvious credential shapes.
SENSITIVE_SUFFIXES = (".snk", ".pfx", ".p12", ".key", ".pem", ".jks", ".keystore")

# XML project/config files that must always parse; a malformed one breaks restore
# or the build in a way that is tedious to diagnose from CI logs.
#
# .resx is deliberately excluded: the Visualizer resources legitimately contain
# `&#x0;` character references (a legacy NUL-in-a-font-name quirk that the .NET
# resource reader accepts) which a strict XML 1.0 parser rejects.
XML_EXTENSIONS = (".config", ".props", ".targets", ".csproj", ".fsproj", ".rules")

IGNORED_DIRS = {".git", "bin", "obj", "packages", "_Binaries", "_Reports", "_Mono",
                "_Packaging", "_Publish", "_Generated", "_artifacts", "nupkg"}

# Credential-ish patterns.  Deliberately narrow: this is a backstop against
# "obviously a secret" regressions, not a replacement for gitleaks, which runs
# separately in CI.
SECRET_PATTERNS = (
    (re.compile(r"-----BEGIN (?:RSA |EC |DSA |OPENSSH |PGP )?PRIVATE KEY-----"),
     "private key block"),
    (re.compile(r"(?i)\baws_secret_access_key\s*[=:]\s*\S+"), "AWS secret access key"),
    (re.compile(r"(?i)\bazure_storage_(?:key|connection string)\s*[=:]\s*\S+"), "Azure storage key"),
    (re.compile(r"(?i)\b(?:client_?secret|api_?key|auth_?token)\s*[=:]\s*['\"][^'\"\s]{16,}['\"]"),
     "hardcoded credential literal"),
    (re.compile(r"(?i)\b(?:password|passwd|pwd)\s*[=:]\s*['\"][^'\"\s]{6,}['\"]"),
     "hardcoded password literal"),
)

TEXT_SUFFIXES = (".fs", ".fsx", ".fsproj", ".cs", ".csproj", ".ps1", ".sh", ".yml",
                 ".yaml", ".json", ".config", ".props", ".targets", ".md", ".txt",
                 ".editorconfig", ".gitignore", ".gitattributes", ".example")


# --------------------------------------------------------------------------- #
# helpers
# --------------------------------------------------------------------------- #
def rel(path: str) -> str:
    return os.path.relpath(path, REPO_ROOT).replace(os.sep, "/")


def read(relpath: str) -> str:
    """Read a repository-relative file.

    Joined against REPO_ROOT rather than opened relative to the process working
    directory, so the gate inspects the same files no matter where it is invoked
    from. (The two used to disagree, and every check silently read the wrong file
    whenever the cwd was not the repository root.)
    """
    with open(os.path.join(REPO_ROOT, relpath), "r", encoding="utf-8-sig",
              errors="replace") as handle:
        return handle.read()


def exists(relpath: str) -> bool:
    return os.path.exists(os.path.join(REPO_ROOT, relpath))


def normalise_source(url: str) -> str:
    """Compare feed URLs exactly, but not on incidental punctuation.

    Scheme and host case are not significant; a trailing slash and surrounding
    whitespace are not either.  Everything else -- the path, and in particular
    whether the scheme is https -- is, because that is the part the allow-list
    is protecting.
    """
    url = url.strip().rstrip("/")
    if "://" in url:
        scheme, _, rest = url.partition("://")
        url = scheme.lower() + "://" + rest.lower()
    return url


def normalise_source_set(urls) -> frozenset:
    return frozenset(normalise_source(u) for u in urls)


def walk(include_ext: tuple, skip_ext: tuple = ()):
    """Yield repo-relative paths, skipping build output and VCS metadata."""
    for dirpath, dirnames, filenames in os.walk(REPO_ROOT):
        dirnames[:] = [d for d in dirnames if d not in IGNORED_DIRS]
        for name in filenames:
            if name.startswith("."):
                continue
            _, ext = os.path.splitext(name)
            if include_ext and ext.lower() not in include_ext:
                continue
            if skip_ext and ext.lower() in skip_ext:
                continue
            yield rel(os.path.join(dirpath, name))


class Result:
    def __init__(self) -> None:
        self.checks = []  # (name, ok, [messages])

    def record(self, name: str, ok: bool, messages=None) -> bool:
        self.checks.append((name, ok, list(messages or ())))
        return ok

    @property
    def failures(self):
        return [c for c in self.checks if not c[1]]

    def report(self, quiet: bool) -> int:
        for name, ok, messages in self.checks:
            if ok:
                if not quiet:
                    print("  PASS  %s" % name)
                continue
            print("  FAIL  %s" % name)
            for message in messages:
                print("          %s" % message)
        total = len(self.checks)
        bad = len(self.failures)
        print("")
        if bad:
            print("verify-repo: %d of %d checks FAILED" % (bad, total))
        else:
            print("verify-repo: all %d checks passed" % total)
        return 1 if bad else 0


# --------------------------------------------------------------------------- #
# individual checks
# --------------------------------------------------------------------------- #
def check_nuget_sources(result: Result) -> None:
    """Only nuget.org may be an enabled package source.

    Rationale: every extra feed widens the dependency-confusion / typosquat
    surface.  The three MyGet "F-*" feeds this repository used to trust were
    retired years ago, so they can only break restore today.
    """
    messages = []
    # Only real NuGet configuration files carry the source policy. `packages.config`
    # is a *package* manifest and `App.config` is application configuration;
    # neither declares sources, and treating them as if they did produced a wall
    # of false positives.
    # walk() already yields repository-relative paths. Relativising them again
    # only happens to work when the current directory is the repository root,
    # because relpath() resolves a relative argument against the cwd -- so this
    # would break the moment anyone ran the script from elsewhere.
    configs = sorted(
        p for p in walk((".config",))
        if os.path.basename(p).lower() == "nuget.config"
    )
    for name in configs:
        path = os.path.join(REPO_ROOT, name)
        if not os.path.isfile(path):
            continue
        try:
            root = ET.parse(path).getroot()
        except ET.ParseError as err:
            messages.append("%s: not well-formed XML (%s)" % (name, err))
            continue

        def local_name(element):
            return element.tag.rsplit("}", 1)[-1]

        # Sources named in <disabledPackageSources> are inert; NuGet spells them
        # as <add key="name" value="true"/> children of the container, so read the
        # key off the child, not off the container.
        disabled = {
            (node.get("key") or "").strip().lower()
            for container in root.iter()
            if local_name(container) == "disabledPackageSources"
            for node in container
            if local_name(node) == "add"
            and (node.get("key") or "").strip()
            and str(node.get("value") or "true").strip().lower() in ("true", "1")
        }
        # The enabled sources are the <add> children of <packageSources>.
        enabled = [
            node
            for container in root.iter()
            if local_name(container) == "packageSources"
            for node in container
            if local_name(node) == "add"
        ]
        if not enabled:
            messages.append("%s: declares no package source at all; restore "
                            "would fail" % name)
            continue
        allowed = normalise_source_set(
            tuple(ALLOWED_NUGET_SOURCES) + tuple(NUGET_SOURCE_EXCEPTIONS.get(name, ()))
        )
        for node in enabled:
            key = (node.get("key") or "").strip()
            value = normalise_source(node.get("value") or "")
            if not value:
                messages.append("%s: package source %r has no value" % (name, key))
                continue
            if key.lower() in disabled:
                continue
            if value not in allowed:
                messages.append(
                    "%s: package source %r (%s) is not an allow-listed source; "
                    "disable it under <disabledPackageSources>, remove it, or add "
                    "a documented exception in Build/verify-repo.py"
                    % (name, key, value)
                )
    result.record("nuget: enabled package sources are allow-listed", not messages,
                  messages)


def check_no_new_key_material(result: Result) -> None:
    """No new private key material may be introduced.

    `git ls-files` is used so that build output under packages/ or obj/ (which
    legitimately contains .snk files restored from NuGet) cannot trip the gate.
    """
    messages = []
    try:
        tracked = subprocess.run(
            ["git", "ls-files", "-z"],
            cwd=REPO_ROOT, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
            timeout=60, check=False,
        ).stdout.decode("utf-8", "replace")
    except (OSError, subprocess.SubprocessError) as err:
        result.record("secrets: no new key material is tracked", True,
                      ["skipped: git unavailable (%s)" % err])
        return

    for name in tracked.split("\0"):
        if not name:
            continue
        if not name.lower().endswith(SENSITIVE_SUFFIXES):
            continue
        if name.replace("\\", "/") in KNOWN_PUBLIC_KEYS:
            continue
        messages.append(
            "%s: private key material is tracked in git; it is already in the "
            "public history so it must be considered compromised -- rotate and "
            "move it to a secret store" % name
        )
    result.record("secrets: no new key material is tracked", not messages, messages)


def check_no_credential_literals(result: Result) -> None:
    """Backstop against obvious credential literals in tracked text files."""
    messages = []
    try:
        tracked = subprocess.run(
            ["git", "ls-files", "-z", "--", "*.fs", "*.fsx", "*.cs", "*.ps1",
             "*.sh", "*.yml", "*.yaml", "*.json", "*.config", "*.props",
             "*.targets", "*.md", "*.env", ".env.example"],
            cwd=REPO_ROOT, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
            timeout=60, check=False,
        ).stdout.decode("utf-8", "replace")
    except (OSError, subprocess.SubprocessError):
        tracked = ""

    names = [n for n in tracked.split("\0") if n]
    for name in names:
        _, ext = os.path.splitext(name)
        if ext.lower() not in TEXT_SUFFIXES and name not in TEXT_SUFFIXES:
            continue
        path = os.path.join(REPO_ROOT, name)
        if not os.path.isfile(path):
            continue
        try:
            lines = read(path).splitlines()
        except OSError:
            continue
        for number, line in enumerate(lines, start=1):
            if "ci.appveyor" in line and "secure:" in line:
                continue  # appveyor's encrypted values are not plaintext
            for pattern, label in SECRET_PATTERNS:
                if pattern.search(line):
                    messages.append("%s:%d: %s" % (name, number, label))
    result.record("secrets: no credential literals in tracked text files",
                  not messages, messages)


def check_xml_well_formed(result: Result) -> None:
    """Every project/config XML file must parse.

    A malformed csproj/fsproj/NuGet.config surfaces as an opaque restore failure
    on a clean machine, so it is much cheaper to catch it here.
    """
    messages = []
    checked = 0
    for name in sorted(walk(XML_EXTENSIONS)):
        checked += 1
        try:
            ET.parse(os.path.join(REPO_ROOT, name))
        except (ET.ParseError, OSError) as err:
            messages.append("%s: %s" % (name, err))
    result.record("xml: %d project/config file(s) are well-formed" % checked,
                  not messages, messages)


def check_docker_context(result: Result) -> None:
    """.dockerignore must keep VCS metadata and key material out of the image."""
    messages = []
    if not exists(".dockerignore"):
        messages.append(".dockerignore is missing; `COPY . .` would embed .git "
                        "and the committed .snk keys in the published image")
    else:
        patterns = {line.strip() for line in read(".dockerignore").splitlines()
                    if line.strip() and not line.strip().startswith("#")}
        required = {
            ".git": "VCS history (and any secret ever committed) would be baked "
                    "into the image layer",
            "**/*.snk": "strong-name private keys would be baked into the image layer",
            "**/*.pfx": "certificate private keys would be baked into the image layer",
        }
        for pattern, why in required.items():
            if pattern not in patterns:
                messages.append(".dockerignore is missing %r: %s" % (pattern, why))
    result.record("docker: build context excludes .git and key material",
                  not messages, messages)


def check_dockerfile(result: Result) -> None:
    """The image must not run as root and must not copy the whole world blindly."""
    messages = []
    if not exists("Dockerfile"):
        messages.append("Dockerfile is missing")
    else:
        lines = read("Dockerfile").splitlines()
        directives = [l.strip() for l in lines
                      if l.strip() and not l.strip().startswith("#")]
        users = [l for l in directives if l.upper().startswith("USER ")]
        if not users:
            messages.append("Dockerfile never switches away from root (no USER "
                            "instruction); add a non-root user")
        elif users[-1].split(None, 1)[1].strip().lower() in ("root", "0", "0:0"):
            messages.append("Dockerfile's final USER is root")
        for line in directives:
            if line.upper().startswith("FROM "):
                parts = line.split()
                image = parts[1]
                if image in ("scratch",) or image.endswith(":latest"):
                    messages.append("FROM %s: 'latest' is not reproducible; pin a "
                                    "specific tag" % image)
                if "@sha256:" not in image:
                    # Not fatal, but the reviewer should know.
                    pass
    result.record("docker: image runs unprivileged and pins its base tag",
                  not messages, messages)


def check_compose(result: Result) -> None:
    """docker-compose must not publish ports nothing listens on or grant root."""
    messages = []
    if not exists("docker-compose.yml"):
        return  # nothing to enforce
    text = read("docker-compose.yml")
    if re.search(r"(?m)^\s*-\s*\"?\d+:\d+\"?\s*$", text):
        messages.append("docker-compose.yml publishes a host port; the dev shell "
                        "has no listener, so this only widens the attack surface")
    if "no-new-privileges" not in text:
        messages.append("docker-compose.yml does not set no-new-privileges")
    result.record("docker: compose drops privileges and exposes no host ports",
                  not messages, messages)


def check_global_json(result: Result) -> None:
    """global.json must allow the pinned SDK band to roll forward.

    An exact-pinned SDK makes a fresh clone fail on every machine that does not
    happen to have that patch level installed, which is the single most common
    reason contributors give up on a repository.
    """
    messages = []
    if not exists("global.json"):
        messages.append("global.json is missing; SDK selection is unmanaged")
    else:
        try:
            with open(os.path.join(REPO_ROOT, "global.json"), "r",
                      encoding="utf-8-sig") as handle:
                document = json.load(handle)
        except ValueError as err:
            messages.append("global.json is not well-formed JSON (%s)" % err)
            document = None
        if isinstance(document, dict):
            sdk = document.get("sdk")
            if not isinstance(sdk, dict) or not str(sdk.get("version", "")).strip():
                messages.append("global.json does not pin an SDK version")
            elif not str(sdk.get("rollForward", "")).strip():
                messages.append(
                    "global.json pins %s with no rollForward policy; a fresh "
                    "clone will fail unless that exact patch level is installed"
                    % sdk.get("version")
                )
    result.record("sdk: global.json pins a roll-forward band", not messages, messages)


def expand_sections(section: str):
    """Expand an .editorconfig glob section into its individual patterns.

    `[*.{fs,fsx,fsproj}]` is three patterns, not one, and a naive substring test
    against "*.fs" would not match it. Editors do the expansion, so the gate has
    to as well, or it reports a false negative on a perfectly good config.
    """
    start = section.find("{")
    if start == -1:
        return [section]
    depth = 0
    for index in range(start, len(section)):
        if section[index] == "{":
            depth += 1
        elif section[index] == "}":
            depth -= 1
            if depth == 0:
                prefix = section[:start]
                suffix = section[index + 1:]
                inner = section[start + 1:index]
                options, current, nested = [], "", 0
                for char in inner:
                    if char == "{":
                        nested += 1
                    elif char == "}":
                        nested -= 1
                    if char == "," and nested == 0:
                        options.append(current)
                        current = ""
                    else:
                        current += char
                options.append(current)
                return [prefix + option + suffix for option in options]
    return [section]  # unbalanced braces; treat the section as a literal


def check_editorconfig(result: Result) -> None:
    """A lint/format baseline must exist and cover the languages in the tree."""
    messages = []
    if not exists(".editorconfig"):
        messages.append(".editorconfig is missing; formatting is unenforced "
                        "across a 30+ project tree")
    else:
        text = read(".editorconfig")
        # .editorconfig is INI, not XML: sections are "[glob]" lines.
        sections = []
        for line in text.splitlines():
            line = line.strip()
            if line.startswith("[") and line.endswith("]"):
                sections.extend(expand_sections(line[1:-1]))
        for glob in ("*.fs", "*.fsx", "*.cs"):
            if not any(glob in section for section in sections):
                messages.append(".editorconfig does not cover %s" % glob)
        if "root = true" not in text:
            messages.append(".editorconfig is missing the `root = true` header")
    result.record("lint: .editorconfig covers the source languages", not messages,
                  messages)


def check_env_example(result: Result) -> None:
    """If compose consumes variables, .env.example must document them."""
    messages = []
    if not exists("docker-compose.yml"):
        return
    text = read("docker-compose.yml")
    used = set(re.findall(r"\$\{([A-Za-z_][A-Za-z0-9_]*)", text))
    if not used:
        return
    if not exists(".env.example"):
        messages.append("docker-compose.yml consumes %s but there is no "
                        ".env.example" % ", ".join(sorted(used)))
    else:
        declared = {
            line.split("=", 1)[0].strip()
            for line in read(".env.example").splitlines()
            if line.strip() and not line.strip().startswith("#") and "=" in line
        }
        for name in sorted(used - declared):
            messages.append("docker-compose.yml consumes $%s but .env.example "
                            "does not declare it" % name)
    result.record("docs: every compose variable is declared in .env.example",
                  not messages, messages)


def check_gitignore(result: Result) -> None:
    """Build output must stay out of git."""
    messages = []
    if not exists(".gitignore"):
        messages.append(".gitignore is missing")
    else:
        text = read(".gitignore")
        for pattern in ("**/bin/", "**/obj/", "packages/"):
            if pattern not in text:
                messages.append(".gitignore does not exclude %r" % pattern)
    result.record("vcs: build output is git-ignored", not messages, messages)


def check_required_files_tracked(result: Result) -> None:
    """The files that carry the CI and policy guarantees must not be ignored.

    This exists because of a real regression: `.gitignore` carried a blanket
    `.*/` pattern, which silently ignored every dot-directory including
    `.github/`. A workflow, dependabot config or issue template committed under
    such a path simply never enters the repository, so the pipeline quietly
    does not exist and nothing errors. `git check-ignore` is the only reliable
    way to see that, hence this check.
    """
    messages = []
    required = (
        ".github/workflows/ci.yml",
        ".github/dependabot.yml",
        ".editorconfig",
        ".dockerignore",
        ".env.example",
        "SECURITY.md",
        "Build/verify-repo.py",
    )
    for name in required:
        if not exists(name):
            messages.append("%s: missing; the repository policy is incomplete" % name)
            continue
        try:
            ignored = subprocess.run(
                ["git", "check-ignore", "-q", "--", name],
                cwd=REPO_ROOT, stderr=subprocess.DEVNULL,
                timeout=30, check=False,
            ).returncode == 0
        except (OSError, subprocess.SubprocessError):
            ignored = False
        if ignored:
            messages.append(
                "%s: is matched by .gitignore, so it will never be committed and "
                "the guarantee it encodes will silently not exist" % name
            )
    result.record("vcs: policy and CI files are not git-ignored", not messages,
                  messages)


CHECKS = (
    ("nuget-source-policy", check_nuget_sources),
    ("tracked-key-material", check_no_new_key_material),
    ("credential-literals", check_no_credential_literals),
    ("xml-well-formed", check_xml_well_formed),
    ("docker-build-context", check_docker_context),
    ("dockerfile-hardening", check_dockerfile),
    ("compose-hardening", check_compose),
    ("sdk-reproducibility", check_global_json),
    ("editor-config", check_editorconfig),
    ("env-documentation", check_env_example),
    ("gitignore", check_gitignore),
    ("policy-files-not-ignored", check_required_files_tracked),
)


def _run_check(check, files):
    """Run one check against a synthetic fixture tree; return its messages.

    REPO_ROOT is rebound to the throwaway directory for the duration, so the
    check sees only the fixture. The binding is restored in a finally block, and
    the fixture is removed by the context manager either way.
    """
    previous_root = REPO_ROOT
    try:
        with tempfile.TemporaryDirectory() as fixture:
            for name, body in files.items():
                target = os.path.join(fixture, name.replace("/", os.sep))
                parent = os.path.dirname(target)
                if parent:
                    os.makedirs(parent, exist_ok=True)
                with open(target, "w", encoding="utf-8") as handle:
                    handle.write(body)
            globals()["REPO_ROOT"] = fixture
            result = Result()
            check(result)
    finally:
        globals()["REPO_ROOT"] = previous_root
    return result.checks[0][2]


GOOD_NUGET = """<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
    <add key="legacy" value="https://retired.example/v3/index.json" />
  </packageSources>
  <disabledPackageSources>
    <add key="legacy" value="true" />
  </disabledPackageSources>
</configuration>
"""

BAD_NUGET = GOOD_NUGET.replace(
    '  <disabledPackageSources>\n'
    '    <add key="legacy" value="true" />\n'
    '  </disabledPackageSources>\n',
    "",
).replace(
    '<add key="legacy" value="https://retired.example/v3/index.json" />',
    '<add key="rogue" value="https://rogue.example/v3/index.json" />',
)

GOOD_DOCKERIGNORE = "# comment\n.git\n**/*.snk\n**/*.pfx\n"
GOOD_DOCKERFILE = "FROM ubuntu:22.04\nRUN true\nUSER altcover\n"
GOOD_COMPOSE = (
    "services:\n"
    "  app:\n"
    "    build:\n"
    "      context: .\n"
    "      args:\n"
    "        ALTCOVER_UID: \"${ALTCOVER_UID:-1000}\"\n"
    "    security_opt:\n"
    "      - no-new-privileges:true\n"
    "    cap_drop:\n"
    "      - ALL\n"
)
GOOD_GLOBAL_JSON = '{\n  "sdk": {\n    "version": "2.1.302",\n    "rollForward": "latestFeature"\n  }\n}\n'
GOOD_EDITORCONFIG = (
    "root = true\n\n[*.{fs,fsx}]\nindent_size = 2\n\n[*.cs]\nindent_size = 4\n"
)
GOOD_ENV_EXAMPLE = "ALTCOVER_UID=1000\nALTCOVER_GID=1000\n"


def self_test() -> int:
    """Prove that each policy check can actually fail.

    A gate that cannot fail is worse than no gate: it reports green forever and
    people stop reading it. Every case below feeds a known-bad fixture to a real
    check and asserts that the check complains, using a throwaway directory in
    the system temp area. The working tree is never modified, so this is safe to
    run anywhere, including on a dirty checkout.

    Checks that depend on git (tracked key material, credential literals, and
    "policy files are not ignored") are exercised by the same code path as the
    real run and are not duplicated here: there is no hermetic way to fake
    `git ls-files` without creating a repository.
    """
    cases = (
        ("nuget: allow-listed source passes", check_nuget_sources,
         {"NuGet.config": GOOD_NUGET}, False),
        ("nuget: rogue feed is rejected", check_nuget_sources,
         {"NuGet.config": BAD_NUGET}, True),
        ("nuget: plain http feed is rejected", check_nuget_sources,
         {"NuGet.config": GOOD_NUGET.replace("https://api.nuget.org",
                                             "http://api.nuget.org")}, True),
        ("nuget: packages.config is not a source config", check_nuget_sources,
         {"NuGet.config": GOOD_NUGET,
          "some/dir/packages.config": "<packages/>"}, False),
        ("nuget: malformed XML is rejected", check_nuget_sources,
         {"NuGet.config": "<configuration><packageSources>\n<!-- a -- b -->\n"
                          "</packageSources></configuration>"}, True),
        ("docker: complete .dockerignore passes", check_docker_context,
         {".dockerignore": GOOD_DOCKERIGNORE}, False),
        ("docker: missing **/*.snk is rejected", check_docker_context,
         {".dockerignore": GOOD_DOCKERIGNORE.replace("**/*.snk\n", "")}, True),
        ("docker: missing .git is rejected", check_docker_context,
         {".dockerignore": GOOD_DOCKERIGNORE.replace(".git\n", "")}, True),
        ("docker: absent .dockerignore is rejected", check_docker_context,
         {}, True),
        ("dockerfile: unprivileged user passes", check_dockerfile,
         {"Dockerfile": GOOD_DOCKERFILE}, False),
        ("dockerfile: final USER root is rejected", check_dockerfile,
         {"Dockerfile": GOOD_DOCKERFILE.replace("USER altcover", "USER root")}, True),
        ("dockerfile: no USER at all is rejected", check_dockerfile,
         {"Dockerfile": GOOD_DOCKERFILE.replace("USER altcover\n", "")}, True),
        ("dockerfile: :latest base is rejected", check_dockerfile,
         {"Dockerfile": GOOD_DOCKERFILE.replace("ubuntu:22.04", "ubuntu:latest")}, True),
        ("compose: hardened compose passes", check_compose,
         {"docker-compose.yml": GOOD_COMPOSE}, False),
        ("compose: published host port is rejected", check_compose,
         {"docker-compose.yml": GOOD_COMPOSE + '    ports:\n      - "8080:8080"\n'},
         True),
        ("compose: missing no-new-privileges is rejected", check_compose,
         {"docker-compose.yml": GOOD_COMPOSE.replace(
             "    security_opt:\n      - no-new-privileges:true\n", "")}, True),
        ("sdk: roll-forward band passes", check_global_json,
         {"global.json": GOOD_GLOBAL_JSON}, False),
        ("sdk: exact pin without rollForward is rejected", check_global_json,
         {"global.json": GOOD_GLOBAL_JSON.replace(
             ',\n    "rollForward": "latestFeature"', "")}, True),
        ("sdk: absent global.json is rejected", check_global_json, {}, True),
        ("lint: .editorconfig covering fs and cs passes", check_editorconfig,
         {".editorconfig": GOOD_EDITORCONFIG}, False),
        ("lint: .editorconfig without cs is rejected", check_editorconfig,
         {".editorconfig": GOOD_EDITORCONFIG.replace("[*.cs]", "[*.vb]")}, True),
        ("lint: absent .editorconfig is rejected", check_editorconfig, {}, True),
        ("docs: declared compose variables pass", check_env_example,
         {"docker-compose.yml": GOOD_COMPOSE, ".env.example": GOOD_ENV_EXAMPLE},
         False),
        ("docs: undocumented compose variable is rejected", check_env_example,
         {"docker-compose.yml": GOOD_COMPOSE, ".env.example": "ALTCOVER_GID=1000\n"},
         True),
        ("docs: absent .env.example is rejected", check_env_example,
         {"docker-compose.yml": GOOD_COMPOSE}, True),
        ("xml: well-formed project files pass", check_xml_well_formed,
         {"AltCover/altcover.core.fsproj": "<Project />"}, False),
        ("xml: malformed project file is rejected", check_xml_well_formed,
         {"AltCover/altcover.core.fsproj": "<Project>"}, True),
    )

    failures = 0
    for label, check, files, expect_complaint in cases:
        messages = _run_check(check, files)
        complained = bool(messages)
        ok = complained == expect_complaint
        if not ok:
            failures += 1
        print("  %s  %s" % ("PASS" if ok else "FAIL", label))
        if not ok:
            print("          expected %s, got %s" % (
                "a complaint" if expect_complaint else "silence",
                "; ".join(messages) or "silence"))

    print("")
    if failures:
        print("verify-repo self-test: %d of %d cases FAILED" % (failures, len(cases)))
        return 1
    print("verify-repo self-test: all %d cases passed" % len(cases))
    return 0


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(
        description="Repository policy and security gate for AltCover."
    )
    parser.add_argument("--quiet", action="store_true", help="only print failures")
    parser.add_argument("--list", action="store_true", help="list checks and exit")
    parser.add_argument(
        "--self-test",
        action="store_true",
        help="prove each check can fail, using throwaway fixtures",
    )
    args = parser.parse_args(argv)

    if args.list:
        for name, _ in CHECKS:
            print(name)
        return 0

    if args.self_test:
        return self_test()

    if not args.quiet:
        print("verify-repo: checking %s\n" % REPO_ROOT)
    result = Result()
    for _, check in CHECKS:
        try:
            check(result)
        except Exception as err:  # a broken check must not mask the others
            result.record(getattr(check, "__name__", "check"), False,
                          ["check raised %s: %s" % (type(err).__name__, err)])
    return result.report(args.quiet)


if __name__ == "__main__":
    sys.exit(main())

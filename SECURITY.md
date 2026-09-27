# Security Policy

AltCover rewrites IL in assemblies you are about to execute, and it is
routinely wired into CI pipelines with elevated access to build agents, package
feeds and signing material. That makes it worth being explicit about how to
report a problem, and about the known weaknesses in this repository.

## Reporting a vulnerability

Please **do not open a public issue** for a security problem.

Report privately through GitHub's *Report a vulnerability* button on the
**Security** tab of the repository. That opens a private advisory visible only
to the maintainers.

Please include:

* the AltCover version (from the release notes or `AltCover --version`) and the
  target framework you instrumented (`net20` / `net471` / `netcoreapp2.x`);
* a minimal reproducer -- ideally a command line plus the smallest assembly or
  project that triggers it;
* what an attacker gains, not just what breaks.

We aim to acknowledge within 3 working days and to ship a fix or a mitigation
within 14 days of a confirmed report. We will credit you in the release notes
unless you ask us not to.

### Scope

In scope: the instrumenter, the recorder/shadow process, the MSBuild targets in
`nupkg/build/netstandard2.0/AltCover.targets`, the PowerShell module, and the
report parsers and writers (OpenCover, Cobertura, LCov, `ConvertToXDocument`).

Out of scope: the `Sample1`..`Sample10` and `Demo` projects. They are
deliberately odd fixtures used to exercise the instrumenter -- for example
`Demo/xunit-dotnet/UnitTest1.cs` contains a test that is *meant* to fail, and
`Sample3/Class1.cs` contains unreachable exception handling. Please report
those only if the instrumenter mishandles them, not because the sample code
itself is bad.

## Known weaknesses in this repository

### Strong-name keys are committed to the public history (high)

`Build/Infrastructure.snk`, `Build/Recorder.snk` and `Build/SelfTest.snk` are
**private** strong-name keys, tracked in git and therefore public since the
first commit that added them.

`Build/Infrastructure.snk` is not merely a test fixture: the
`PrepareFrameworkBuild` target in `Build/targets.fsx` passes it to `ILMerge` as
`/keyfile:` when producing the shipping `AltCover.exe`, and
`Build/actions.fsx` derives the `InternalsVisibleTo` grant from its public half.
The practical consequences:

* anyone can produce an assembly that strong-name-signs as AltCover, defeating
  any consumer trust decision based on the strong name;
* strong names are not a security boundary in the CLR, but they *are* routinely
  used as one in build and packaging pipelines, so this removes a control those
  pipelines believe they have;
* the key cannot simply be deleted, because it is an identity that published
  releases depend on.

**Remediation (tracked):** generate a fresh key, move it out of git into a
secret store or a CI-protected file, re-sign future releases with it, and treat
every previously published binary as unverified with respect to its strong
name. Until that happens:

* `.dockerignore` keeps `**/*.snk` and `**/*.pfx` out of any container image;
* `Build/verify-repo.py` fails the build if *new* key material is added, with an
  explicit allow-list for the three historical keys;
* the `secret-scan` CI job is configured to ignore exactly those three files
  (`.gitleaks.toml`) and nothing else.

**Do not** add a new private key, certificate or password file to this
repository. If a review asks you to, that is a finding.

### Package sources

`NuGet.config` originally trusted three MyGet `F-*` feeds in addition to
nuget.org. Those feeds were retired by their owner, so they cannot serve a
package today, and every extra feed is an unreviewed supply-chain hop -- a feed
that answers for a package name normally taken from nuget.org is the classic
dependency-confusion vector. They are now listed under
`<disabledPackageSources>`.

`Build/verify-repo.py` fails if any package source other than nuget.org is
enabled. If you genuinely need another feed, add it *disabled*, with a comment
explaining why, and update `ALLOWED_NUGET_SOURCES` in that script in the same
pull request, so the exception is deliberate and reviewable.

### Full-history secret scan

The CI `secret-scan` job scans the working tree only. A full-history scan has
not been performed, and is expected to surface credentials committed and removed
over this repository's ~1,800 commits. That work needs a rotation plan, so it is
tracked here rather than being wired in as a red/green build gate. Run it
locally against a mirror when you pick this up:

```bash
gitleaks git --redact --verbose .
```

### Second package source file

`Visualizer/nuget.config` is a second NuGet configuration and, unlike the root
one, it had *only* the retired `AvaloniaCI` feed enabled. It now also enables
nuget.org, which can only improve restore. The retired feed is still enabled
there because the visualizer pins `Avalonia 0.6.2-build5768-beta`, a CI build
that was never published to nuget.org; moving off it requires recompiling
`Visualizer/*.fs` against a published Avalonia version, which is a real change
rather than a config edit. It is covered by a single, named exception in
`NUGET_SOURCE_EXCEPTIONS` in `Build/verify-repo.py` so the exception is visible
in review rather than hidden. Removing it is a tracked follow-up, recorded in
`Visualizer/nuget.config`.

### Development container

The `Dockerfile`/`docker-compose.yml` pair is a contributor convenience, not a
production deployment:

* it runs as an unprivileged user (configure the uid/gid through `.env`, see
  `.env.example`);
* it drops all Linux capabilities and sets `no-new-privileges`;
* it publishes no host ports;
* it never receives the signing keys, because `.dockerignore` excludes them.

Do not add `privileged: true`, `cap_add`, or a `ports:` mapping to make a local
build work. If the build needs a capability, fix the build.

## Hardening guidance for CI consumers

AltCover runs on your build agent and rewrites the assemblies you then execute.
If you adopt it in a pipeline:

* pin the tool version (`AltCover` / `altcover.dotnet` / `altcover.global`
  packages) rather than tracking a floating version;
* restore only from nuget.org -- keep a copy of this repository's
  `NuGet.config` allow-list policy as a starting point;
* treat the `__Saved`/`__Instrumented` output directories as build output: the
  tool writes to the path the command line names, so a pipeline that accepts an
  untrusted `--outputDirectory` will write wherever it is told;
* run the instrumenter as an unprivileged user with no access to your signing
  service;
* if a fork or an untrusted pull request is allowed to influence the build
  definition (`Build/*.fsx`), it already has arbitrary code execution. Do not
  give that job a signing key or a publish token.

## Supported versions

Security fixes are made on the current release line and on the `master` branch.
Older lines are not patched; upgrade before reporting a problem against them.

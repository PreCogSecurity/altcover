**Expect slower/bugfix-only releases for the Summer**

# altcover
Instrumenting coverage tool for .net (framework 2.0+  and core) and Mono, reimplemented and extended almost beyond recognition from [dot-net-coverage](https://github.com/SteveGilham/dot-net-coverage), plus a set of related utilities for processing the results from this and from other programs producing similar output formats.

## Never mind the fluff -- how do I get started?

Start with the [Quick Start guide](https://github.com/SteveGilham/altcover/wiki/QuickStart-Guide)

The latest releases can be downloaded from [releases](https://github.com/SteveGilham/altcover/releases), but the easiest (and most automated) way is through the [nuget package](https://www.nuget.org/packages/AltCover).

## What's in the box?

For Mono, .net framework and .net core, except as noted

* `AltCover`, a command-line tool for recording code coverage (including dotnet and global tool versions)
* MSBuild tasks to drive the tool, including `dotnet test` integration
* A PowerShell module (not mono) containing a cmdlet that drives the tool, and other cmdlets for manipulating coverage reports
* **NEW** A coverage visualizer tool (.net framework and mono only, so far; for .net framework, needs GTK# v2.12.xx installed separately -- see https://www.mono-project.com/download/stable/#download-win )

![Visualizer screenshot](./AltCover.Visualizer/Screenshot.png)

## Why altcover?
As the name suggests, it's an alternative coverage approach.  Rather than working by hooking the .net profiling API at run-time, it works by weaving the same sort of extra IL into the assemblies of interest ahead of execution.  This means that it should work pretty much everywhere, whatever your platform, so long as the executing process has write access to the results file.  You can even mix-and-match between platforms used to instrument and those under test.

In particular, while instrumenting .net core assemblies "just works" with this approach, it also supports Mono, as long as suitable `.mdb` (or `.pdb`, in recent versions) symbols are available.  One major limitation here is that the `.mdb` format only stores the start location in the source of any code sequence point, and not the end; consequently any nicely coloured reports that take that information into account may show a bit strangely.  

### Why altcover? -- the back-story of why it was ever a thing

Back in 2010, the new .net version finally removed the deprecated profiling APIs that the free NCover 1.5.x series relied upon.  The first version of AltCover was written to both fill a gap in functionality, and to give me an excuse for a ground-up F# project to work on.  As such, it saw real production use for about a year and a half, until OpenCover reached a point where it could be used for .net4/x64 work (and I could find time to adapt everything downstream that consumed NCover format input).

Fast forwards to autumn 2017, and I get the chance to dust the project off, with the intention of saying that it worked on Mono, too -- and realise that it's _déja vu_ all over again, because .net core didn't yet have profiler based coverage tools either, and the same approach would work there as well.

### Other notes

1. On old-fashioned .net framework, the `ProcessExit` event handling window of ~2s is sufficient for processing significant bodies of code under test (several 10s of kloc, as observed in production back in the '10-'11 timeframe); under `dotnet test` the window seems to be rather tighter (about 100ms, experimentally, about enough for 1kloc).  Therefore, the preferred way to perform coverage gathering for .net core, except for the smallest programs, is to run with AltCover in the "runner" mode.  By their nature, unit tests invoking significant frameworks are not small programs, even if the system under test is itself small.

2. Under Mono on non-Windows platforms the default values of `--debug:full` or `--debug:pdbonly` generate no symbols from F# projects -- and without symbols, such assemblies cannot be instrumented.  Unlike with C# projects, where the substitution appears to be automatic, to use the necessary `--debug:portable` option involves explicitly hand editing the old-school `.fsproj` file to have `<DebugType>portable</DebugType>`.  


## Continuous Integration

| | | |
| --- | --- | --- |
| **Build** | <sup>AppVeyor</sup> [![Build status](https://img.shields.io/appveyor/ci/SteveGilham/altcover/master.svg)](https://ci.appveyor.com/project/SteveGilham/altcover) [![Test status](https://img.shields.io/appveyor/tests/SteveGilham/altcover/master.svg)](https://ci.appveyor.com/project/SteveGilham/altcover) <sup>Travis</sup> [![Build status](https://travis-ci.org/SteveGilham/altcover.svg?branch=master)](https://travis-ci.org/SteveGilham/altcover#)|
| **Unit Test coverage** | <sup>Coveralls</sup> [![Coverage Status](https://coveralls.io/repos/github/SteveGilham/altcover/badge.svg?branch=master)](https://coveralls.io/github/SteveGilham/altcover?branch=master) |
| **Nuget** | [![Nuget](https://buildstats.info/nuget/AltCover)](http://nuget.org/packages/AltCover) [![Nuget](https://img.shields.io/nuget/vpre/AltCover.svg)](http://nuget.org/packages/AltCover) |
| (.dotnet) | [![Nuget](https://buildstats.info/nuget/altcover.dotnet)](http://nuget.org/packages/altcover.dotnet) [![Nuget](https://img.shields.io/nuget/vpre/altcover.dotnet.svg)](http://nuget.org/packages/altcover.dotnet) |
| (.global) | [![Nuget](https://buildstats.info/nuget/altcover.global)](http://nuget.org/packages/altcover.global) [![Nuget](https://img.shields.io/nuget/vpre/altcover.global.svg)](http://nuget.org/packages/altcover.global) |

Coverage is uploaded to Coveralls by the FAKE `UnitTestWithAltCoverRunner`
target, not by a separate CI step: it converts the report that AltCover has just
produced with `coveralls.net.exe`. The upload is skipped when
`COVERALLS_REPO_TOKEN` is unset, so a fork's pull request does not fail on a
missing secret.

## Security

Please read [SECURITY.md](./SECURITY.md) before adopting AltCover in a pipeline.

In short: report vulnerabilities privately via the repository's *Security* tab
rather than a public issue, and note that this repository's historical
strong-name private keys (`Build/*.snk`) are already in the public git history
and must be treated as compromised. Restore from nuget.org only, and keep
signing material out of any build that runs `Build/*.fsx`.


## Usage

See the [Wiki page](https://github.com/SteveGilham/altcover/wiki/Usage) for details

## Roadmap

See the [current project](https://github.com/SteveGilham/altcover/projects/6) for details

## Building

### Tooling

#### All platforms

It is assumed that the following are available

.net core SDK 2.1.302 or later (`dotnet`) -- try https://www.microsoft.com/net/download  
PowerShell Core 6.0.2 or later (`pwsh`) -- try https://github.com/powershell/powershell

#### Windows

You will need Visual Studio VS2017 (Community Edition) v15.7.latest with F# language support (or just the associated build tools and your editor of choice).  The NUnit3 Test Runner will simplify the basic in-IDE development cycle.  Note that some of the unit tests expect that the separate build of test assemblies under Mono, full .net framework and .net core has taken place; there will be up to 20 failures when running the unit tests in Visual Studio from clean when those expected assemblies are not found.

For the .net 2.0 support, if you don't already have FSharp.Core.dll version 2.3.0.0 (usually in Reference Assemblies\Microsoft\FSharp\.NETFramework\v2.0\2.3.0.0), then you will need to install this -- the [Visual F# Tools 4.0 RTM](https://www.microsoft.com/en-us/download/details.aspx?id=48179) `FSharp_Bundle.exe` is the most convenient source. 

For GTK# support, the GTK# latest 2.12 install is expected -- try https://www.mono-project.com/download/stable/#download-win  

#### *nix

It is assumed that `mono` (version 5.12.x) and `dotnet` are on the `PATH` already, and everything is built from the command line, with your favourite editor used for coding.

### Bootstrapping

Start by setting up `dotnet fake` with `dotnet restore dotnet-fake.fsproj`
Then `dotnet fake run ./Build/setup.fsx` to do the rest of the set-up.

### Normal builds

Running `dotnet fake run ./Build/build.fsx` performs a full build/test/package process.

Use `dotnet fake run ./Build/build.fsx --target <targetname>` to run to a specific target.

The targets that matter when you are changing the instrumenter, and that CI runs
as part of the full build:

| Target | What it does |
| --- | --- |
| `BuildRelease` / `BuildDebug` | compiles `AltCover.sln` and `altcover.core.sln` |
| `Lint`, `Gendarme`, `FxCop` | static analysis. **`Lint` is currently a no-op** -- see [Code style](#code-style) |
| `JustUnitTest` | NUnit + xUnit unit tests only (fast; this is the one to use while iterating) |
| `UnitTestDotNet` | the `*.tests.core.fsproj` suites under `dotnet test` |
| `UnitTest` | coverage gate: fails if any layer reports <= 99% line coverage |
| `Pester` | the PowerShell module tests (`Build/Pester.Tests.ps1`) |
| `SelfTest` | instruments AltCover with itself and checks the result |

The fastest inner loop is:

```bash
dotnet fake run ./Build/build.fsx --target JustUnitTest
```

Note that some unit tests expect the separate build of the test assemblies under
Mono, .net framework and .net core to have already happened, so run the full
build at least once before trusting a green `JustUnitTest`.

#### Verifying the repository (no toolchain required)

`Build/verify-repo.py` is a dependency-free gate that runs on any machine with
Python 3 -- no SDK, no restore, no network:

```bash
python Build/verify-repo.py          # verbose
python Build/verify-repo.py --quiet  # summary only, for CI
python Build/verify-repo.py --list   # enumerate the checks
```

It enforces the package-source allow-list, rejects newly added private key
material and obvious credential literals, checks that every project XML file
parses, that the container build context excludes `.git` and keys, that the
image does not run as root, that `global.json` has a roll-forward band, and that
`docker-compose.yml` and `.env.example` agree. It is the first job in
`.github/workflows/ci.yml`, and the `.travis.yml` `script` runs it before the
.NET toolchain is touched, so a policy failure costs seconds rather than a
15-minute build.

It also tests itself, which is the part that makes the rest of it trustworthy:

```bash
python Build/verify-repo.py --self-test
```

That runs each check against a known-bad fixture in a throwaway directory and
fails if the check *does not* complain. A gate that cannot fail is worse than no
gate. Writing it caught three real defects in the gate itself, two of which were
latent path-handling bugs that only appeared when the script was run from
outside the repository root.

#### Code style

`.editorconfig` is the formatting baseline (2-space indent for F#, 4 for C#,
UTF-8, LF, final newline). It is honoured by Visual Studio, VS Code, Rider and
`dotnet format`, so it applies whether or not you run the linter.

`Settings.FSharpLint` holds the naming and rewrite rules and is a stricter
supplement, but the `Lint` FAKE target is currently disabled: FSharpLint cannot
parse the F# version this project compiles against (see the commented-out code
in `Build/targets.fsx` and
[FSharpLint#266](https://github.com/fsprojects/FSharpLint/issues/266)). Until
that is resolved, `.editorconfig` plus review is what actually keeps the tree
consistent.

#### If the build fails

If there's a passing build on the CI servers for this commit, then it's likely to be one of the [intermittent build failures](https://github.com/SteveGilham/altcover/wiki/Intermittent-build-issues) that can arise from the tooling used. The standard remedy is to try again.

### Unit Tests

The tests in the `Tests.fs` file are ordered in the same dependency order as the code within the AltCover project (the later `Runner` tests aside).  While working on any given layer, it would make sense to comment out all the tests for later files so as to show what is and isn't being covered by explicit testing, rather than merely being cascaded through.

### Environment

There are no environment variables required to build or test. The optional ones
are documented in [`.env.example`](./.env.example); copy it to `.env` (which is
git-ignored and excluded from the Docker build context) before using the
container. In summary:

| Variable | Used by | Purpose |
| --- | --- | --- |
| `ALTCOVER_UID` / `ALTCOVER_GID` | `docker-compose.yml`, `Dockerfile` | uid/gid the dev shell runs as; set to your own on Linux so the bind-mounted tree stays writable |
| `DOTNET_CLI_TELEMETRY_OPTOUT` | container, CI | keep SDK telemetry off shared/CI machines |
| `DOTNET_NOLOGO` | container, CI | keep build logs greppable |
| `COVERALLS_REPO_TOKEN` | `Build/targets.fsx` | enables the Coveralls upload; unset means the upload is skipped, not failed |
| `APPVEYOR_BUILD_VERSION` | `Build/targets.fsx` | stamps the assembly version in a release build; derived locally from `appveyor.yml` and git |

### Containerised development

```bash
cp .env.example .env
# on Linux, so the bind mount is writable:
printf 'ALTCOVER_UID=%s\nALTCOVER_GID=%s\n' "$(id -u)" "$(id -g)" >> .env
docker compose build
docker compose run --rm app
```

The image is a development shell, not a service: it runs as an unprivileged
user, drops all Linux capabilities, sets `no-new-privileges`, publishes no
ports, and -- via `.dockerignore` -- contains neither the git history nor the
`Build/*.snk` signing keys. It deliberately does *not* install the .NET SDK;
provision the toolchain you need on top, or use the host toolchain as the
README's "Tooling" section describes.


## Thanks to

* [AppVeyor](https://ci.appveyor.com/project/SteveGilham/altcover) for allowing free build CI services for Open Source projects
* [travis-ci](https://travis-ci.org/SteveGilham/altcover) for allowing free build CI services for Open Source projects
* [Coveralls](https://coveralls.io/r/SteveGilham/altcover) for allowing free services for Open Source projects

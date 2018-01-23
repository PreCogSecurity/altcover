﻿namespace Tests

open System
open System.IO
open System.Reflection
open System.Text

open AltCover
open AltCover.Augment
open NUnit.Framework

[<TestFixture>]
type AltCoverTests() = class

  // Augment.fs

  [<Test>]
  member self.AugmentNullableDetectNulls() =
    let input = [ "string"; null; "another string" ]
    let nulls = input |> Seq.map (Option.nullable >> Option.isNone)
    Assert.That(nulls, Is.EquivalentTo([false; true; false]))

  [<Test>]
  member self.AugmentGetOrElseFillsInNone() =
    let input = [ "string"; null; "another string" ]
    let strings = input |> Seq.map (Option.nullable >> (Option.getOrElse "fallback"))
    Assert.That(strings, Is.EquivalentTo([ "string"; "fallback"; "another string" ]))

  // CommandLine.fs

  [<Test>]
  member self.NoThrowNoErrorLeavesAllOK () =
    try
      CommandLine.error <- false
      CommandLine.doPathOperation ignore
      Assert.That(CommandLine.error, Is.False)
    finally
      CommandLine.error <- false

  [<Test>]
  member self.NoThrowWithErrorIsSignalled () =
    try
      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> CommandLine.error <- true)
      Assert.That(CommandLine.error, Is.True)
    finally
      CommandLine.error <- false

  [<Test>]
  member self.ArgumentExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "ArgumentException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> ArgumentException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + Environment.NewLine))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.IOExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "IOException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> IOException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + Environment.NewLine))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.NotSupportedExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "NotSupportedException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> NotSupportedException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + Environment.NewLine))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.SecurityExceptionWrites () =
    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr
      let unique = "SecurityException " + Guid.NewGuid().ToString()

      CommandLine.error <- false
      CommandLine.doPathOperation (fun () -> System.Security.SecurityException(unique) |> raise)
      Assert.That(CommandLine.error, Is.True)
      Assert.That(stdout.ToString(), Is.Empty)
      let result = stderr.ToString()
      Assert.That(result, Is.EqualTo (unique + Environment.NewLine))
    finally
      CommandLine.error <- false
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  // Runner.fs and CommandLine.fs

  [<Test>]
  member self.UsageIsAsExpected() =
    let options = Runner.DeclareOptions ()
    let saved = Console.Error

    try
      use stderr = new StringWriter()
      Console.SetError stderr
      CommandLine.Usage "UsageError" options
      let result = stderr.ToString().Replace("\r\n", "\n")
      let expected = """Error - usage is:
  -r, --recorderDirectory=VALUE
                             The folder containing the instrumented code to
                               monitor (including the AltCover.Recorder.g.dll
                               generated by previous a use of the .net core
                               AltCover).
  -w, --workingDirectory=VALUE
                             Optional: The working directory for the
                               application launch
  -x, --executable=VALUE     The executable to run e.g. dotnet
  -?, --help, -h             Prints out the options.
"""

      Assert.That (result, Is.EqualTo (expected.Replace("\r\n", "\n")), "*" + result + "*")

    finally Console.SetError saved

  [<Test>]
  member self.ShouldLaunchWithExpectedOutput() =
    // Hack for running while instrumented
    let where = Assembly.GetExecutingAssembly().Location
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../_Mono/Sample1")
#else
    let path' = path
#endif
    let files = Directory.GetFiles(path')
    let program = files
                  |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                  |> Seq.head

    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      CommandLine.Launch program (String.Empty) (Path.GetDirectoryName (Assembly.GetExecutingAssembly().Location))

      Assert.That(stderr.ToString(), Is.Empty)
      let result = stdout.ToString()
      // hack for Mono
      let computed = if result.Length = 14 then
                       result |> Encoding.Unicode.GetBytes |> Array.takeWhile (fun c -> c <> 0uy)|> Encoding.UTF8.GetString
                     else result

      if "TRAVIS_JOB_NUMBER" |> Environment.GetEnvironmentVariable |> String.IsNullOrWhiteSpace || result.Length > 0 then
        Assert.That(computed.Trim(), Is.EqualTo("Where is my rocket pack?"))
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.ShouldHaveExpectedOptions() =
    let options = Runner.DeclareOptions ()
    Assert.That (options.Count, Is.EqualTo 5)
    Assert.That(options |> Seq.filter (fun x -> x.Prototype <> "<>")
                        |> Seq.forall (fun x -> (String.IsNullOrWhiteSpace >> not) x.Description))
    Assert.That (options |> Seq.filter (fun x -> x.Prototype = "<>") |> Seq.length, Is.EqualTo 1)

  [<Test>]
  member self.ParsingJunkIsAnError() =
    let options = Runner.DeclareOptions ()
    let parse = CommandLine.ParseCommandLine [| "/@thisIsNotAnOption" |] options
    match parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "UsageError")
                     Assert.That (y, Is.SameAs options)

  [<Test>]
  member self.ParsingJunkAfterSeparatorIsExpected() =
    let options = Runner.DeclareOptions ()
    let input = [| "--";  "/@thisIsNotAnOption"; "this should be OK" |]
    let parse = CommandLine.ParseCommandLine input options
    match parse with
    | Left _ -> Assert.Fail()
    | Right (x, y) -> Assert.That (x, Is.EquivalentTo (input |> Seq.skip 1))
                      Assert.That (y, Is.SameAs options)

  [<Test>]
  member self.ParsingHelpGivesHelp() =
    let options = Runner.DeclareOptions ()
    let input = [| "--?" |]
    let parse = CommandLine.ParseCommandLine input options
    match parse with
    | Left _ -> Assert.Fail()
    | Right (x, y) -> Assert.That (y, Is.SameAs options)

    match CommandLine.ProcessHelpOption parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "HelpText")
                     Assert.That (y, Is.SameAs options)

    // a "not sticky" test
    lock Runner.executable (fun () ->
      Runner.executable := None
      match CommandLine.ParseCommandLine [| "/x"; "x" |] options
            |> CommandLine.ProcessHelpOption with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty))

  [<Test>]
  member self.ParsingErrorHelpGivesHelp() =
    let options = Runner.DeclareOptions ()
    let input = [| "--o"; Path.GetInvalidPathChars() |> String |]
    let parse = CommandLine.ParseCommandLine input options
    match parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "UsageError")
                     Assert.That (y, Is.SameAs options)

    match CommandLine.ProcessHelpOption parse with
    | Right _ -> Assert.Fail()
    | Left (x, y) -> Assert.That (x, Is.EqualTo "UsageError")
                     Assert.That (y, Is.SameAs options)

    // a "not sticky" test
    lock Runner.executable (fun () ->
      Runner.executable := None
      match CommandLine.ParseCommandLine [| "/x"; "x" |] options
            |> CommandLine.ProcessHelpOption with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty))

  [<Test>]
  member self.ParsingExeGivesExe() =
    lock Runner.executable (fun () ->
    try
      Runner.executable := None
      let options = Runner.DeclareOptions ()
      let unique = "some exe"
      let input = [| "-x"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match !Runner.executable with
      | None -> Assert.Fail()
      | Some x -> Assert.That(Path.GetFileName x, Is.EqualTo unique)
    finally
      Runner.executable := None)

  [<Test>]
  member self.ParsingMultipleExeGivesFailure() =
    lock Runner.executable (fun () ->
    try
      Runner.executable := None
      let options = Runner.DeclareOptions ()
      let unique = Guid.NewGuid().ToString()
      let input = [| "-x"; unique; "/x"; unique.Replace("-", "+") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.executable := None)

  [<Test>]
  member self.ParsingMoExeGivesFailure() =
    lock Runner.executable (fun () ->
    try
      Runner.executable := None
      let options = Runner.DeclareOptions ()
      let blank = " "
      let input = [| "-x"; blank; |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.executable := None)

  [<Test>]
  member self.ParsingWorkerGivesWorker() =
    try
      Runner.workingDirectory <- None
      let options = Runner.DeclareOptions ()
      let unique = Path.GetFullPath(".")
      let input = [| "-w"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match Runner.workingDirectory with
      | None -> Assert.Fail()
      | Some x -> Assert.That(x, Is.EqualTo unique)
    finally
      Runner.workingDirectory <- None

  [<Test>]
  member self.ParsingMultipleWorkerGivesFailure() =
    try
      Runner.workingDirectory <- None
      let options = Runner.DeclareOptions ()
      let input = [| "-w"; Path.GetFullPath("."); "/w"; Path.GetFullPath("..") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.workingDirectory <- None

  [<Test>]
  member self.ParsingBadWorkerGivesFailure() =
    try
      Runner.workingDirectory <- None
      let options = Runner.DeclareOptions ()
      let unique = Guid.NewGuid().ToString().Replace("-", "*")
      let input = [| "-w"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.workingDirectory <- None

  [<Test>]
  member self.ParsingNoWorkerGivesFailure() =
    try
      Runner.workingDirectory <- None
      let options = Runner.DeclareOptions ()
      let input = [| "-w" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.workingDirectory <- None

  [<Test>]
  member self.ParsingRecorderGivesRecorder() =
    try
      Runner.recordingDirectory <- None
      let options = Runner.DeclareOptions ()
      let unique = Path.GetFullPath(".")
      let input = [| "-r"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Left _ -> Assert.Fail()
      | Right (x, y) -> Assert.That (y, Is.SameAs options)
                        Assert.That (x, Is.Empty)

      match Runner.recordingDirectory with
      | None -> Assert.Fail()
      | Some x -> Assert.That(x, Is.EqualTo unique)
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  member self.ParsingMultipleRecorderGivesFailure() =
    try
      Runner.recordingDirectory <- None
      let options = Runner.DeclareOptions ()
      let input = [| "-r"; Path.GetFullPath("."); "/r"; Path.GetFullPath("..") |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  member self.ParsingBadRecorderGivesFailure() =
    try
      Runner.recordingDirectory <- None
      let options = Runner.DeclareOptions ()
      let unique = Guid.NewGuid().ToString().Replace("-", "*")
      let input = [| "-r"; unique |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  member self.ParsingNoRecorderGivesFailure() =
    try
      Runner.recordingDirectory <- None
      let options = Runner.DeclareOptions ()
      let input = [| "-r" |]
      let parse = CommandLine.ParseCommandLine input options
      match parse with
      | Right _ -> Assert.Fail()
      | Left (x, y) -> Assert.That (y, Is.SameAs options)
                       Assert.That (x, Is.EqualTo "UsageError")
    finally
      Runner.recordingDirectory <- None

  [<Test>]
  member self.ShouldProcessTrailingArguments() =
    // Hack for running while instrumented
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let path = Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "_Mono/Sample1")
#if NETCOREAPP2_0
    let path' = if Directory.Exists path then path
                else Path.Combine(where.Substring(0, where.IndexOf("_Binaries")), "../_Mono/Sample1")
#else
    let path' = path
#endif
    let files = Directory.GetFiles(path')
    let program = files
                  |> Seq.filter (fun x -> x.EndsWith(".exe", StringComparison.OrdinalIgnoreCase))
                  |> Seq.head

    let saved = (Console.Out, Console.Error)
    try
      use stdout = new StringWriter()
      use stderr = new StringWriter()
      Console.SetOut stdout
      Console.SetError stderr

      let u1 = Guid.NewGuid().ToString()
      let u2 = Guid.NewGuid().ToString()

      CommandLine.ProcessTrailingArguments [program; u1; u2]
                                     (DirectoryInfo(where))

      Assert.That(stderr.ToString(), Is.Empty)
      let result = stdout.ToString()

      // hack for Mono
      let computed = if result.Length = 50 then
                       result |> Encoding.Unicode.GetBytes |> Array.takeWhile (fun c -> c <> 0uy)|> Encoding.UTF8.GetString
                     else result
      if "TRAVIS_JOB_NUMBER" |> Environment.GetEnvironmentVariable |> String.IsNullOrWhiteSpace || result.Length > 0 then
        Assert.That(computed.Trim(), Is.EqualTo("Where is my rocket pack? " +
                                                  u1 + "*" + u2))
    finally
      Console.SetOut (fst saved)
      Console.SetError (snd saved)

  [<Test>]
  member self.ShouldNoOp() =
    let where = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    CommandLine.ProcessTrailingArguments [] (DirectoryInfo(where))
    Assert.Pass()

end
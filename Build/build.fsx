#r "../packages/FAKE.4.64.3/tools/FakeLib.dll" // include Fake lib
#I "../packages/FSharpLint.Fake.0.8.1/tools"
#r "FSharpLint.Fake.dll"
#I "../packages/ZipStorer.3.4.0/lib/net20"
#r "ZipStorer.dll"
#I "../packages/FSharp.Formatting.2.14.4/lib/net40"
#r "FSharp.Markdown.dll"
#I "../packages/YamlDotNet.4.2.3/lib/net35/"
#r "YamlDotNet.dll"
#r "System.Xml"
#r "System.Xml.Linq"

open System
open System.IO
open System.IO.Compression
open System.Reflection
open System.Xml
open System.Xml.Linq

open Fake
open Fake.AssemblyInfoFile
open Fake.Testing
open Fake.OpenCoverHelper
open Fake.ReportGeneratorHelper
open FSharp.Markdown
open FSharpLint.Fake
open YamlDotNet.RepresentationModel

let Copyright  = ref String.Empty
let Version = ref String.Empty

let OpenCoverFilter = "+[AltCove*]* -[*]Microsoft.* -[*]System.* +[*]N.*"
let AltCoverFilter= @" -s=Mono -s=\.Recorder -s=Sample -s=nunit -t=System. -t=Sample3\.Class2 "

Target "Lint" (fun _ ->
    !! "**/*.fsproj"
        |> Seq.filter (fun n -> n.IndexOf(".core.") = -1)
        |> Seq.iter (FSharpLint (fun options -> { options with FailBuildIfAnyWarnings = true }) ))

// The clean target cleans the build and deploy folders
Target "Clean" (fun _ ->
    printfn "Cleaning"
    subDirectories (directoryInfo ".")
    |> Seq.map (fun d -> [ [|d|]; subDirectories d])
    |> Seq.concat 
    |> Seq.collect id
    |> Seq.map (fun d -> [ [|d|]; subDirectories d])
    |> Seq.concat 
    |> Seq.collect id
    |> Seq.filter (fun x -> x.Name.StartsWith "_" || x.Name = "bin" || x.Name = "obj")
    |> Seq.map (fun x -> x.FullName)
    |> Seq.distinct
    // arrange so leaves get deleted first, avoiding "does not exist" warnings
    |> Seq.groupBy (fun x -> x |> Seq.filter (fun c -> c='\\' || c = '/') |> Seq.length)
    |> Seq.map (fun (n,x) -> (n, x |> Seq.sort))
    |> Seq.sortBy (fun (n,x) -> -1 * n)
    |> Seq.map (fun (n,x) -> x)
    |> Seq.concat
    |> Seq.toList
    |> DeleteDirs

    !! ((environVar "TEMP") @@ "*.tmp.dll.mdb")
    |> DeleteFiles
)

Target "SetVersion" (fun _ ->
    let now = DateTimeOffset.UtcNow
    let epoch = DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan(int64 0))
    let diff = now.Subtract(epoch)
    let fraction = diff.Subtract(TimeSpan.FromDays(float diff.Days))
    let revision= ((int fraction.TotalSeconds) / 3)
    use yaml = new FileStream("appveyor.yml", FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.SequentialScan)
    use yreader = new StreamReader(yaml)
    let ystream = new YamlStream()
    ystream.Load(yreader)
    let mapping = ystream.Documents.[0].RootNode :?> YamlMappingNode
    let version = (string mapping.Children.[YamlScalarNode("version")])
    let majmin = String.Join(".", version.Split('.') |> Seq.take 2)
    let appveyor = environVar "APPVEYOR_BUILD_VERSION"    
    Version := if String.IsNullOrWhiteSpace appveyor then sprintf "%s.%d.%d" majmin diff.Days revision else appveyor
    printfn "Build version : %s" (!Version)
    let copy = sprintf "© 2010-%d by Steve Gilham <SteveGilham@users.noreply.github.com>" now.Year
    Copyright := "Copyright " + copy

    let stream2 = new System.IO.FileStream("./Build/SelfTest.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)
    let pair2 = StrongNameKeyPair(stream2)
    let key2 = BitConverter.ToString pair2.PublicKey

    let stream = new System.IO.FileStream("./Build/Infrastructure.snk", System.IO.FileMode.Open, System.IO.FileAccess.Read)
    let pair = StrongNameKeyPair(stream)
    let key = BitConverter.ToString pair.PublicKey

    CreateFSharpAssemblyInfo "./_Generated/AssemblyVersion.fs"
        [Attribute.Version (majmin + ".0.0")
         Attribute.FileVersion (!Version)
         Attribute.Company "Steve Gilham"
         Attribute.Product "AltCover"
         Attribute.Trademark ""
         Attribute.Copyright copy
         ]

    let template ="namespace AltCover
open System.Reflection
open System.Runtime.CompilerServices
#if DEBUG
[<assembly: AssemblyConfiguration(\"Debug {0}\")>]
#else
[<assembly: AssemblyConfiguration(\"Release {0}\")>]
#endif
#if NETSTANDARD2_0
[<assembly: InternalsVisibleTo(\"AltCover.Shadow.Tests\")>]
#else
#if NETCOREAPP2_0
[<assembly: InternalsVisibleTo(\"AltCover.Tests\")>]

#else
[<assembly: InternalsVisibleTo(\"AltCover.Tests, PublicKey={1}\")>]
[<assembly: InternalsVisibleTo(\"AltCover.Tests, PublicKey={2}\")>]
[<assembly: InternalsVisibleTo(\"AltCover.Shadow.Tests, PublicKey={1}\")>]
[<assembly: InternalsVisibleTo(\"AltCover.Shadow.Tests, PublicKey={2}\")>]
[<assembly: InternalsVisibleTo(\"AltCover.Shadow.Tests2, PublicKey={1}\")>]
[<assembly: InternalsVisibleTo(\"AltCover.Shadow.Tests2, PublicKey={2}\")>]
#endif
#endif
()
"
    let file = String.Format(System.Globalization.CultureInfo.InvariantCulture,
                template, (!Version), key.Replace("-", String.Empty), key2.Replace("-", String.Empty))
    let path = @"_Generated\VisibleToTest.fs"
    // Update the file only if it would change
    let old = if File.Exists(path) then File.ReadAllText(path) else String.Empty
    if not (old.Equals(file)) then File.WriteAllText(path, file)
)

Target "BuildRelease" (fun _ ->
   [ "AltCover.sln" ]
     |> MSBuildRelease "" ""
     |> Log "AppBuild-Output: "

   ILMerge (fun p -> { p with DebugInfo = true
                              TargetKind = TargetKind.Exe
                              KeyFile = "./Build/Infrastructure.snk"
                              Version = (String.Join(".", (!Version).Split('.') |> Seq.take 2) + ".0.0")
                              Internalize = InternalizeTypes.Internalize
                              Libraries = !! "./_Binaries/AltCover/Release+AnyCPU/Mono.C*.dll"
                              AttributeFile = "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"})
                              "./_Binaries/AltCover/AltCover.exe"
                              "./_Binaries/AltCover/Release+AnyCPU/AltCover.exe"
)

Target "BuildDotNetRelease" (fun _ ->
    DotNetCli.Build
        (fun p -> 
            { p with 
                Configuration = "Release"
                Project =  "./altcover.core.sln"})

    ensureDirectory "./_Binaries/netcoreapp2.0"
    !! (@"_Binaries\AltCover\Release+AnyCPU\netcoreapp2.0\*")
    |> (Copy "./_Binaries/netcoreapp2.0")

)

Target "BuildDebug" (fun _ ->
   !! "**/AltCove*.sln"  // include demo projects
     |> Seq.filter (fun n -> n.IndexOf(".core.") = -1)
     |> MSBuildDebug "" ""
     |> Log "AppBuild-Output: "
)

Target "BuildDotNetDebug" (fun _ ->
    DotNetCli.Build
        (fun p -> 
            { p with 
                Configuration = "Debug"
                Project =  "./altcover.core.sln"})
)

Target "Test" (fun _ ->
    ensureDirectory "./_Reports"
    !! (@"_Binaries\*Tests\Debug+AnyCPU\*.Test*.dll")
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/NUnit3Report.xml"] })
)

Target "TestDotNet" (fun _ ->
    ensureDirectory "./_Reports"
    !! (@".\*Tests\*.tests.core.fsproj")
    |> Seq.iter (fun f -> printfn "Testing %s" f
                          DotNetCli.Test
                             (fun p -> 
                                  { p with 
                                      Configuration = "Debug"
                                      Project =  f}))
)
Target "TestDotNetOnMono" (fun _ ->
    ensureDirectory "./_Reports"
    DotNetCli.RunCommand id "run --project ./AltCover/altcover.core.fsproj -- -t \"System.\" -x \"./_Reports/TestDotNetOnMono.xml\" -o \"./_Mono/_DotNetInstrumented\" -i \"./_Mono/Sample1\""

    let sampleRoot = "./_Mono/_DotNetInstrumented"
    let result2 = ExecProcess (fun info -> info.FileName <- sampleRoot @@ "/Sample1.exe"
                                           info.WorkingDirectory <- sampleRoot
                                           info.Arguments <- "") (TimeSpan.FromMinutes 5.0)
    if result2 <> 0 then failwith "Instrumented .exe failed"

    let reportSigil = "dotnet"
    let simpleReport = "./_Reports/TestDotNetOnMono.xml"
    ensureDirectory ("./_Reports/_SimpleReport" + reportSigil)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_SimpleReport" + reportSigil})
        [simpleReport]

    // get recorded details from here
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                   |> Seq.toList

    let zero = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "0")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList
    let ones = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "1")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList

    if (List.length ones) + (List.length zero) <> (List.length recorded) then failwith "unexpected visits"
    let zero' = zero |> Seq.distinct |> Seq.toList

    if ["18"; "19"; "20"] <> zero' then failwith ("wrong unvisited : " + (sprintf "%A" zero'))

    let ones' = ones |> Seq.distinct |> Seq.toList
    if ["11"; "12"; "13"; "14"; "15"; "16"; "21"] <> ones' then failwith ("wrong number of visited : " + (sprintf "%A" ones'))
)

Target "TestCover" (fun _ ->
    ensureDirectory "./_Reports/_UnitTest"
    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = "."
                                 TestRunnerExePath = findToolInSubPath "nunit3-console.exe" "."
                                 Filter = "+[AltCover]* +[AltCover.Shadow]* -[*]Microsoft.* -[*]System.* -[Sample*]*"
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = RegisterType.RegisterUser
                                 Output = "_Reports/OpenCoverReport.xml" })
        "_Binaries/AltCover.Tests/Debug+AnyCPU/AltCover.Tests.dll _Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/AltCover.Shadow.Tests.dll _Binaries/AltCover.Shadow.Tests/Debug+AnyCPU/AltCover.Shadow.Tests2.dll --result=./_Reports/NUnit3ReportOpenCovered.xml"
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges ]
                                       TargetDir = "_Reports/_UnitTest"})
        ["./_Reports/OpenCoverReport.xml"]

    if not <| String.IsNullOrWhiteSpace (environVar "APPVEYOR_BUILD_NUMBER") then
            ExecProcess (fun info -> info.FileName <- findToolInSubPath "coveralls.net.exe" "."
                                     info.WorkingDirectory <- "_Reports"
                                     info.Arguments <- "--opencover OpenCoverReport.xml") (TimeSpan.FromMinutes 5.0)
            |> ignore
)

Target "FSharpTypes" ( fun _ ->
    ensureDirectory "./_Reports"
    let simpleReport = (FullName "./_Reports") @@ ( "FSharpTypes.xml")
    let binRoot = FullName "_Binaries/AltCover/Debug+AnyCPU"
    let sampleRoot = FullName "_Binaries/Sample2/Debug+AnyCPU"
    let instrumented = "__Instrumented"
    let result = ExecProcess (fun info -> info.FileName <- binRoot @@ "AltCover.exe"
                                          info.WorkingDirectory <- sampleRoot
                                          info.Arguments <- ("-t=System. -tMicrosoft. -x=" + simpleReport + " /o=./" + instrumented)) (TimeSpan.FromMinutes 5.0)
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    // Edit xml report to store new hits
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("method"))
                   |> Seq.map (fun x -> x.Attribute(XName.Get("name")).Value)
                   |> Seq.sort
                   |> Seq.toList
    let expected = "[\"Invoke\"; \"as_bar\"; \"bytes\"; \"get_MyBar\"; \"makeThing\"; \"returnBar\"; \"returnFoo\";\n \"testMakeThing\"; \"testMakeUnion\"]"
    if recorded.Length <> 9 then failwith (sprintf "Bad method list length %A" recorded)
    if (sprintf "%A" recorded) <> expected then failwith (sprintf "Bad method list %A" recorded)
    )

Target "SelfTest" (fun _ ->
    let targetDir = "_Binaries/AltCover.Tests/Debug+AnyCPU"
    let reports = FullName "./_Reports"
    let altReport = reports @@ "AltCoverage.xml"
    let keyfile = FullName "Build\SelfTest.snk"

    ensureDirectory "./_Reports/_Instrumented"
    ensureDirectory (targetDir @@ "__Instrumented")

    printfn "Self-instrument under OpenCover"
    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = targetDir
                                 TestRunnerExePath = findToolInSubPath "AltCover.exe" targetDir
                                 Filter = OpenCoverFilter
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = RegisterType.RegisterUser
                                 Output = reports @@ "OpenCoverInstrumentationReport.xml" })
        ("/sn=" + keyfile + AltCoverFilter + "-x=" + altReport)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_Instrumented"})
        ["./_Reports/OpenCoverInstrumentationReport.xml"]

    // get recorder details from here
    use coverageFile = new FileStream("./_Reports/OpenCoverInstrumentationReport.xml", FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    // Edit xml report to store new hits
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorder = coverageDocument.Descendants(XName.Get("Module"))
                   |> Seq.filter (fun el -> el.Descendants(XName.Get("ModulePath")).Nodes()
                                            |> Seq.exists (fun n -> n.ToString().EndsWith("AltCover.Recorder.dll")))
                   |> Seq.head

    printfn "Re-instrument everything"
    ensureDirectory "./_Reports/_AltReport"
    let altReport2 = reports @@ "AltSelfTestCoverage.xml"
    let result = ExecProcess (fun info -> info.FileName <- "_Binaries/AltCover.Tests/Debug+AnyCPU/__Instrumented/AltCover.exe"
                                          info.WorkingDirectory <- "_Binaries/AltCover.Tests/Debug+AnyCPU"
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=.\__ReInstrument -x=" + altReport2)) (TimeSpan.FromMinutes 5.0)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_AltReport"})
        [altReport2]

    if result <> 0 then failwithf "Re-instrument returned with a non-zero exit code"

    printfn "Unit test instrumented code"
    ensureDirectory "./_Reports"
    [ !! "_Binaries/AltCover.Tests/Debug+AnyCPU/__ReInstrument/*.Tests.dll"
      !! "_Binaries/AltCover.Tests/Debug+AnyCPU/__ReInstrument/*ple2.dll"]
    |> Seq.concat |> Seq.distinct
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/NUnit3ReportInstrumented.xml"] })
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_SelfTestReport"})
        [altReport2]

    printfn "Instrument and run the shadow tests"
    let shadowDir = "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
    ensureDirectory "./_Reports/_TotalSelfTestReport"
    let altReport3 = reports @@ "ShadowSelfTestCoverage.xml"
    let result = ExecProcess (fun info -> info.FileName <- "_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"
                                          info.WorkingDirectory <- "_Binaries/AltCover.Shadow.Tests/Debug+AnyCPU"
                                          info.Arguments <- ("/sn=" + keyfile + AltCoverFilter + @"/o=.\__Instrument -x=" + altReport3)) (TimeSpan.FromMinutes 5.0)
    !! (@"_Binaries\AltCover.Shadow.Tests\Debug+AnyCPU\__Instrument\*.Test*.dll")
    |> NUnit3 (fun p -> { p with ToolPath = findToolInSubPath "nunit3-console.exe" "."
                                 WorkingDir = "."
                                 ResultSpecs = ["./_Reports/NUnit3ReportShadow.xml"] })
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_TotalSelfTestReport"})
        [altReport2; altReport3]
    
    printfn "Unit-test instrumented code under OpenCover"
    ensureDirectory "./_Reports/_UnitTestInstrumented"
    OpenCover (fun p -> { p with ExePath = findToolInSubPath "OpenCover.Console.exe" "."
                                 WorkingDir = "."
                                 TestRunnerExePath = findToolInSubPath "nunit3-console.exe" "."
                                 Filter = OpenCoverFilter
                                 MergeByHash = true
                                 OptionalArguments = "-excludebyattribute:*ExcludeFromCodeCoverageAttribute;*ProgIdAttribute"
                                 Register = RegisterType.RegisterUser
                                 Output = "_Reports/OpenCoverReportAltCovered.xml" })
        "_Binaries/AltCover.Tests/Debug+AnyCPU/__Instrumented/AltCover.Tests.dll _Binaries/AltCover.Tests/Debug+AnyCPU/__Instrumented/Sample2.dll --result=./_Reports/NUnit3ReportAltCovered.xml"

    use coverageFile2 = new FileStream("./_Reports/OpenCoverReportAltCovered.xml", FileMode.Open, FileAccess.ReadWrite, FileShare.None, 4096, FileOptions.SequentialScan)
    let coverageDocument2 = XDocument.Load(XmlReader.Create(coverageFile2))
    let recorder2 = coverageDocument2.Descendants(XName.Get("Module"))
                    |> Seq.filter (fun el -> el.Descendants(XName.Get("ModulePath")).Nodes()
                                             |> Seq.exists (fun n -> n.ToString().EndsWith("AltCover.Recorder.g.dll")))
                    |> Seq.head // at most 1
    recorder2.SetAttributeValue(XName.Get("hash"), recorder.Attribute(XName.Get("hash")).Value)

    ["ModulePath"; "ModuleTime"; "ModuleName"]
    |> Seq.iter (fun name -> let from = recorder.Descendants(XName.Get(name)).Nodes() |> Seq.head :?> XText
                             let to' = recorder2.Descendants(XName.Get(name)).Nodes() |> Seq.head :?> XText
                             to'.Value <- from.Value)

    // Save modified xml to a file
    coverageFile2.Seek(0L, SeekOrigin.Begin) |> ignore
    coverageFile2.SetLength(int64 0) // truncate it all because the rewrite ends up one line shorter for some reason and leaves a dangling tag
    use writer = System.Xml.XmlWriter.Create(coverageFile2)
    coverageDocument2.WriteTo(writer)
    writer.Flush()
    writer.Close()
    coverageFile2.Close()

    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_UnitTestInstrumented"})
        ["./_Reports/OpenCoverReportAltCovered.xml"]
)

Target "BuildMonoSamples" (fun _ ->
    ensureDirectory "./_Mono/Sample1"
    let mcs = findToolInSubPath "mcs.exe" ".."
    let result = ExecProcess (fun info -> info.FileName <- mcs
                                          info.WorkingDirectory <- "."
                                          info.Arguments <- (@"-debug -out:./_Mono/Sample1/Sample1.exe  .\Sample1\Program.cs")) (TimeSpan.FromMinutes 5.0)
    if result <> 0 then failwith "Mono compilation failed"

    // Fix up symbol file to have the MVId emitted by the System.Reflection.Emit code
    let assembly = System.Reflection.Assembly.ReflectionOnlyLoadFrom (FullName "./_Mono/Sample1/Sample1.exe")
    let mvid = assembly.ManifestModule.ModuleVersionId.ToByteArray();
    let symbols = System.IO.File.ReadAllBytes("./_Mono/Sample1/Sample1.exe.mdb")
    mvid|> Array.iteri (fun i x -> symbols.[i+16] <- x)
    System.IO.File.WriteAllBytes("./_Mono/Sample1/Sample1.exe.mdb", symbols)

    ensureDirectory "./_Mono/Sample3"
    let mcs = findToolInSubPath "mcs.exe" ".."
    let result = ExecProcess (fun info -> info.FileName <- mcs
                                          info.WorkingDirectory <- "."
                                          info.Arguments <- (@"-target:library -debug -out:./_Mono/Sample3/Sample3.dll  .\Sample3\Class1.cs")) (TimeSpan.FromMinutes 5.0)
    if result <> 0 then failwith "Mono compilation failed"

    // Fix up symbol file to have the MVId emitted by the System.Reflection.Emit code
    let assembly = System.Reflection.Assembly.ReflectionOnlyLoadFrom (FullName "./_Mono/Sample3/Sample3.dll")
    let mvid = assembly.ManifestModule.ModuleVersionId.ToByteArray();
    let symbols = System.IO.File.ReadAllBytes("./_Mono/Sample3/Sample3.dll.mdb")
    mvid|> Array.iteri (fun i x -> symbols.[i+16] <- x)
    System.IO.File.WriteAllBytes("./_Mono/Sample3/Sample3.dll.mdb", symbols)
    
)

let SimpleInstrumentingRun (samplePath:string) (binaryPath:string) (reportSigil:string) =
    printfn "Instrument a simple executable"
    ensureDirectory "./_Reports"
    let simpleReport = (FullName "./_Reports") @@ ( "SimpleCoverage" + reportSigil + ".xml")
    let binRoot = FullName binaryPath
    let sampleRoot = FullName samplePath
    let instrumented = "__Instrumented" + reportSigil
    let result = ExecProcess (fun info -> info.FileName <- binRoot @@ "AltCover.exe"
                                          info.WorkingDirectory <- sampleRoot
                                          info.Arguments <- ("-t=System. -x=" + simpleReport + " /o=./" + instrumented)) (TimeSpan.FromMinutes 5.0)
    if result <> 0 then failwith "Simple instrumentation failed"
    let result2 = ExecProcess (fun info -> info.FileName <- sampleRoot @@ (instrumented + "/Sample1.exe")
                                           info.WorkingDirectory <- (sampleRoot @@ instrumented)
                                           info.Arguments <- "") (TimeSpan.FromMinutes 5.0)
    if result2 <> 0 then failwith "Instrumented .exe failed"

    ensureDirectory ("./_Reports/_SimpleReport" + reportSigil)
    ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                       TargetDir = "_Reports/_SimpleReport" + reportSigil})
        [simpleReport]

    // get recorded details from here
    use coverageFile = new FileStream(simpleReport, FileMode.Open, FileAccess.Read, FileShare.None, 4096, FileOptions.SequentialScan)
    let coverageDocument = XDocument.Load(XmlReader.Create(coverageFile))
    let recorded = coverageDocument.Descendants(XName.Get("seqpnt"))
                   |> Seq.toList

    let zero = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "0")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList
    let ones = recorded
               |> Seq.filter (fun x -> x.Attribute(XName.Get("visitcount")).Value = "1")
               |> Seq.map (fun x -> x.Attribute(XName.Get("line")).Value)
               |> Seq.sort
               |> Seq.toList

    if (List.length ones) + (List.length zero) <> (List.length recorded) then failwith "unexpected visits"
    let zero' = zero |> Seq.distinct |> Seq.toList

    if ["18"; "19"; "20"] <> zero' then failwith ("wrong unvisited : " + (sprintf "%A" zero'))

    let ones' = ones |> Seq.distinct |> Seq.toList
    if ["11"; "12"; "13"; "14"; "15"; "16"; "21"] <> ones' then failwith ("wrong number of visited : " + (sprintf "%A" ones'))

Target "SimpleInstrumentation" (fun _ ->
   SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" "_Binaries/AltCover/Debug+AnyCPU" String.Empty
)

Target "SimpleMonoTest" (fun _ ->
    SimpleInstrumentingRun "_Mono/Sample1" "_Binaries/AltCover/Debug+AnyCPU" ".M"
)

Target "BulkReport" (fun _ ->
    printfn "Overall coverage reporting"
    ensureDirectory "./_Reports/_BulkReport"
    !! "./_Reports/*cover*.xml"
    |> Seq.filter (fun f -> not <| f.Contains("NUnit"))
    |> Seq.toList
    |> ReportGenerator (fun p -> { p with ExePath = findToolInSubPath "ReportGenerator.exe" "."
                                          ReportTypes = [ ReportGeneratorReportType.Html; ReportGeneratorReportType.Badges ]
                                          TargetDir = "_Reports/_BulkReport"})
)

Target "FxCop" (fun _ ->
    ensureDirectory "./_Reports"
    let fxCop = combinePaths (environVar "VS150COMNTOOLS") "../../Team Tools/Static Analysis Tools/FxCop/FxCopCmd.exe"
    ["_Binaries/AltCover/Debug+AnyCPU/AltCover.exe"]
    |> FxCop (fun p -> { p with ToolPath = fxCop
                                WorkingDir = "."
                                UseGACSwitch = true
                                Verbose = false
                                ReportFileName = "_Reports/FxCopReport.xml"
                                TypeList = ["AltCover.Augment"
                                            "AltCover.Filter"
                                            "AltCover.Instrument"
                                            "AltCover.KeyStore"
                                            "AltCover.Main"
                                            "AltCover.Naming"
                                            "AltCover.ProgramDatabase"
                                            "AltCover.Report"
                                            "AltCover.Visitor"
                                            ]
                                Rules = ["-Microsoft.Design#CA1004"
                                         "-Microsoft.Design#CA1006"
                                         "-Microsoft.Design#CA1011" // maybe sometimes
                                         "-Microsoft.Design#CA1062" // null checks,  In F#!
                                         "-Microsoft.Maintainability#CA1506"
                                         "-Microsoft.Naming#CA1704"
                                         "-Microsoft.Naming#CA1707"
                                         "-Microsoft.Naming#CA1709"
                                         "-Microsoft.Naming#CA1715"
                                          ]
                                IgnoreGeneratedCode  = true})
    if fileExists "_Reports/FxCopReport.xml" then failwith "FxCop Errors were detected"

    ["_Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll"]
    |> FxCop (fun p -> { p with ToolPath = fxCop
                                WorkingDir = "."
                                UseGACSwitch = true
                                Verbose = false
                                ReportFileName = "_Reports/FxCopReport.xml"
                                TypeList = ["AltCover.Recorder.Instance"]
                                Rules = ["-Microsoft.Design#CA1004"
                                         "-Microsoft.Design#CA1006"
                                         "-Microsoft.Design#CA1011" // maybe sometimes
                                         "-Microsoft.Design#CA1062" // null checks,  In F#!
                                         "-Microsoft.Maintainability#CA1506"
                                         "-Microsoft.Naming#CA1704"
                                         "-Microsoft.Naming#CA1707"
                                         "-Microsoft.Naming#CA1709"
                                         "-Microsoft.Naming#CA1715"
                                          ]
                                IgnoreGeneratedCode  = true})
    if fileExists "_Reports/FxCopReport.xml" then failwith "FxCop Errors were detected"
    
)

Target "Gendarme" (fun _ ->
    ensureDirectory "./_Reports"

    let r = ExecProcess (fun info -> info.FileName <- (findToolInSubPath "gendarme.exe" ".\packages")
                                     info.WorkingDirectory <- "."
                                     info.Arguments <- "--severity all --confidence all --config ./Build/rules.xml --console --html ./_Reports/gendarme.html _Binaries/AltCover/Debug+AnyCPU/AltCover.exe  _Binaries/AltCover.Shadow/Debug+AnyCPU/AltCover.Shadow.dll") (TimeSpan.FromMinutes 5.0)
    if r <> 0 then failwith  "Gendarme Errors were detected"
)

Target "Package"  (fun _ ->
    ensureDirectory "./_Binaries/Packaging"
    ensureDirectory "./_Packaging"

    let packingCopyright = (!Copyright).Replace("©", "&#xa9;").Replace("<","&lt;").Replace(">", "&gt;")
    let AltCover = FullName "_Binaries/AltCover/AltCover.exe"
    let recorder = FullName "_Binaries/AltCover/Release+AnyCPU/AltCover.Recorder.dll"
    let resources = filesInDirMatchingRecursive "AltCover.resources.dll" (directoryInfo (FullName "_Binaries/AltCover/Release+AnyCPU")) 
    let readme = FullName "README.md"
    let document = File.ReadAllText readme
    let docHtml = """<?xml version="1.0"  encoding="utf-8"?>
<!DOCTYPE html>
<html lang="en">
<head>
<title>AltCover README</title>
</head>
<body>
"""               + (Markdown.TransformHtml document) + """
<footer><p style="text-align: center">""" + packingCopyright + """</p>
</footer>
</body>
</html>
"""
    let xmlform = XDocument.Parse docHtml
    let body = xmlform.Descendants(XName.Get "body")
    let eliminate = [ "Continuous Integration"; "Building"; "Thanks to" ]
    let keep = ref true

    let kill = body.Elements() 
               |> Seq.map (fun x -> match x.Name.LocalName with
                                    | "h2" -> keep := (List.tryFind (fun e -> e = String.Concat(x.Nodes())) eliminate) |> Option.isNone
                                    | "footer" -> keep := true
                                    | _ -> ()
                                    if !keep then None else Some x)
               |> Seq.toList
    kill |> 
    Seq.iter (fun q -> match q with 
                       | Some x -> x.Remove()
                       | _ -> ())

    let packable = FullName "./_Binaries/README.html"
    xmlform.Save packable

    let applicationFiles = [
                            (AltCover, Some "tools", None)
                            (recorder, Some "tools", None)
                            (packable, Some "", None)
                           ]
    let resourceFiles = resources
                        |> Seq.map (fun x -> x.FullName)
                        |> Seq.map (fun x -> (x, Some ("tools/" + Path.GetFileName(Path.GetDirectoryName(x))), None))
                        |> Seq.toList

    NuGet (fun p ->
    {p with
        Authors = ["Steve Gilham"]
        Project = "altcover"
        Description = "A pre-instrumented code coverage tool for .net and Mono"
        OutputPath = "./_Packaging"
        WorkingDir = "./_Binaries/Packaging"
        Files = List.concat [applicationFiles; resourceFiles]
        Version = !Version
        Copyright = (!Copyright).Replace("©", "(c)")
        Publish = false
        ReleaseNotes = FullName "ReleaseNotes.md"
                       |> File.ReadAllText 
        })
        "./Build/AltCover.nuspec"
)

Target "SimpleReleaseTest" (fun _ ->
   let nugget = !! "./_Packaging/*.nupkg" |> Seq.last
   // should work but doesn't ZipFile.ExtractToDirectory(nugget, "_Packaging/Unpack")
   // so do this
   let zip = ZipStorer.Open(nugget, FileAccess.Read)
   let unpack = FullName "_Packaging/Unpack"
   zip.ReadCentralDir()
    |> Seq.filter (fun entry -> let name = Path.GetFileName(entry.FilenameInZip)
                                name.StartsWith("AltCover.", StringComparison.OrdinalIgnoreCase) &&
                                    (Path.GetExtension(name).Length = 4))
    |> Seq.iter (fun entry -> zip.ExtractFile(entry, unpack @@ Path.GetFileName(entry.FilenameInZip)) |> ignore)

   SimpleInstrumentingRun "_Binaries/Sample1/Debug+AnyCPU" unpack ".R"
)

Target "SimpleMonoReleaseTest" (fun _ ->

    let unpack = FullName "_Packaging/Unpack"
    SimpleInstrumentingRun "_Mono/Sample1" unpack ".MR"
)

Target "dotnet" ignore

Target "All" ignore

"Clean"
==> "SetVersion"
==> "BuildRelease"
==> "Package"
==> "SimpleReleaseTest"
==> "SimpleMonoReleaseTest"

"Clean"
==> "SetVersion"
==> "BuildDotNetRelease"
==> "dotnet"

"BuildMonoSamples"
==> "SimpleMonoReleaseTest"

"BuildMonoSamples"
==> "Test"

"BuildMonoSamples"
==> "TestDotNet"

"SetVersion"
=?> ("BuildDebug", (not(File.Exists("./_Generated/AssemblyVersion.fs"))))

"SetVersion"
=?> ("BuildDotNetDebug", (not(File.Exists("./_Generated/AssemblyVersion.fs"))))

"SetVersion"
?=> "BuildDotNetDebug"

"SetVersion"
?=> "BuildDebug"

"Clean"
?=> "BuildDebug"

"Clean"
?=> "BuildDotNetDebug"

"BuildDebug"
==> "Lint"
"BuildDebug"
==> "Test"
"BuildDebug"
==> "TestCover"
"BuildDebug"
==> "FxCop"
"BuildDebug"
==> "SelfTest"
"BuildDebug"
==> "SimpleInstrumentation"
"BuildDebug"
==> "BuildDotNetRelease"
"BuildDebug"
==> "BuildMonoSamples"
"BuildDebug"
==> "FSharpTypes"
"BuildDebug"
==> "Gendarme"

"BuildDotNetDebug"
==> "TestDotNet"

"TestCover"
==> "BulkReport"

"SelfTest"
==> "BulkReport"

"SimpleInstrumentation"
==> "BulkReport"

"BulkReport"
==> "All"

"SimpleMonoReleaseTest"
==> "All"

"SimpleMonoTest"
==> "All"

"FSharpTypes"
==> "All"

"Lint"
==> "All"

"FxCop"
==> "All"

"Gendarme"
==> "All"

"dotnet"
==> "All"

"Lint"
?=> "FxCop"

"FxCop"
?=> "Gendarme"

"Test"
==> "TestCover"

"TestDotNet"
==> "TestDotNetOnMono"

"TestDotNetOnMono"
==> "dotnet"


RunTargetOrDefault "All"
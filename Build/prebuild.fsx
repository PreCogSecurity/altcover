open System
open System.IO

let here = Directory.GetCurrentDirectory()
let fakelib = Directory.GetFiles(here, "*akeLib.dll", SearchOption.AllDirectories)
              |> Seq.head
              |> Path.GetDirectoryName

let lintlib = Directory.GetFiles(here, "*SharpLint.Fake.dll", SearchOption.AllDirectories)
              |> Seq.head
              |> Path.GetDirectoryName

let mdlib = Directory.GetFiles(here, "*Sharp.Markdown.dll", SearchOption.AllDirectories)
            |> Seq.filter (fun n -> n.Contains("net40"))
            |> Seq.head
            |> Path.GetDirectoryName

let ylib = Directory.GetFiles(here, "*amlDotNet.dll", SearchOption.AllDirectories)
            |> Seq.filter (fun n -> n.Contains("net35"))
            |> Seq.head
            |> Path.GetDirectoryName

let nlib = Directory.GetFiles(here, "*framework.dll", SearchOption.AllDirectories)
            |> Seq.filter (fun n -> n.Contains("net45"))
            |> Seq.head
            |> Path.GetDirectoryName

let build = """#r "paket:
nuget Fake.Core.Target prerelease //"
#r "paket:
nuget FSharpLint.Core prerelease //"
#r "FSharpLint.Core.dll"
//#I @"{0}" // include Fake lib
//#r "FakeLib.dll"
//#I @"{1}"
//#r "FSharpLint.Fake.dll"
#I @"{2}"
#r "FSharp.Markdown.dll"
#I @"{3}"
#r "YamlDotNet.dll"
#I @"{4}"
#r "nunit.framework.dll"
#r "System.IO.Compression.FileSystem.dll"
#r "System.Xml"
#r "System.Xml.Linq"

#load "actions.fsx"
#load "targets.fsx"
"""

let formatted = String.Format(build, fakelib, lintlib, mdlib, ylib, nlib)
File.WriteAllText("./Build/build.fsx", formatted)
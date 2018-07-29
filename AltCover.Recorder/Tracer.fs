namespace AltCover.Recorder

open System.Collections.Generic
open System.IO
open System.IO.Compression

type Tracer = {
                Tracer : string
                Runner : bool
                Definitive : bool
                Stream : System.IO.Stream
                Formatter : System.IO.BinaryWriter
              }
  with
#if NETSTANDARD2_0
    static member Core () =
             typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>.Assembly.Location
#endif

    static member Create (name:string) =
      printfn "Create tracer %s" name
      {
       Tracer = name
       Runner = false
       Definitive = false
       Stream = null
       Formatter = null
      }

    member this.IsConnected () =
      match this.Stream with
      | null -> false
      | _ -> this.Runner

    member this.Connect () =
      if File.Exists this.Tracer then
        Seq.initInfinite (fun i -> Path.ChangeExtension(this.Tracer,
                                                        sprintf ".%d.acv" i))
        |> Seq.filter (File.Exists >> not)
        |> Seq.map (fun f -> let fs = File.OpenWrite f
                             let s = new BufferedStream(new DeflateStream(fs, CompressionMode.Compress))
                             printfn "Connected %s" f
                             { this with Stream = s
                                         Formatter = new BinaryWriter(s)
                                         Runner = true })
        |> Seq.head
      else
        printfn "No tracer file; no connection"
        this

    member this.Close() =
      this.Formatter.Close()

    member internal this.Push (moduleId:string) (hitPointId:int) context =
      this.Formatter.Write moduleId
      this.Formatter.Write hitPointId
      match context with
      | Null -> this.Formatter.Write(Tag.Null |> byte)
      | Time t  -> this.Formatter.Write(Tag.Time |> byte)
                   this.Formatter.Write(t)
      | Call t -> this.Formatter.Write(Tag.Call |> byte)
                  this.Formatter.Write(t)
      | Both (t', t) -> this.Formatter.Write(Tag.Both |> byte)
                        this.Formatter.Write(t')
                        this.Formatter.Write(t)

    member internal this.CatchUp (visits:Dictionary<string, Dictionary<int, int * Track list>>) =
      printfn "Catching up..."
      let empty = Null
      visits.Keys
      |> Seq.iter (fun moduleId ->
        visits.[moduleId].Keys
        |> Seq.iter (fun hitPointId -> let n, l = visits.[moduleId].[hitPointId]
                                       let push = this.Push moduleId hitPointId
                                       [seq {1 .. n} |> Seq.map (fun _ -> empty )
                                        l |> List.toSeq]
                                       |> Seq.concat |> Seq.iter push
                                       ))
      visits.Clear()
      printfn "Caught up"

    member this.OnStart () =
      let running = if this.Tracer <> "Coverage.Default.xml.acv" then
                       this.Connect () else this
      printfn "Definitve is %A" running
      {running with Definitive = true}

    member this.OnConnected f g =
      if this.IsConnected() then f()
      else g ()

    member internal this.OnFinish visits =
      printfn "OnFinish at %A" System.DateTime.UtcNow
      this.CatchUp visits
      this.Push System.String.Empty -1 Null
      this.Stream.Flush()
      this.Close()
      printfn "OnFinish done at %A" System.DateTime.UtcNow

    member internal this.OnVisit visits moduleId hitPointId context =
      this.CatchUp visits
      this.Push moduleId hitPointId context
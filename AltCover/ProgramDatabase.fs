﻿namespace AltCover

open System
open System.IO

open AltCover.Augment

open Mono.Cecil
open Mono.Cecil.Cil
open Mono.Cecil.Mdb
open Mono.Cecil.Pdb

module ProgramDatabase =
  // We no longer have to violate Cecil encapsulation to get the PDB path!
  let GetPdbFromImage (assembly:AssemblyDefinition) =
    if assembly.MainModule.HasDebugHeader then
      let header = assembly.MainModule.GetDebugHeader()
      if header.HasEntries then
          let entry = header.Entries // header is followed by UTF-8 nul terminated string
                      |> Seq.head
          if entry.Data.Length > 0x18 then 
              let name = entry.Data
                              |> Seq.skip 0x18  // size of the debug header
                              |> Seq.takeWhile (fun x -> x <> byte 0)
                              |> Seq.toArray
              Some (System.Text.Encoding.UTF8.GetString name)
              |> Option.filter (fun (s:String) -> s.Length > 0)
              |> Option.filter File.Exists
          else 
              printfn "%d entries" header.Entries.Length
              printfn "%A" entry.Data
              None
      else
        None // TODO -- built-in mdb path for Mono
    else
      None

  let GetPdbWithFallback (assembly:AssemblyDefinition) =
    match GetPdbFromImage assembly with
    | None -> let fallback = Path.ChangeExtension(assembly.MainModule.FileName, ".pdb")
              if File.Exists(fallback)
                then Some fallback
                else let fallback2 = assembly.MainModule.FileName + ".mdb"
                     // Note -- the assembly path, not the mdb path, because GetSymbolReader wants the assembly path for Mono
                     if File.Exists(fallback2) then Some assembly.MainModule.FileName else None
    | pdbpath -> pdbpath

  // Ensure that we read symbols from the .pdb path we discovered.
  // Cecil currently only does the Path.ChangeExtension(path, ".pdb") fallback if left to its own devices
  // Will fail  with InvalidOperationException if there is a malformed file with the expected name
  let ReadSymbols (assembly:AssemblyDefinition) =
    GetPdbWithFallback assembly
    |> Option.map (fun pdbpath ->
                        let provider : ISymbolReaderProvider = if pdbpath.EndsWith(".pdb", StringComparison.OrdinalIgnoreCase) then
                                                                   PdbReaderProvider() :> ISymbolReaderProvider
                                                               else MdbReaderProvider() :> ISymbolReaderProvider

                        let reader = provider.GetSymbolReader(assembly.MainModule, pdbpath)
                        assembly.MainModule.ReadSymbols(reader)
                        reader)
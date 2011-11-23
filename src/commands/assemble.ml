(*
 * This file is part of Barista.
 * Copyright (C) 2007-2011 Xavier Clerc.
 *
 * Barista is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Barista is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


open Utils

let names = [ "assemble"; "asm" ]

let description = "compiles to a class file"

let latex_description =
  "Assembles (\\ie compiles) the passed assembler files into \\java{} " ^
  "bytecode class files."

type parameters = {
    mutable target : Version.t;
    mutable compute_stacks : bool;
    mutable optimize : bool;
    mutable class_path : ClassPath.t;
    mutable sources : string list;
    mutable destination : string;
  }

let versions =
  List.map
    (fun x -> Version.to_string x, x)
    Version.all

let make_parameters () =
  let parameters = {
    target = Version.default;
    compute_stacks = false;
    optimize = false;
    class_path = ClassPath.make ();
    sources = [];
    destination = ".";
  } in
  let args = Command.build_args "assemble" [
    ("-cp",
     Arg.String
       (fun s ->
         let class_path = ClassPath.append s parameters.class_path in
         parameters.class_path <- class_path),
     "<path>  Add to classpath");
    ("-classpath",
     Arg.String
       (fun s ->
         parameters.class_path <- ClassPath.make_of_string s),
     "<colon-separated-list>  Set classpath");
    ("-compute-stacks",
     Arg.Unit (fun () -> parameters.compute_stacks <- true),
     " Computes stack elements (max_locals, max_stacks, and frames)");
    ("-d",
     Arg.String (fun s -> parameters.destination <- s),
     "<path>  Output path for generated class files");
    ("-destination",
     Arg.String (fun s -> parameters.destination <- s),
     "<path>  Output path for generated class files");
    ("-optimize",
     Arg.Unit (fun () -> parameters.optimize <- true),
     " Optimize bytecode");
    ("-target",
     Arg.Symbol
       (List.map fst versions,
        (fun s -> parameters.target <- List.assoc s versions)),
     " Target version for generated class files") ] in
  parameters,
  args,
  (fun s -> parameters.sources <- s :: parameters.sources)

let run params =
  let sources = List.rev params.sources in
  let class_loader = ClassLoader.make params.class_path in
  let to_string x =
    try
      UTF8.to_string (Name.external_utf8_for_class x)
    with _ -> "<unrepresentable class name>" in
  List.iter
    (fun source ->
      let source = InputStream.make_of_channel (open_in source) in
      let name =
        Assembler.assemble
          ~version:params.target
          ~compute_stacks:params.compute_stacks
          ~optimize:params.optimize
          ~class_loader:class_loader
          source
          (Assembler.Path params.destination) in
      Printf.printf "%S has been compiled\n" (to_string name);
      InputStream.close_noerr source)
    sources;
  ClassPath.close params.class_path

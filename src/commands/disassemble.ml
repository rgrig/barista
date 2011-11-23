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
open Command

let names = [ "disassemble"; "dasm" ]

let description = "decompiles to a source file"

let latex_description =
  "Disassembles (\\ie decompiles) the passed \\java{} bytecode class " ^
  "files into assembler files."

type parameters = base_parameters

let make_parameters () = make_base_parameters "disassemble"

let run params =
  let classes = List.rev params.elements in
  let print_header = make_header_printer (List.length classes) in
  List.iter
    (fun clas ->
      print_header clas;
      let clas = UTF8.of_string clas in
      Disassembler.disassemble params.class_path clas)
    classes;
  ClassPath.close params.class_path

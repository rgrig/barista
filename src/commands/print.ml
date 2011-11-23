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

let names = [ "print" ]

let description = "prints the contents of a class"

let latex_description =
  "Prints the contents of the passed \\java{} bytecode class files " ^
  "onto the standard output."

type parameters = base_parameters

let make_parameters () = make_base_parameters "print"

let run params =
  let classes = List.rev params.elements in
  let print_header = make_header_printer (List.length classes) in
  List.iter
    (fun clas ->
      print_header clas;
      let clas = UTF8.of_string clas in
      ClassPrinter.print params.class_path clas)
    classes;
  ClassPath.close params.class_path

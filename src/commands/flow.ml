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

let names = [ "flow" ]

let description = "prints the control flow of a method"

let latex_description =
  "Prints onto the standard output the control flow of the passed methods. " ^
  "The output format is the dot format\\footnote{\\url{http://www.graphviz" ^
  ".org/doc/info/lang.html}}. The passed methods should follow the format " ^
  "for method signatures as presented in chapter " ^
  "\\ref{ch:programming-assembler-language}. As an example, printing the " ^
  "control flow for the \\texttt{toString} method is done by the following " ^
  "command line:" ^
  "\\begin{center}" ^
  "  \\texttt{barista flow 'java.lang.Object.toString():java.lang.String'}" ^
  "\\end{center}" ^
  "It is important to enclose the signature of the method inside quotes, " ^
  "as otherwise the shell whould interpret the parentheses."

type parameters = base_parameters

let make_parameters () = make_base_parameters "flow"

let run params =
  let methods = List.rev params.elements in
  let print_header = make_header_printer (List.length methods) in
  let class_loader = ClassLoader.make params.class_path in
  List.iter
    (fun clas ->
      print_header clas;
      let clas = UTF8.of_string clas in
      FlowPrinter.print class_loader clas)
    methods;
  ClassPath.close params.class_path

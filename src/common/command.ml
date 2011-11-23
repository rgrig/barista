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


module type T = sig

  val names : string list

  val description : string

  val latex_description : string

  type parameters

  val make_parameters : unit -> parameters * (Arg.key * Arg.spec * Arg.doc) list * Arg.anon_fun

  val run : parameters -> unit

end

let build_args command args =
  let args = ref args in
  let usage_msg = Printf.sprintf "options for command %S:" command in
  let help () = Arg.usage !args usage_msg in
  args := !args
    @ [ ("-help",
         Arg.Unit help,
         " Display options for the command");
        ("--help",
         Arg.Unit help,
         " Display options for the command") ];
  !args

type base_parameters = {
    mutable class_path : ClassPath.t;
    mutable elements : string list;
  }

let make_base_parameters command =
  let parameters = {
    class_path = ClassPath.make ();
    elements = [];
  } in
  let args = build_args command [
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
     "<colon-separated-list>  Set classpath")
  ] in
  parameters,
  args,
  (fun s -> parameters.elements <- s :: parameters.elements)

let make_empty_parameters command =
  (),
  build_args command [],
  (fun s ->
    let msg = Printf.sprintf "invalid parameter %S" s in
    raise (Arg.Bad msg))

let make_header_printer n =
  if n > 1 then
    (fun s ->
      let buf = Buffer.create 80 in
      Buffer.add_string buf "--- ";
      Buffer.add_string buf s;
      Buffer.add_char buf ' ';
      while Buffer.length buf < 79 do
        Buffer.add_char buf '-';
      done;
      print_endline (Buffer.contents buf))
    else
      (fun _ -> ())

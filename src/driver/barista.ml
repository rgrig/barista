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

open BaristaLibrary
open Utils


let print_help () =
  Printf.eprintf "Usage: %s <command> <command parameters...>\n"
    (Filename.basename Sys.argv.(0));
  List.iter
    (fun command ->
      let module Cmd = (val command : Command.T) in
      let names = List.map (Printf.sprintf "%S") Cmd.names in
      let _, args, _ = Cmd.make_parameters () in
      Printf.eprintf "\ncommand %s (%s):\n" (String.concat " / " names) Cmd.description;
      List.iter
        (fun (key, _, doc) ->
          Printf.eprintf "  %s %s\n" key doc)
        (Arg.align args);
      ())
    Predef.commands

let main () =
  if Array.length Sys.argv > 1 then begin
    match Sys.argv.(1) with
    | "help" | "-help" | "--help" ->
        print_help ()
    | command ->
        let is_prefix ?(min_len = 3) s1 s2 =
          let len1 = String.length s1 in
          let len2 = String.length s2 in
          (len1 >= min_len) && (len1 <= len2) && (s1 = String.sub s2 0 len1) in
        let commands =
          List.map
            (fun command ->
              let module Cmd = (val command : Command.T) in
              List.map (fun x -> x, command) Cmd.names)
            Predef.commands in
        let commands = List.flatten commands in
        let commands =
          List.filter
            (fun (name, _) -> is_prefix command name)
            commands in
        (match commands with
        | [] ->
            Printf.eprintf "*** no command matches %S\n\n" command;
            print_help ();
            exit 1
        | [name, cmd] ->
            let module Cmd = (val cmd : Command.T) in
            let params, args, func = Cmd.make_parameters () in
            Arg.current := 1;
            Arg.parse args func (Printf.sprintf "command %S:" name);
            Cmd.run params
        | _ ->
            Printf.eprintf "*** several commands match %S\n\n" command;
            print_help ();
            exit 1)
  end

let () =
  try
    main ();
    exit 0
  with
  | Assembler.Exception (n, e) ->
      Printf.eprintf "*** assembler error line %d: %s\n" n (Assembler.string_of_error e);
      exit 1
  | FlowPrinter.Exception e ->
      Printf.eprintf "*** flow printer error: %s\n" (FlowPrinter.string_of_error e);
  | ClassPath.Exception e ->
      Printf.eprintf "*** class path error: %s\n" (ClassPath.string_of_error e);
      exit 1
  | ClassLoader.Exception e ->
      Printf.eprintf "*** class loader error: %s\n" (ClassLoader.string_of_error e);
      exit 1
  | Sys_error se ->
      Printf.eprintf "*** system error: %s\n" se;
      exit 1
  | e ->
      Printf.eprintf "*** error: %s\n" (Printexc.to_string e);
      exit 1

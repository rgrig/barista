(*
 * This file is part of Barista.
 * Copyright (C) 2007-2011 Xavier Clerc.
 *
 * Barista is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Barista is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ocamlbuild_plugin

let odocl_file = Pathname.pwd / "barista.odocl"
let mlpack_file = Pathname.pwd / "baristaLibrary.mlpack"
let src_path = Pathname.pwd / "src"
let lib_dirs = ["analysis"; "classfile"; "common"; "helpers"; "utf8"]
let excluded_modules = ref []

let () =
  let odocl_chan = open_out odocl_file in
  let mlpack_chan = open_out mlpack_file in
  let add_file filename =
    let ok_extensions = ["ml"; "mly"; "mll"] in
    if List.exists (Pathname.check_extension filename) ok_extensions then begin
          let modulename = Pathname.remove_extension filename in
          let modulename = Pathname.basename modulename in
          let modulename = String.capitalize modulename in
          if not (List.mem modulename !excluded_modules) then begin
            excluded_modules := modulename :: !excluded_modules;
            output_string odocl_chan modulename;
            output_char odocl_chan '\n';
            output_string mlpack_chan modulename;
            output_char mlpack_chan '\n'
          end
      end in
  List.iter
    (fun path ->
      let path = src_path / path in
      if Pathname.is_directory path then
        Array.iter add_file (Pathname.readdir path)
      else
        add_file path)
    lib_dirs;
  close_out_noerr odocl_chan;
  close_out_noerr mlpack_chan

let version_tag = "src_common_currentVersion_ml"
let version_ml = "src/common/currentVersion.ml"
let version_file = "../VERSION"
let predef_ml = "src/driver/predef.ml"
let commands_path = "../src/commands"

let () =
  let safe_cp src dst =
    let src = Pathname.mk src in
    let dst = Pathname.mk dst in
    let dir = Pathname.dirname dst in
    let cmd = Printf.sprintf "mkdir -p %s" (Pathname.to_string dir) in
    if Sys.command cmd <> 0 then failwith ("cannot run " ^ cmd);
    cp src dst in
  dispatch begin function
    | After_rules ->
        flag ["ocaml"; "compile"; "warnings"] (S[A"-w"; A"Ae"; A"-warn-error"; A"A"]);
        flag ["ocaml"; "doc"] (A"-sort");
        List.iter
          (fun lib ->
            let use_lib = "use_" ^ lib in
            let path = "+" ^ lib in
            let libname native lib = lib ^ (if native then ".cmxa" else ".cma") in
            flag ["ocaml"; "compile"; use_lib] (S[A"-I"; P path]);
            flag ["ocaml"; "link"; "byte"; use_lib] (S[A"-I"; P path; A(libname false lib)]);
            flag ["ocaml"; "link"; "native"; use_lib] (S[A"-I"; P path; A(libname true lib)]))
          ["zip"; "camomile"];

        dep [version_tag] [version_ml];
        rule ("generation of " ^ version_ml)
          ~prod:version_ml
          ~insert:`bottom
          (fun _ _ ->
            let version =
              try
                List.hd (string_list_of_file (Pathname.mk version_file))
              with _ -> "unknown" in
            let name, channel = Filename.open_temp_file "version" ".ml" in
            Printf.fprintf channel "let value = %S\n" version;
            close_out_noerr channel;
            safe_cp name version_ml);

        dep ["src_driver_predef_ml"] [predef_ml];
        rule ("generation of " ^ predef_ml)
          ~prod:predef_ml
          ~insert:`bottom
          (fun _ _ ->
            let print_module_list channel path =
              let filenames = Sys.readdir path in
              Array.sort Pervasives.compare filenames;
              Array.iter
                (fun filename ->
                  if Filename.check_suffix filename ".mli" then begin
                    let basename = Filename.basename filename in
                    let module_name = String.capitalize (Filename.chop_suffix basename ".mli") in
                    Printf.fprintf channel "  (module %s : Command.T);\n" module_name;
                  end)
                filenames in
            let name, channel = Filename.open_temp_file "predef" ".ml" in
            Printf.fprintf channel "let commands = [\n";
            print_module_list channel commands_path;
            Printf.fprintf channel "]\n";
            close_out_noerr channel;
            safe_cp name predef_ml)
    | _ -> ()
  end

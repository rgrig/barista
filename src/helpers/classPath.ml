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

(* Types *)

type entry =
  | Directory of string
  | Archive of Zip.in_file

type t = entry list


(* Exception *)

type error =
  | Unable_to_open_archive of string
  | Does_not_exist of string
  | Class_not_found of string

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Unable_to_open_archive s ->
      Printf.sprintf "unable to open archive %S" s
  | Does_not_exist s ->
      Printf.sprintf "%S does not exist" s
  | Class_not_found s ->
      Printf.sprintf "class %S not found" s

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Constructor *)

let colon = Str.regexp (Str.quote ":")

let dot = Str.regexp (Str.quote ".")

let make_entry s =
  try
    if Sys.is_directory s then
      let last = (String.length s) - 1 in
      Directory (if s.[last] = '/' then s else s ^ "/")
    else if Sys.file_exists s then
      (try
        Archive (Zip.open_in s)
      with _ -> fail (Unable_to_open_archive s))
    else
      fail (Does_not_exist s)
  with Sys_error _ -> fail (Does_not_exist s)

let map_elements l =
  List.map make_entry l

let make_of_string ?(separator=":") s =
  let sep =
    if separator = ":" then
      colon
    else
      Str.regexp (Str.quote separator) in
  let l = Str.split sep s in
  map_elements l

let make_of_list l =
  map_elements l

let make () =
  let s = try Sys.getenv "CLASSPATH" with Not_found -> "." in
  make_of_string s

let append ?(separator=":") s l =
  let l' = make_of_string ~separator s in
  l @ l'

let prepend ?(separator=":") s l =
  let l' = make_of_string ~separator s in
  l' @ l


(* Functions *)

let rec lookup cp s =
  match cp with
  | (Directory d) :: tl ->
      (try
        InputStream.make_of_channel (open_in (d ^ s))
      with _ -> lookup tl s)
  | (Archive z) :: tl ->
      (try
        let entry = Zip.find_entry z s in
        let data = Zip.read_entry z entry in
        InputStream.make_of_string data
      with _ -> lookup tl s)
  | [] -> fail (Class_not_found s)

let search_stream cp s =
  let rec search () =
    try
      lookup cp s
    with _ ->
      try
        s.[String.rindex s '/'] <- '$';
        search ()
      with _ -> fail (Class_not_found s) in
  search ()

let open_stream cp s =
  let s' = (Str.global_replace dot "/" s) ^ ".class" in
  try
    search_stream cp s'
  with _ -> fail (Class_not_found s)

let close cp =
  let close_entry = function
    | Directory _ -> ()
    | Archive z -> try Zip.close_in z with _ -> () in
  List.iter close_entry cp

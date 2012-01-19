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

type t = CamomileLibrary.UChar.t

BARISTA_ERROR =
  | Unrepresentable_character of (x : t) ->
      Printf.sprintf "unrepresentable character (code '%d')" (CamomileLibrary.UChar.uint_code x)
  | Invalid_character_code of (x : int) ->
      Printf.sprintf "invalid character code '%d'" x

let of_char = CamomileLibrary.UChar.of_char

let to_char x =
  try
    CamomileLibrary.UChar.char_of x
  with CamomileLibrary.UChar.Out_of_range -> fail (Unrepresentable_character x)

let to_char_noerr x =
  try
    CamomileLibrary.UChar.char_of x
  with _ -> '?'

let of_code x =
  try
    CamomileLibrary.UChar.chr x
  with Invalid_argument _ -> fail (Invalid_character_code x)

let to_code x =
  try
    CamomileLibrary.UChar.code x
  with CamomileLibrary.UChar.Out_of_range -> fail (Unrepresentable_character x)

let equal = CamomileLibrary.UChar.eq

let compare = CamomileLibrary.UChar.compare

module CharInfo = CamomileLibrary.UCharInfo.Make (CamomileLibraryDefault.Config)

let is_letter ch =
  match CharInfo.general_category ch with
  | `Lu | `Ll | `Lt | `Lo | `Lm -> true
  | _ -> false

let is_digit ch =
  match CharInfo.general_category ch with
  | `Nd | `Nl | `No -> true
  | _ -> false

let underscore = of_char '_'

let is_letter_or_digit ch =
  equal underscore ch
  || is_letter ch
  || is_digit ch

external to_camomile : t -> CamomileLibrary.UChar.t = "%identity"

external of_camomile : CamomileLibrary.UChar.t -> t = "%identity"

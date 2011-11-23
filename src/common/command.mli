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

(** Definition of commands to be launched from command-line. *)


module type T = sig

  val names : string list
  (** The various names for the command. *)

  val description : string
  (** The short description used for command-line help. *)

  val latex_description : string
  (** The LaTeX-formmatted description used for the manual. *)

  type parameters
  (** The type of parameters to be passed to the command. *)

  val make_parameters : unit -> parameters * (Arg.key * Arg.spec * Arg.doc) list * Arg.anon_fun
  (** Build a [params, specs, func] triplet where:
      - [params] are the parameters passed to the [run] function;
      - [specs] are the command-line specifiers passed to
        [Arg.parse_argv];
      - [func] is the anonymous function passed to [Arg.parse_argv].

      [specs] and [func] are supposed to update [params] when passed to
      [Arg.parse_argv]. *)

  val run : parameters -> unit
  (** Actually executes of the command with passed parameters. *)

end
(** The type of commands with names, parameters, and implementation. *)

val build_args : string -> (Arg.key * Arg.spec * Arg.doc) list -> (Arg.key * Arg.spec * Arg.doc) list
(** [build_args cmd args] returns [args] with overloaded ["-help"] and
    ["--help"] switches for command [cmd]. *)

type base_parameters = {
    mutable class_path : ClassPath.t; (** Class path to process elements. *)
    mutable elements : string list; (** Elements to process. *)
  }
(** The type of basic command parameters. *)

val make_base_parameters : string -> base_parameters * (Arg.key * Arg.spec * Arg.doc) list * Arg.anon_fun
(** Returns the elements needed to process [base_parameters]; class path
    is handled through ["-cp"] and ["-classpath"], while elements are the
    other arguments provided on the command-line. The passed string is
    the name of the command. *)

val make_empty_parameters : string -> unit * (Arg.key * Arg.spec * Arg.doc) list * Arg.anon_fun
(** Returns the elements needed accept no parameter.
    The passed string is the name of the command. *)

val make_header_printer : int -> (string -> unit)
(** [make_header_printer n] returns a function to print headers on the
    standard output. Nothing will be printed by the returned function
    if [n <= 1], otherwise the passed string will be printed enclosed
    with dashes. *)

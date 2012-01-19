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

(** Java- and UTF8-related constants used throughout the Barista project. *)


(** {6 Java constants} *)

val magic_number : int64


(** {6 String constants} *)

(** {7 Attribute-related strings} *)

val attr_annotation_default : Utils.UTF8.t

val attr_bootstrap_methods : Utils.UTF8.t

val attr_code : Utils.UTF8.t

val attr_constant_value : Utils.UTF8.t

val attr_deprecated : Utils.UTF8.t

val attr_enclosing_method : Utils.UTF8.t

val attr_exceptions : Utils.UTF8.t

val attr_inner_classes : Utils.UTF8.t

val attr_line_number_table : Utils.UTF8.t

val attr_local_variable_table : Utils.UTF8.t

val attr_local_variable_type_table : Utils.UTF8.t

val attr_module : Utils.UTF8.t

val attr_module_requires : Utils.UTF8.t

val attr_module_permits : Utils.UTF8.t

val attr_module_provides : Utils.UTF8.t

val attr_runtime_invisible_annotations : Utils.UTF8.t

val attr_runtime_invisible_parameter_annotations : Utils.UTF8.t

val attr_runtime_invisible_type_annotations : Utils.UTF8.t

val attr_runtime_visible_annotations : Utils.UTF8.t

val attr_runtime_visible_parameter_annotations : Utils.UTF8.t

val attr_runtime_visible_type_annotations : Utils.UTF8.t

val attr_signature : Utils.UTF8.t

val attr_source_debug_extension : Utils.UTF8.t

val attr_source_file : Utils.UTF8.t

val attr_stack_map_table : Utils.UTF8.t

val attr_synthetic : Utils.UTF8.t

val attr_unknown : Utils.UTF8.t

val attr_ignored : Utils.UTF8.t

val attr_all : Utils.UTF8.t list

(** {7 Miscellaneous strings} *)

val class_constructor : Utils.UTF8.t

val class_initializer : Utils.UTF8.t

val empty_utf8 : Utils.UTF8.t

val java_lang_Object : Utils.UTF8.t


(** {6 Character constants} *)

(** {7 Capital letters} *)

val capital_b : Utils.UChar.t

val capital_c : Utils.UChar.t

val capital_d : Utils.UChar.t

val capital_f : Utils.UChar.t

val capital_i : Utils.UChar.t

val capital_j : Utils.UChar.t

val capital_l : Utils.UChar.t

val capital_s : Utils.UChar.t

val capital_t : Utils.UChar.t

val capital_v : Utils.UChar.t

val capital_z : Utils.UChar.t

(** {7 Small letters} *)

val small_c : Utils.UChar.t

val small_e : Utils.UChar.t

val small_s : Utils.UChar.t

(** {7 Digits} *)

val digits : Utils.UChar.t list

(** {7 Special characters} *)

val at_character : Utils.UChar.t

val back_slash : Utils.UChar.t

val circonflex : Utils.UChar.t

val closing_parenthesis : Utils.UChar.t

val closing_square_bracket : Utils.UChar.t

val colon : Utils.UChar.t

val comma : Utils.UChar.t

val dollar : Utils.UChar.t

val dot : Utils.UChar.t

val greater_than : Utils.UChar.t

val lower_than : Utils.UChar.t

val minus : Utils.UChar.t

val opening_parenthesis : Utils.UChar.t

val opening_square_bracket : Utils.UChar.t

val plus : Utils.UChar.t

val quote : Utils.UChar.t

val semi_colon : Utils.UChar.t

val sharp : Utils.UChar.t

val slash : Utils.UChar.t

val space : Utils.UChar.t

val star : Utils.UChar.t

val tabulation : Utils.UChar.t

val underscore : Utils.UChar.t

val percentage : Utils.UChar.t

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


(* Java constants *)

let magic_number = 0x0CAFEBABEL


(* String constants *)

(* Attribute-related strings *)

let attr_annotation_default = @"AnnotationDefault"

let attr_bootstrap_methods = @"BootstrapMethods"

let attr_code = @"Code"

let attr_constant_value = @"ConstantValue"

let attr_deprecated = @"Deprecated"

let attr_enclosing_method = @"EnclosingMethod"

let attr_exceptions = @"Exceptions"

let attr_inner_classes = @"InnerClasses"

let attr_line_number_table = @"LineNumberTable"

let attr_local_variable_table = @"LocalVariableTable"

let attr_local_variable_type_table = @"LocalVariableTypeTable"

let attr_module = @"Module"

let attr_module_requires = @"ModuleRequires"

let attr_module_permits = @"ModulePermits"

let attr_module_provides = @"ModuleProvides"

let attr_runtime_invisible_annotations = @"RuntimeInvisibleAnnotations"

let attr_runtime_invisible_parameter_annotations = @"RuntimeInvisibleParameterAnnotations"

let attr_runtime_invisible_type_annotations = @"RuntimeInvisibleTypeAnnotations"

let attr_runtime_visible_annotations = @"RuntimeVisibleAnnotations"

let attr_runtime_visible_parameter_annotations = @"RuntimeVisibleParameterAnnotations"

let attr_runtime_visible_type_annotations = @"RuntimeVisibleTypeAnnotations"

let attr_signature = @"Signature"

let attr_source_debug_extension = @"SourceDebugExtension"

let attr_source_file = @"SourceFile"

let attr_stack_map_table = @"StackMapTable"

let attr_synthetic = @"Synthetic"

let attr_all = [ attr_annotation_default;
                 attr_bootstrap_methods;
                 attr_code;
                 attr_constant_value;
                 attr_deprecated;
                 attr_enclosing_method;
                 attr_exceptions;
                 attr_inner_classes;
                 attr_line_number_table;
                 attr_local_variable_table;
                 attr_local_variable_type_table;
                 attr_module;
                 attr_module_requires;
                 attr_module_permits;
                 attr_module_provides;
                 attr_runtime_invisible_annotations;
                 attr_runtime_invisible_parameter_annotations;
                 attr_runtime_invisible_type_annotations;
                 attr_runtime_visible_annotations;
                 attr_runtime_visible_parameter_annotations;
                 attr_runtime_visible_type_annotations;
                 attr_signature;
                 attr_source_debug_extension;
                 attr_source_file;
                 attr_stack_map_table ;
                 attr_synthetic ]

(* Miscellaneous strings *)

let class_constructor = @"<init>"

let class_initializer = @"<clinit>"

let empty_utf8 = @""

let java_lang_Object = @"java/lang/Object"


(* Character constants *)

(* Capital letters *)

let capital_b = @'B'

let capital_c = @'C'

let capital_d = @'D'

let capital_f = @'F'

let capital_i = @'I'

let capital_j = @'J'

let capital_l = @'L'

let capital_s = @'S'

let capital_t = @'T'

let capital_v = @'V'

let capital_z = @'Z'

(* Small letters *)

let small_c = @'c'

let small_e = @'e'

let small_s = @'s'

(* Digits *)

let digits = [ @'0'; @'1'; @'2'; @'3'; @'4'; @'5'; @'6'; @'7'; @'8'; @'9' ]

(* Special characters *)

let at_character = @'@'

let back_slash = @'\\'

let circonflex = @'^'

let closing_parenthesis = @')'

let closing_square_bracket = @']'

let colon = @':'

let comma = @','

let dollar = @'$'

let dot = @'.'

let greater_than = @'>'

let lower_than = @'<'

let minus = @'-'

let opening_parenthesis = @'('

let opening_square_bracket = @'['

let plus = @'+'

let quote = @'"'

let semi_colon = @';'

let sharp = @'#'

let slash = @'/'

let space = @' '

let star = @'*'

let tabulation = @'\t'

let underscore = @'_'

let percentage = @'%'

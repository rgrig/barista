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

let attr_annotation_default = UTF8.of_string "AnnotationDefault"

let attr_bootstrap_methods = UTF8.of_string "BootstrapMethods"

let attr_code = UTF8.of_string "Code"

let attr_constant_value = UTF8.of_string "ConstantValue"

let attr_deprecated = UTF8.of_string "Deprecated"

let attr_enclosing_method = UTF8.of_string "EnclosingMethod"

let attr_exceptions = UTF8.of_string "Exceptions"

let attr_inner_classes = UTF8.of_string "InnerClasses"

let attr_line_number_table = UTF8.of_string "LineNumberTable"

let attr_local_variable_table = UTF8.of_string "LocalVariableTable"

let attr_local_variable_type_table = UTF8.of_string "LocalVariableTypeTable"

let attr_module = UTF8.of_string "Module"

let attr_module_requires = UTF8.of_string "ModuleRequires"

let attr_module_permits = UTF8.of_string "ModulePermits"

let attr_module_provides = UTF8.of_string "ModuleProvides"

let attr_runtime_invisible_annotations = UTF8.of_string "RuntimeInvisibleAnnotations"

let attr_runtime_invisible_parameter_annotations = UTF8.of_string "RuntimeInvisibleParameterAnnotations"

let attr_runtime_invisible_type_annotations = UTF8.of_string "RuntimeInvisibleTypeAnnotations"

let attr_runtime_visible_annotations = UTF8.of_string "RuntimeVisibleAnnotations"

let attr_runtime_visible_parameter_annotations = UTF8.of_string "RuntimeVisibleParameterAnnotations"

let attr_runtime_visible_type_annotations = UTF8.of_string "RuntimeVisibleTypeAnnotations"

let attr_signature = UTF8.of_string "Signature"

let attr_source_debug_extension = UTF8.of_string "SourceDebugExtension"

let attr_source_file = UTF8.of_string "SourceFile"

let attr_stack_map_table = UTF8.of_string "StackMapTable"

let attr_synthetic = UTF8.of_string "Synthetic"

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

let class_constructor = UTF8.of_string "<init>"

let class_initializer = UTF8.of_string "<clinit>"

let empty_utf8 = UTF8.of_string ""

let java_lang_Object = UTF8.of_string "java/lang/Object"


(* Character constants *)

(* Capital letters *)

let capital_b = UChar.of_char 'B'

let capital_c = UChar.of_char 'C'

let capital_d = UChar.of_char 'D'

let capital_f = UChar.of_char 'F'

let capital_i = UChar.of_char 'I'

let capital_j = UChar.of_char 'J'

let capital_l = UChar.of_char 'L'

let capital_s = UChar.of_char 'S'

let capital_t = UChar.of_char 'T'

let capital_v = UChar.of_char 'V'

let capital_z = UChar.of_char 'Z'

(* Small letters *)

let small_c = UChar.of_char 'c'

let small_e = UChar.of_char 'e'

let small_s = UChar.of_char 's'

(* Digits *)

let digits = List.map UChar.of_char ['0'; '1'; '2'; '3'; '4';
                                     '5'; '6'; '7'; '8'; '9'  ]

(* Special characters *)

let at_character = UChar.of_char '@'

let back_slash = UChar.of_char '\\'

let circonflex = UChar.of_char '^'

let closing_parenthesis = UChar.of_char ')'

let closing_square_bracket = UChar.of_char ']'

let colon = UChar.of_char ':'

let comma = UChar.of_char ','

let dollar = UChar.of_char '$'

let dot = UChar.of_char '.'

let greater_than = UChar.of_char '>'

let lower_than = UChar.of_char '<'

let minus = UChar.of_char '-'

let opening_parenthesis = UChar.of_char '('

let opening_square_bracket = UChar.of_char '['

let plus = UChar.of_char '+'

let quote = UChar.of_char '"'

let semi_colon = UChar.of_char ';'

let sharp = UChar.of_char '#'

let slash = UChar.of_char '/'

let space = UChar.of_char ' '

let star = UChar.of_char '*'

let tabulation = UChar.of_char '\t'

let underscore = UChar.of_char '_'

let percentage = UChar.of_char '%'

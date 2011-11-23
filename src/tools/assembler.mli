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

(** Assembling of Java assembler source. *)


type error =
  | Invalid_scope
  | Invalid_class_flags
  | Duplicate_extends
  | Duplicate_implements
  | Invalid_constant_value
  | Invalid_annotation
  | Code_too_large
  | Unable_compile
  | Duplicate_label of string
  | Undefined_label of string
  | Invalid_offset
  | Invalid_parameter
  | Invalid_flag of string
  | Flag_expected
  | Constant_out_of_bounds
  | Instruction_waited
  | Invalid_class
  | Invalid_field_flags
  | Invalid_field_name
  | Invalid_field_type
  | Invalid_field_directive
  | Invalid_return_type
  | Invalid_constructor_return_type
  | Invalid_constructor_flags
  | Invalid_initializer_signature
  | Invalid_initializer_flags
  | Invalid_method_flags
  | Invalid_method_directive
  | Exception_class_name_waited
  | Invalid_inner_class
  | Invalid_outer_class
  | Invalid_inner_name
  | Invalid_inner_class_flags
  | Invalid_method
  | Invalid_class_signature
  | Invalid_field_signature
  | Invalid_method_signature
  | Invalid_local_variable_type
  | Invalid_local_variable_signature
  | Invalid_attribute_arguments
  | Unknown_attribute
  | Invalid_directive_arguments
  | Invalid_directive
  | Syntax_error
  | Lexer_error of Lexer.error
  | ClassDefinition_error of ClassDefinition.error
  | ClassFile_error of ClassFile.error
  | Method_error of Method.error
  | Field_error of Field.error
  | Attribute_error of Attribute.error
  | Instruction_error of Instruction.error
  | ByteCode_error of ByteCode.error
  | Annotation_error of Annotation.error
  | AccessFlag_error of AccessFlag.error
  | ConstantPool_error of ConstantPool.error
  | Signature_error of Signature.error
  | Descriptor_error of Descriptor.error
  | Name_error of Name.error
  | Version_error of Version.error
  | Invalid_stack_frame
  | Internal_inner_class
  | Internal_pushback
  | Optimization_error of string
  | Several_frames_at_offset of Utils.u2
  | Unable_to_compute_stack_frames of string

exception Exception of int * error
(** Exception to be raised when a function of this module fails. *)

val string_of_error : error -> string
(** Converts the passed error into a string. *)

type destination =
  | Stream of OutputStream.t
    (** Assembly results in an append to the passed output stream. *)
  | Path of string
    (** Assembly results in a file creation, relatively to the passed path. *)
(** The type of assembly destination. *)

val assemble : ?version : Version.t -> ?compute_stacks : bool -> ?optimize : bool -> ?class_loader: ClassLoader.t -> InputStream.t -> destination -> Name.for_class
(** Assembles the file whose source input channel is passed, returning
    the name of the compiled class. The default version used for class
    encoding is set to [Version.default], and both automatic computation
    of stack elements and optimizations are disabled. The produced class
    file is written to the passed destination.
    Raises [Exception] if an error occurs. *)

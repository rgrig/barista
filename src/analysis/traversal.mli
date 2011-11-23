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

(** Class types and default implementations for "mapper" and "iterator"
    over class definitions.
    Functions from [ClassDefinition] should be used for applications
    of "mapppers" and "iterators". *)

class type class_definition_mapper = object
  method class_definition : AccessFlag.for_class list -> Name.for_class -> Name.for_class option -> Name.for_class list -> Field.t list -> Method.t list -> Attribute.for_class list -> (AccessFlag.for_class list * Name.for_class * Name.for_class option * Name.for_class list * Field.t list * Method.t list * Attribute.for_class list)
  method class_flags : AccessFlag.for_class list -> AccessFlag.for_class list
  method class_name : Name.for_class -> Name.for_class
  method class_extends : Name.for_class option -> Name.for_class option
  method class_implements : Name.for_class -> Name.for_class
  method class_field : Field.t -> Field.t
  method class_method : Method.t -> Method.t
  method class_attribute : Attribute.for_class -> Attribute.for_class
  method regular_method : Method.regular -> Method.regular
  method constructor_method : Method.constructor -> Method.constructor
  method initializer_method : Method.class_initializer -> Method.class_initializer
end
(** This class type defines a "mapper", instances being used as
    "functions" to transform class definitions. The "function" is defined
    {i by parts} through the various methods of the object. Any method is
    responsible for the calling of its embedded elements; this means that
    the method [class_definition] should call methods to map fields,
    attributes, etc. *)

class default_class_definition_mapper : class_definition_mapper
(** The default "mapper", that encodes the identity over class
    definitions. When inheriting from this class, one should not forget
    to call the parent methods in order to ensure that the whole
    structure is mapped. *)    

class type class_definition_iterator = object
  method class_definition : AccessFlag.for_class list -> Name.for_class -> Name.for_class option -> Name.for_class list -> Field.t list -> Method.t list -> Attribute.for_class list -> unit
  method class_flags : AccessFlag.for_class list -> unit
  method class_name : Name.for_class -> unit
  method class_extends : Name.for_class option -> unit
  method class_implements : Name.for_class -> unit
  method class_field : Field.t -> unit
  method class_method : Method.t -> unit
  method class_attribute : Attribute.for_class -> unit
  method regular_method : Method.regular -> unit
  method constructor_method : Method.constructor -> unit
  method initializer_method : Method.class_initializer -> unit
end
(** This class type defines an "iterator", instances being used as
    "functions" to iterate over the different components of a class
    definitions. The "function" is defined by parts through the various
    methods of the object. Any method is responsible for the calling of
    its embedded elements; this means that the method [class_definition]
    should call methods to map fields, attributes, etc. *)

class default_class_definition_iterator : class_definition_iterator
(** The default "iterator", that iterates over the whole structure by
    doing nothing. When inheriting from this class, one should not forget
    to call the parent methods in order to ensure that the whole
    structure is traversed. *)

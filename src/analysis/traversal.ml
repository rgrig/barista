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

class default_class_definition_mapper : class_definition_mapper = object (self)
  method class_definition flags name extends implements fields methods attributes =
    let flags' = self#class_flags flags in
    let name' = self#class_name name in
    let extends' = self#class_extends extends in
    let implements' = List.map self#class_implements implements in
    let fields' = List.map self#class_field fields in
    let methods' = List.map self#class_method methods in
    let attributes' = List.map self#class_attribute attributes in
    (flags', name', extends', implements', fields', methods', attributes')

  method class_flags x = x

  method class_name x = x

  method class_extends x = x

  method class_implements x = x

  method class_field x = x

  method class_method = function
    | Method.Regular mr -> Method.Regular (self#regular_method mr)
    | Method.Constructor mc -> Method.Constructor (self#constructor_method mc)
    | Method.Initializer mi -> Method.Initializer (self#initializer_method mi)

  method class_attribute x = x

  method regular_method x = x

  method constructor_method x = x

  method initializer_method x = x

end

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

class default_class_definition_iterator : class_definition_iterator = object (self)
  method class_definition flags name extends implements fields methods attributes =
    self#class_flags flags;
    self#class_name name;
    self#class_extends extends;
    List.iter self#class_implements implements;
    List.iter self#class_field fields;
    List.iter self#class_method methods;
    List.iter self#class_attribute attributes

  method class_flags _ = ()

  method class_name _ = ()

  method class_extends _ = ()

  method class_implements _ = ()

  method class_field _ = ()

  method class_method = function
    | Method.Regular x -> self#regular_method x
    | Method.Constructor x -> self#constructor_method x
    | Method.Initializer x -> self#initializer_method x

  method class_attribute _ = ()

  method regular_method _ = ()

  method constructor_method _ = ()

  method initializer_method _ = ()

end

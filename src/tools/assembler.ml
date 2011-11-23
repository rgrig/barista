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
open LineUtils
open Consts


(* Exception *)

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
  | Several_frames_at_offset of u2
  | Unable_to_compute_stack_frames of string

exception Exception of int * error

let string_of_error = function
  | Invalid_scope -> "invalid scope"
  | Invalid_class_flags -> "invalid class flags"
  | Duplicate_extends -> "duplicate .extends directive"
  | Duplicate_implements -> "duplicate .implements directive"
  | Invalid_constant_value -> "invalid constant value"
  | Invalid_annotation -> "invalid annotation"
  | Code_too_large -> "code is too large"
  | Unable_compile -> "unable to compile instruction"
  | Duplicate_label s -> Printf.sprintf "duplicate label %S" s
  | Undefined_label s -> Printf.sprintf "undefined label %S" s
  | Invalid_offset -> "end offset should be greater than start offset"
  | Invalid_parameter -> "invalid parameter"
  | Invalid_flag s -> Printf.sprintf "invalid flag %S" s
  | Flag_expected -> "flag expected"
  | Constant_out_of_bounds -> "constant is out of bounds (integer value in 0..65535 waited)"
  | Instruction_waited -> "instruction waited after 'wide' metainstruction"
  | Invalid_class -> "invalid .class directive"
  | Invalid_field_flags -> "invalid field flags"
  | Invalid_field_name -> "invalid field name"
  | Invalid_field_type -> "invalid field type"
  | Invalid_field_directive -> "invalid .field directive"
  | Invalid_return_type -> "invalid return type"
  | Invalid_constructor_return_type -> "invalid constructor return type"
  | Invalid_constructor_flags -> "invalid constructor flags"
  | Invalid_initializer_signature -> "invalid initializer signature"
  | Invalid_initializer_flags -> "invalid initializer flags"
  | Invalid_method_flags -> "invalid method flags"
  | Invalid_method_directive -> "invalid .method directive"
  | Exception_class_name_waited -> "exception class name waited"
  | Invalid_inner_class -> "invalid inner class"
  | Invalid_outer_class -> "invalid outer class"
  | Invalid_inner_name -> "invalid inner name"
  | Invalid_inner_class_flags -> "invalid inner class flags"
  | Invalid_method -> "invalid method"
  | Invalid_class_signature -> "invalid class signature"
  | Invalid_field_signature -> "invalid field signature"
  | Invalid_method_signature -> "invalid method signature"
  | Invalid_local_variable_type -> "invalid local variable type"
  | Invalid_local_variable_signature -> "invalid local variable signature"
  | Invalid_attribute_arguments -> "invalid attribute argument(s)"
  | Unknown_attribute -> "unknown attribute"
  | Invalid_directive_arguments -> "invalid directive argument(s)"
  | Invalid_directive -> "unknown directive"
  | Syntax_error -> "syntax error"
  | Lexer_error e -> Printf.sprintf "lexing error (%s)" (Lexer.string_of_error e)
  | ClassDefinition_error e -> ClassDefinition.string_of_error e
  | ClassFile_error e -> ClassFile.string_of_error e
  | Method_error e -> Method.string_of_error e
  | Field_error e -> Field.string_of_error e
  | Attribute_error e -> Attribute.string_of_error e
  | Instruction_error e -> Instruction.string_of_error e
  | ByteCode_error e -> ByteCode.string_of_error e
  | Annotation_error e -> Annotation.string_of_error e
  | AccessFlag_error e -> AccessFlag.string_of_error e
  | ConstantPool_error e -> ConstantPool.string_of_error e
  | Signature_error e -> Signature.string_of_error e
  | Descriptor_error e -> Descriptor.string_of_error e
  | Name_error e -> Name.string_of_error e
  | Version_error e -> Version.string_of_error e
  | Invalid_stack_frame -> "invalid frame definition"
  | Internal_inner_class -> "internal error (single inner class attribute waited)"
  | Internal_pushback -> "internal error (pushback to full buffer)"
  | Optimization_error s -> Printf.sprintf "unable to optimize method (%s)" s
  | Several_frames_at_offset x -> Printf.sprintf "several frames at offset %d" (x :> int)
  | Unable_to_compute_stack_frames s -> Printf.sprintf "unable to compute stack frames (%s)" s

let () =
  Printexc.register_printer
    (function
      | Exception (i, e) -> Some (Printf.sprintf "%d: %s" i (string_of_error e))
      | _ -> None)


(* Types *)

type destination =
  | Stream of OutputStream.t
  | Path of string

type scope =
  | Nothing
  | Class
  | Field of Field.t
  | Method of int * Method.t

type instruction =
  | Compiled of Instruction.t
  | To_compile of int * (Instruction.t lazy_t)

type tail =
  | Empty_tail
  | Match_offset_pairs of (int64 * UTF8.t) list
  | Offsets of UTF8.t list


(* State *)

let check_version_flags version l =
  let bounds = List.map AccessFlag.version_bounds l in
  match bounds with
  | _ :: _ -> Version.check (Version.intersect_list bounds) version
  | [] -> ()

let default_annotation_name = UTF8.of_string "defaultannotation.class"
let default_annotation_property = Lexer.Identifier (UTF8.of_string "defaultannotation")

class assembler_state version compute_stacks optimize class_loader =
  object (self)

      (* Line *)
    val mutable line = 0
    method get_line = line
    method incr_line = line <- succ line
    method decr_line = line <- pred line
    method fail_line : 'a . int -> error -> 'a =
      fun n e -> raise (Exception (n, e))
    method fail_err : 'a . error -> 'a =
      fun e -> raise (Exception (line, e))
    method fail_scope : 'a . 'a =
      raise (Exception (line, Invalid_scope))

      (* Scope *)
    val mutable current_scope = Nothing
    method check_scope_not_nothing =
      if current_scope = Nothing then self#fail_scope
    method check_scope_nothing =
      if current_scope <> Nothing then self#fail_scope
    method check_scope_class =
      if current_scope <> Class then self#fail_scope
    method check_scope_method =
      match current_scope with
      | Method _ -> ()
      | _ -> self#fail_scope
    method get_scope =
      current_scope
    method set_scope s =
      current_scope <- s

      (* Class data *)
    val mutable class_flags = []
    val mutable class_name = Name.make_for_class_from_external (UTF8.of_string "pack.Class")
    val mutable class_extends = None
    val mutable class_implements = []
    val mutable class_fields = []
    val mutable class_methods = []
    val mutable class_attributes = []
    method set_class_flags flags =
      (try
        class_flags <- AccessFlag.check_class_flags (List.map self#flag_of_token flags)
      with AccessFlag.Exception _ ->
        self#fail_err Invalid_class_flags);
      (try
        check_version_flags
          version
          (class_flags :> AccessFlag.t list)
      with Version.Exception e -> self#fail_err (Version_error e))
    method is_interface =
      List.mem `Interface class_flags
    method set_class_name cn =
      class_name <- cn
    method get_class_name =
      class_name
    method set_class_extends p =
      if class_extends <> None then
        self#fail_err Duplicate_extends
      else
        class_extends <- Some p
    method add_class_implements i =
      if List.exists (Name.equal_for_class i) class_implements then
        self#fail_err Duplicate_implements
      else
        class_implements <- i :: class_implements
    method write_class dst =
      let rec mkdirs dir =
        match Filename.dirname dir with
        | "." | "/" | "\\" ->
	    if not (Sys.file_exists dir) then
              Unix.mkdir dir 0o755
        | parent ->
            mkdirs parent;
	    if not (Sys.file_exists dir) then
              Unix.mkdir dir 0o755 in
      let def = {
        ClassDefinition.access_flags = class_flags;
        ClassDefinition.name = class_name;
        ClassDefinition.extends = class_extends;
        ClassDefinition.implements = List.rev class_implements;
        ClassDefinition.fields = List.rev class_fields;
        ClassDefinition.methods = List.rev class_methods;
        ClassDefinition.attributes = List.rev class_attributes;
      } in
      let file = ClassDefinition.encode ~version:version def in
      match dst with
      | Stream os -> ClassFile.write file os
      | Path s ->
          let last_index = pred (String.length s) in
          let full_path =
            (if s.[last_index] <> '/' then s ^ "/" else s) ^
            (UTF8.to_string (Name.internal_utf8_for_class class_name)) ^ ".class" in
          let dir_path = Filename.dirname full_path in
          if dir_path <> "." then mkdirs dir_path;
          let os = OutputStream.make_of_channel (open_out full_path) in
          ClassFile.write file os;
          OutputStream.close os

        (* Attributes *)
    method add_class_attribute a =
      self#check_scope_class;
      class_attributes <- a :: class_attributes
    method add_class_attribute_inner a =
      let inner, others = List.partition
          (function
            | `InnerClasses _ -> true
            | _ -> false)
          class_attributes in
      let inner' = match inner with
      | [] -> `InnerClasses [a]
      | [`InnerClasses cl] ->
          `InnerClasses (cl @ [a])
      | _ -> self#fail_err Internal_inner_class in
      class_attributes <- inner' :: others
    method add_field_attribute a =
      match current_scope with
      | Field f -> current_scope <- Field { f with Field.attributes = a :: f.Field.attributes }
      | _ -> self#fail_scope
    method add_field_attribute_const const_value =
      match current_scope with
      | Field f ->
          let v = match const_value, f.Field.descriptor with
          | Lexer.Int x, `Long -> Attribute.Long_value x
          | Lexer.Float x, `Float -> Attribute.Float_value x
          | Lexer.Float x, `Double -> Attribute.Double_value x
          | Lexer.Int x, `Boolean -> Attribute.Boolean_value (x <> 0L)
          | Lexer.Int x, `Byte -> Attribute.Byte_value (Int64.to_int x)
          | Lexer.Int x, `Char -> Attribute.Character_value (Int64.to_int x)
          | Lexer.Int x, `Short -> Attribute.Short_value (Int64.to_int x)
          | Lexer.Int x, `Int -> Attribute.Integer_value (Int64.to_int32 x)
          | Lexer.String x, `Class c ->
              if UTF8.equal (UTF8.of_string "java.lang.String") (Name.external_utf8_for_class c) then
                Attribute.String_value x
              else
                self#fail_err Invalid_constant_value
          | _ -> self#fail_err Invalid_constant_value in
          self#add_field_attribute (`ConstantValue v)
      | _ -> self#fail_scope
    method add_method_attribute a =
      match current_scope with
      | Method (l, m) -> (match m with
        | Method.Regular r ->
            current_scope <- Method (l, Method.Regular { r with Method.attributes = a :: r.Method.attributes })
        | Method.Constructor c ->
            current_scope <- Method (l, Method.Constructor { c with Method.cstr_attributes = a :: c.Method.cstr_attributes })
        | Method.Initializer i ->
            current_scope <- Method (l, Method.Initializer { i with Method.init_attributes = a :: i.Method.init_attributes }))
      | _ -> self#fail_scope

            (* Annotations *)
    val visible_annotations = UTF8Hashtbl.create 8
    val invisible_annotations = UTF8Hashtbl.create 8
    val visible_parameter_annotations = Hashtbl.create 8
    val invisible_parameter_annotations = Hashtbl.create 8
    val default_annotation = UTF8Hashtbl.create 8
    method private add_annot tbl name tokens =
        let current = try UTF8Hashtbl.find tbl name with _ -> [] in
        UTF8Hashtbl.replace tbl name ((line, tokens) :: current)
    method add_annotation b name tokens =
      let tbl = if b then visible_annotations else invisible_annotations in
      self#add_annot tbl name tokens
    method add_parameter_annotation b (idx : int) name tokens =
      let tbl = if b then visible_parameter_annotations else invisible_parameter_annotations in
      let tbl' = try
        Hashtbl.find tbl idx
      with _ ->
        let res = UTF8Hashtbl.create 8 in
        Hashtbl.add tbl idx res;
        res in
      self#add_annot tbl' name tokens
    method add_default_annotation tokens =
      self#check_scope_method;
      self#add_annot default_annotation default_annotation_name (default_annotation_property :: tokens)
    method private make_pairs p =
      UTF8Hashtbl.fold
        (fun name l accu ->
          let v = match l with
          | [_, (Lexer.Primitive_type `Boolean) :: (Lexer.Int b) :: []] ->
              Annotation.Boolean_value (b <> 0L)
          | [_, (Lexer.Primitive_type `Byte) :: (Lexer.Int b) :: []] ->
              Annotation.Byte_value (Int64.to_int b)
          | [_, (Lexer.Primitive_type `Char) :: (Lexer.Int c) :: []] ->
              Annotation.Char_value (UChar.of_code (Int64.to_int c))
          | [_, (Lexer.Primitive_type `Double) :: (Lexer.Float d) :: []] ->
              Annotation.Double_value d
          | [_, (Lexer.Primitive_type `Float) :: (Lexer.Float f) :: []] ->
              Annotation.Float_value f
          | [_, (Lexer.Primitive_type `Int) :: (Lexer.Int i) :: []] ->
              Annotation.Int_value (Int64.to_int32 i)
          | [_, (Lexer.Primitive_type `Long) :: (Lexer.Int l) :: []] ->
              Annotation.Long_value l
          | [_, (Lexer.Primitive_type `Short) :: (Lexer.Int s) :: []] ->
              Annotation.Short_value (Int64.to_int s)
          | [_, (Lexer.Identifier id) :: (Lexer.String s) :: []]
            when UTF8.equal id (UTF8.of_string "string") ->
              Annotation.String_value s
          | [_, (Lexer.Identifier id) :: (Lexer.Class_name n) :: (Lexer.Identifier f) :: []]
            when UTF8.equal id (UTF8.of_string "enum") ->
              Annotation.Enum_value (n, (Name.make_for_field f))
          | [_, (Lexer.Identifier id) :: (Lexer.Class_name n) :: []]
            when UTF8.equal id (UTF8.of_string "class") ->
              Annotation.Class_value n
          | list ->
              if (List.for_all (function | _, (Lexer.Identifier id) :: _ when UTF8.equal id (UTF8.of_string "annotation") -> true | _ -> false) list) then
                let pairs = UTF8Hashtbl.create 8 in
                match list with
                | (ln, (Lexer.Identifier _) :: (Lexer.Class_name cn) :: (Lexer.Identifier name) :: l)
                  :: tail ->
                    UTF8Hashtbl.replace pairs name [ln, l];
                    List.iter (function
                      | ln, (Lexer.Identifier _) :: (Lexer.Class_name cn') :: l ->
                          if not (UTF8.equal (Name.external_utf8_for_class cn) (Name.external_utf8_for_class cn')) then
                            self#fail_line ln Invalid_annotation
                          else
                            let current = try UTF8Hashtbl.find pairs name with _ -> [] in
                            UTF8Hashtbl.replace pairs name ((ln, l) :: current)
                      | ln, _ -> self#fail_line ln Invalid_annotation)
                      tail;
                    Annotation.Annotation_value (cn, (self#make_pairs pairs))
                | elems ->
                    let ln = List.fold_left (fun acc x -> min acc (fst x)) 1 elems in
                    self#fail_line ln Invalid_annotation
              else if (List.for_all (function | _, (Lexer.Int _) :: _ -> true | _ -> false) list) then
                let array = UTF8Hashtbl.create 8 in
                List.iter
                  (function
                    | ln, (Lexer.Int idx) :: tail ->
                        let idx' = UTF8.of_string (Int64.to_string idx) in
                        let current = try UTF8Hashtbl.find array idx' with _ -> [] in
                        UTF8Hashtbl.replace array idx' ((ln, tail) :: current)
                    | _ ->
                        let ln = List.fold_left (fun acc x -> min acc (fst x)) 1 list in
                        self#fail_line ln Invalid_annotation)
                  list;
                let pairs = self#make_pairs array in
                let pairs' =
                  List.sort
                    (fun (x, _) (y, _) -> compare x y)
                    (List.map
                       (fun (n, v) -> (int_of_string (UTF8.to_string n)), v)
                       pairs) in
                Annotation.Array_value (List.map snd pairs')
              else
                let ln = List.fold_left (fun acc x -> min acc (fst x)) 1 list in
                self#fail_line ln Invalid_annotation in
          (name, v) :: accu)
        p
        []
    method private compile_hashtable tbl =
      let res = UTF8Hashtbl.fold
          (fun cn l accu ->
            let pairs = UTF8Hashtbl.create 8 in
            List.iter
              (function
                | (line, ((Lexer.Identifier name) :: tokens)) ->
                    let current = try UTF8Hashtbl.find pairs name with _ -> [] in
                    UTF8Hashtbl.replace pairs name ((line, tokens) :: current)
                | (_, []) -> ()
                | (line, _) -> self#fail_line line Invalid_annotation)
              l;
            let pairs' = self#make_pairs pairs in
            let cn' = Name.make_for_class_from_external cn in
            (cn', pairs') :: accu)
          tbl
          [] in
      UTF8Hashtbl.clear tbl;
      res
    method private compile_annotations =
      (if UTF8Hashtbl.length visible_annotations > 0 then
        [(`RuntimeVisibleAnnotations (self#compile_hashtable visible_annotations))]
      else
        [])
      @ (if UTF8Hashtbl.length invisible_annotations > 0 then
        [(`RuntimeInvisibleAnnotations (self#compile_hashtable invisible_annotations))]
      else
        [])
    method private compile_parameter_annotations =
      let compile tbl =
        let params = Hashtbl.fold
            (fun idx tbl' accu ->
              (idx, (self#compile_hashtable tbl')) :: accu)
            tbl
            [] in
        Hashtbl.clear tbl;
        List.map
          snd
          (List.sort
             (fun (x, _) (y, _) -> compare x y)
             params) in
      (if Hashtbl.length visible_parameter_annotations > 0 then
        [(`RuntimeVisibleParameterAnnotations (compile visible_parameter_annotations))]
      else
        [])
      @ (if Hashtbl.length invisible_parameter_annotations > 0 then
        [(`RuntimeInvisibleParameterAnnotations (compile invisible_parameter_annotations))]
      else
        [])
    method private compile_default_annotation =
      if UTF8Hashtbl.length default_annotation > 0 then
        let annot = self#compile_hashtable default_annotation in
        UTF8Hashtbl.clear default_annotation;
        [`AnnotationDefault (snd (List.hd (snd (List.hd annot))))]
      else
        []

          (* Code elements *)
    val mutable current_code = []
    val mutable current_pc = 0
    val mutable max_stack = None
    val mutable max_locals = None
    val mutable labels = UTF8Hashtbl.create 32
    val mutable exceptions = []
    val mutable frames = []
    val mutable code_attributes = []
    val mutable line_numbers = []
    val mutable local_variables = []
    val mutable local_variable_types = []
    method set_max_stack n =
      match current_scope with
      | Method _ -> max_stack <- Some (self#check_u2 n)
      | _ -> self#fail_scope
    method set_max_locals n =
      match current_scope with
      | Method _ -> max_locals <- Some (self#check_u2 n)
      | _ -> self#fail_scope
    method compile_instruction wide id args tail =
      let size, cmp = try
        Instruction.compile
          current_pc
          wide
          (UTF8.to_string id)
          (self#params_of_tokens line None args)
          (self#compile_tail line None tail)
      with Instruction.Exception e -> self#fail_err (Instruction_error e) in
      (try
        let bounds = Instruction.version_bounds cmp in
        Version.check bounds version
      with Version.Exception e -> self#fail_err (Version_error e));
      if (List.exists self#is_label args) || (tail <> Empty_tail) then
        let pc = current_pc in
        let ln = line in
        let lz = lazy (snd (Instruction.compile
                              pc
                              wide
                              (UTF8.to_string id)
                              (self#params_of_tokens ln (Some pc) args)
                              (self#compile_tail ln (Some pc) tail))) in
        current_code <- (To_compile (ln, lz)) :: current_code;
      else
        current_code <- (Compiled cmp) :: current_code;
      if size + current_pc > current_pc then
        current_pc <- size + current_pc
      else
        self#fail_err Code_too_large
    method private compile_instructions =
      List.rev_map
        (function
          | Compiled x -> x
          | To_compile (n, y) ->
              try
                Lazy.force y
              with
              | Instruction.Exception e -> self#fail_line n (Instruction_error e)
              | _ -> self#fail_line n Unable_compile)
        current_code
    method add_label lbl =
      if UTF8Hashtbl.mem labels lbl then
        self#fail_err (Duplicate_label (UTF8.to_string lbl))
      else
        UTF8Hashtbl.add labels lbl current_pc
    method get_label_offset line lbl =
      try
        UTF8Hashtbl.find labels lbl
      with Not_found ->
        self#fail_line line (Undefined_label (UTF8.to_string lbl))
    method add_exception start_pc end_pc handler_pc caught =
      match current_scope with
      | Method _ ->
          exceptions <- (line, start_pc, end_pc, handler_pc, caught) :: exceptions
      | _ -> self#fail_scope
    method private compile_exceptions =
      List.rev_map
        (fun (line, start_pc, end_pc, handler_pc, caught) ->
          let s = self#get_label_offset line start_pc in
          let e = self#get_label_offset line end_pc in
          let h = self#get_label_offset line handler_pc in
          if s <= e then
            { Attribute.try_start = u2 s;
              Attribute.try_end = u2 e;
              Attribute.catch = u2 h;
              Attribute.caught = caught }
          else
            self#fail_line line Invalid_offset)
        exceptions
    method add_frame lbl tokens =
      match current_scope with
      | Method _ -> frames <- (line, lbl, tokens) :: frames
      | _ -> self#fail_scope
    method private compile_frames =
      let rec split acc = function
        | [] -> (List.rev acc), []
        | Lexer.Tilde :: tl -> (List.rev acc), tl
        | hd :: tl -> split (hd :: acc) tl in
      let array_type_of_utf8 line s =
        let rec make_array n t =
          if n = 1 then
            `Array (Descriptor.filter_void Descriptor.Invalid_array_element_type t)
          else
            let sub_array = make_array (pred n) t in
            `Array (Descriptor.filter_void
                      Descriptor.Invalid_array_element_type
                      (sub_array :> Descriptor.java_type)) in
        let idx = UTF8.index_from s 0 opening_square_bracket in
        let base = UTF8.substring s 0 (pred idx) in
        make_array
          (((UTF8.length s) - idx + 1) / 2)
          (match UTF8.to_string base with
          | "boolean" -> `Boolean
          | "byte" -> `Byte
          | "char" -> `Char
          | "double" -> `Double
          | "float" -> `Float
          | "int" -> `Int
          | "long" -> `Long
          | "short" -> `Short
          | "void" -> self#fail_line line Invalid_stack_frame
          | _ -> `Class (Name.make_for_class_from_external base)) in
      let rec decode_types line acc = function
        | [] -> List.rev acc
        | (Lexer.Primitive_type `Int) :: tl ->
            decode_types line (Attribute.Integer_variable_info :: acc) tl
        | (Lexer.Primitive_type `Float) :: tl ->
                decode_types line (Attribute.Float_variable_info :: acc) tl
        | (Lexer.Primitive_type `Long) :: tl ->
                decode_types line (Attribute.Long_variable_info :: acc) tl
        | (Lexer.Primitive_type `Double) :: tl ->
                decode_types line (Attribute.Double_variable_info :: acc) tl
        | (Lexer.Array_type at) :: tl ->
                decode_types line (Attribute.Object_variable_info (`Array_type (array_type_of_utf8 line at)) :: acc) tl
        | (Lexer.Identifier typ) :: tl ->
            (match (try UTF8.to_string typ with _ -> "") with
            | "top" ->
                decode_types line (Attribute.Top_variable_info :: acc) tl
            | "null" ->
                decode_types line (Attribute.Null_variable_info :: acc) tl
            | "uninit_this" ->
                decode_types line (Attribute.Uninitialized_this_variable_info :: acc) tl
            | "uninit" ->
                (match tl with
                | (Lexer.Label lbl) :: tl' ->
                    let ofs = self#get_label_offset line lbl in
                    decode_types line ((Attribute.Uninitialized_variable_info (u2 ofs)) :: acc) tl'
                | _ -> self#fail_line line Invalid_stack_frame)
            | _ ->
                decode_types line ((Attribute.Object_variable_info (`Class_or_interface (Name.make_for_class_from_external typ))) :: acc) tl)
        | _ -> self#fail_line line Invalid_stack_frame in
      let frames' =
        List.sort
          (fun (_, x1, _) (_, x2, _) -> compare x1 x2)
          (List.map
             (fun (line, lbl, tokens) ->
               let lbl' = self#get_label_offset line lbl in
               (line, lbl', tokens))
             frames) in
      let seen_offsets = ref [] in
      let frames'' =
        List.map
          (fun (line, ofs, tokens) ->
            let ofs = u2 ofs in
            if List.mem ofs !seen_offsets then
              self#fail_line line (Several_frames_at_offset ofs)
            else
              seen_offsets := ofs :: !seen_offsets;
            match tokens with
            | (Lexer.Identifier kind) :: tl ->
                (match (try UTF8.to_string kind with _ -> "") with
                | "same" ->
                    (match tl with
                    | [] -> Attribute.Same_frame ofs
                    | _ -> self#fail_line line Invalid_stack_frame)
                | "same_locals" ->
                    (match decode_types line [] tl with
                    | [e] -> Attribute.Same_locals_1_stack_item_frame (ofs, e)
                    | _ -> self#fail_line line Invalid_stack_frame)
                | "chop" ->
                    (match tl with
                    | [Lexer.Int 1L] -> Attribute.Chop_1_frame ofs
                    | [Lexer.Int 2L] -> Attribute.Chop_2_frame ofs
                    | [Lexer.Int 3L] -> Attribute.Chop_3_frame ofs
                    | _ -> self#fail_line line Invalid_stack_frame)
                | "append" ->
                    (match decode_types line [] tl with
                    | [t1] -> Attribute.Append_1_frame (ofs, t1)
                    | [t1; t2] -> Attribute.Append_2_frame (ofs, t1, t2)
                    | [t1; t2; t3] -> Attribute.Append_3_frame (ofs, t1, t2, t3)
                    | _ -> self#fail_line line Invalid_stack_frame)
                | "full" ->
                    let l1, l2 = split [] tl in
                    Attribute.Full_frame (ofs, (decode_types line [] l1), (decode_types line [] l2))
                | _ -> self#fail_line line Invalid_stack_frame)
            | _ -> self#fail_line line Invalid_stack_frame)
          frames' in
      match frames with
      | [] -> None
      | _ -> Some frames''
    method add_code_attribute a =
      self#check_scope_method;
      code_attributes <- a :: code_attributes
    method add_line_number sl =
      self#check_scope_method;
      let source_line = match sl with
      | Some l -> self#check_u2 l
      | None -> line in
      line_numbers <- (u2 current_pc, u2 source_line) :: line_numbers
    method add_local_variable start finish id desc idx =
      self#check_scope_method;
      local_variables <- (line, start, finish, id, desc, (self#check_u2 idx)) :: local_variables
    method add_local_variable_type start finish id sign idx =
      self#check_scope_method;
      local_variable_types <- (line, start, finish, id, sign, (self#check_u2 idx)) :: local_variable_types
    method private compile_attributes use_line_numbers =
      let ln =
        if use_line_numbers && line_numbers <> [] then
          [`LineNumberTable (List.rev line_numbers)]
        else
          [] in
      let compile_table (line, start, finish, id, elem, idx) =
        let start_pc = self#get_label_offset line start in
        let end_pc = self#get_label_offset line finish in
        if end_pc <= start_pc then
          self#fail_line line Invalid_offset
        else
          { Attribute.local_start = u2 start_pc;
            Attribute.local_length = u2 (end_pc - start_pc + 1);
            Attribute.local_name = id;
            Attribute.local_descriptor = elem;
            Attribute.local_index = u2 idx } in
      let compile_table_type (line, start, finish, id, elem, idx) =
        let start_pc = self#get_label_offset line start in
        let end_pc = self#get_label_offset line finish in
        if end_pc <= start_pc then
          self#fail_line line Invalid_offset
        else
          { Attribute.local_type_start = u2 start_pc;
            Attribute.local_type_length = u2 (end_pc - start_pc + 1);
            Attribute.local_type_name = id;
            Attribute.local_type_signature = elem;
            Attribute.local_type_index = u2 idx } in
      let lvt =
        if local_variables <> [] then
          [`LocalVariableTable (List.rev_map compile_table local_variables)]
        else
          [] in
      let lvtt =
        if local_variable_types <> [] then
          [`LocalVariableTypeTable (List.rev_map compile_table_type local_variable_types)]
        else
          [] in
      ln @ lvt @ lvtt @ (List.rev code_attributes)
    method is_code_empty =
      current_code = []
    method private clear_code_elements =
      current_code <- [];
      current_pc <- 0;
      max_stack <- None;
      max_locals <- None;
      UTF8Hashtbl.clear labels;
      exceptions <- [];
      frames <- [];
      code_attributes <- [];
      line_numbers <- [];
      local_variables <- [];
      local_variable_types <- []

          (* Helper methods *)
    method private is_label tok =
      match tok with
      | Lexer.Label _ -> true
      | _ -> false
    method private param_of_token line base tok =
      match tok with
      | Lexer.Label l ->
          Instruction.Offset
            (match base with
            | None ->  0l
            | Some b -> Int32.of_int ((self#get_label_offset line l) -  b))
      | Lexer.Int i -> Instruction.Int_constant i
      | Lexer.Float f -> Instruction.Float_constant f
      | Lexer.String s -> Instruction.String_constant s
      | Lexer.Class_name cn -> Instruction.Class_name cn
      | Lexer.Array_type at -> Instruction.Array_type at
      | Lexer.Primitive_type pt -> Instruction.Primitive_type pt
      | Lexer.Field (x, y, z) -> Instruction.Field (x, y, z)
      | Lexer.Method (x, y, z) -> Instruction.Method (x, y, z)
      | Lexer.Array_method (x, y, z) -> Instruction.Array_method (x, y, z)
      | Lexer.Method_type x -> Instruction.Method_type_constant x
      | Lexer.Method_handle x -> Instruction.Method_handle_constant x
      | Lexer.Directive _
      | Lexer.Attribute _
      | Lexer.Method_signature _
      | Lexer.Identifier _
      | Lexer.Arrow
      | Lexer.Dynamic_method _
      | Lexer.Tilde -> self#fail_line line Invalid_parameter
    method private params_of_tokens line base l =
      let simple_case () = List.map (self#param_of_token line base) l in
      (* hack for invodynamic *)
      match l with
      | (Lexer.Method_handle mh) :: tl ->
          let tl = List.rev tl in
          (match tl with
          | (Lexer.Dynamic_method (name, desc)) :: tl ->
              let rec fold = function
                | (Lexer.String x) :: tl ->
                    (`String x) :: (fold tl)
                | (Lexer.Class_name x) :: tl ->
                    (`Class x) :: (fold tl)
                | (Lexer.Primitive_type `Int) :: (Lexer.Int x) :: tl ->
                    (`Integer (Int64.to_int32 x)) :: (fold tl)
                | (Lexer.Primitive_type `Long) :: (Lexer.Int x) :: tl ->
                    (`Long x) :: (fold tl)
                | (Lexer.Primitive_type `Float) :: (Lexer.Float x) :: tl ->
                    (`Float x) :: (fold tl)
                | (Lexer.Primitive_type `Double) :: (Lexer.Float x) :: tl ->
                    (`Double x) :: (fold tl)
                | (Lexer.Method_handle x) :: tl ->
                    (`MethodHandle x) :: (fold tl)
                | (Lexer.Method_type x) :: tl ->
                    (`MethodType x) :: (fold tl)
                | [] -> []
                | _ -> self#fail_line line Invalid_parameter in
              [Instruction.Dynamic_method ((mh, (fold tl)), name, desc)]
          | _ -> simple_case ())
      | _ -> simple_case ()
    method private compile_tail line base tail =
      match tail with
      | Empty_tail -> Instruction.No_tail
      | Match_offset_pairs l ->
          Instruction.Match_offset_pairs
            (List.map
               (fun (x, l) ->
                 s4 (Int64.to_int32 x),
                 s4 (match base with
                 | Some b -> Int32.of_int ((self#get_label_offset line l) - b)
                 | None -> 0l))
               l)
      | Offsets l ->
          Instruction.Long_offsets
            (List.map
               (fun l ->
                 match base with
                 | Some b -> s4 (Int32.of_int ((self#get_label_offset line l) - b))
                 | None -> s4 0l)
               l)
    method flush =
      match current_scope with
      | Nothing -> ()
      | Class ->
          List.iter
            self#add_class_attribute
            (self#compile_annotations :> Attribute.for_class list)
      | Field f ->
          class_fields <-
            let attrs =
              (self#compile_annotations :> Attribute.for_field list)
              @ (List.rev f.Field.attributes) in
            { f with Field.attributes = attrs } :: class_fields
      | Method (start_line, m) ->
          if current_pc > max_u2 then self#fail_err Code_too_large;
          let code =
            if optimize && current_code <> [] then begin
              try
                let code = self#compile_instructions in
                let exception_table = self#compile_exceptions in
                let graph =
                  ControlFlow.graph_of_instructions
                    ~line_mapper:(ControlFlow.line_number_table_mapper line_numbers)
                    code
                    exception_table in
                let graph = Code.optimize_graph graph in
                let code, line_numbers, exception_table, graph = Code.flatten_graph graph in
                let max_stack, max_locals, stack_map_frame =
                  let init_state = StackState.make_of_method class_name m in
                  Code.compute_stack_infos
                    (StackState.unify_to_closest_common_parent class_loader [class_name, class_extends])
                    graph
                    init_state in
                [`Code { Attribute.max_stack = max_stack;
                         Attribute.max_locals = max_locals;
                         Attribute.code = code;
                         Attribute.exception_table = exception_table;
                         Attribute.attributes =
                         if line_numbers <> [] then
                           (`LineNumberTable line_numbers) ::
                           (`StackMapTable stack_map_frame) :: (self#compile_attributes false)
                         else
                           (`StackMapTable stack_map_frame) :: (self#compile_attributes false); }]
              with e -> 
                raise (Exception (start_line, (Optimization_error (Printexc.to_string e))))
            end else if current_code <> [] then
              let code = self#compile_instructions in
              let compiled_frames = self#compile_frames in
              let exception_table = self#compile_exceptions in
              let cmp_max_stack, cmp_max_locals, cmp_stack_map_frame =
                if compute_stacks && ((max_stack = None) || (max_locals = None) || (compiled_frames = None)) then
                  let graph = ControlFlow.graph_of_instructions code exception_table in
                  let init_state = StackState.make_of_method class_name m in
                  (try
                    Code.compute_stack_infos
                      (StackState.unify_to_closest_common_parent class_loader [class_name, class_extends])
                      graph
                      init_state
                  with e ->
                    raise (Exception (start_line, (Unable_to_compute_stack_frames (Printexc.to_string e)))))
                else
                  u2 4, u2 4, [] in
              [`Code { Attribute.max_stack = (match max_stack with Some x -> u2 x | None -> cmp_max_stack);
                       Attribute.max_locals = (match max_locals with Some x -> u2 x | None -> cmp_max_locals);
                       Attribute.code = code;
                       Attribute.exception_table = exception_table;
                       Attribute.attributes =
                       (match compiled_frames, cmp_stack_map_frame with
                       | Some l, _ -> (`StackMapTable l) :: (self#compile_attributes true)
                       | None, (_ :: _ ) -> (`StackMapTable cmp_stack_map_frame) :: (self#compile_attributes true)
                       | None, [] -> self#compile_attributes true); }]
            else
              [] in
          let annot = (self#compile_default_annotation :> Attribute.for_method list)
            @ (self#compile_annotations :> Attribute.for_method list)
            @ (self#compile_parameter_annotations :> Attribute.for_method list) in
          let m' = match m with
          | Method.Regular r ->
              Method.Regular { r with Method.attributes = code @ annot @ (List.rev r.Method.attributes) }
          | Method.Constructor c ->
              Method.Constructor { c with Method.cstr_attributes = code @ annot @ (List.rev c.Method.cstr_attributes) }
          | Method.Initializer i ->
              Method.Initializer { i with Method.init_attributes = code @ annot @ (List.rev i.Method.init_attributes) } in
          class_methods <- m' :: class_methods;
          self#clear_code_elements
    method flag_of_token tok =
      match tok with
      | Lexer.Identifier s ->
          (try
            AccessFlag.of_utf8 s
          with
          | AccessFlag.Exception _ ->
              self#fail_err (Invalid_flag (UTF8.to_string s)))
      | _ -> self#fail_err Flag_expected
    method private check_u2 x =
      if x >= 0L && x <= 65535L then
        (Int64.to_int x)
      else
        self#fail_err Constant_out_of_bounds

  end


(* Functions *)

let assemble ?(version=Version.default) ?(compute_stacks=false) ?(optimize=false) ?(class_loader=ClassLoader.make (ClassPath.make_of_list ["."])) ic dst =
  let state = new assembler_state version compute_stacks optimize class_loader in
  let input = UTF8LineReader.make ic in
  let buffer = ref None in
  let read_tokens () =
    state#incr_line;
    match !buffer with
    | Some tokens -> let res = tokens in buffer := None; res
    | None -> Lexer.tokens_of_line (UTF8LineReader.get input) in
  let pushback_tokens tokens =
    if !buffer <> None then
      state#fail_err Internal_pushback
    else
      (state#decr_line; buffer := Some tokens) in
  let rec read_offsets acc =
    try
      let tokens = read_tokens () in
      match tokens with
      | Lexer.Arrow :: (Lexer.Label lbl) :: [] ->
          read_offsets (lbl :: acc)
      | _ ->
          pushback_tokens tokens;
          Offsets (List.rev acc)
    with End_of_file -> Offsets (List.rev acc) in
  let rec read_match_offset_pairs acc =
    try
      let tokens = read_tokens () in
      match tokens with
      | (Lexer.Int x) :: Lexer.Arrow :: (Lexer.Label lbl) :: [] ->
          read_match_offset_pairs ((x, lbl) :: acc)
      | _ ->
          pushback_tokens tokens;
          Match_offset_pairs (List.rev acc)
    with End_of_file -> Match_offset_pairs (List.rev acc) in
  let read_tail () =
    try
      let first = read_tokens () in
      match first with
      | Lexer.Arrow :: (Lexer.Label lbl) :: [] ->
          read_offsets [lbl]
      | (Lexer.Int x) :: Lexer.Arrow :: (Lexer.Label lbl) :: [] ->
          read_match_offset_pairs [(x, lbl)]
      | _ -> pushback_tokens first; Empty_tail
    with End_of_file -> Empty_tail in
  let compile_instr id l =
    if (try UTF8.to_string id with _ -> "") = "wide" then
      match l with
      | (Lexer.Identifier id') :: tl ->
          let tail = read_tail () in
          state#compile_instruction true id' tl tail
      | _ -> state#fail_err Instruction_waited
    else
      let tail = read_tail () in
      state#compile_instruction false id l tail in
  let version_check f v =
    try
      Version.at_least f v version
    with Version.Exception e -> state#fail_err (Version_error e) in
  let check_version_deprecated () =
    version_check "deprecated" Version.Java_1_1 in
  let check_version_synthetic () =
    version_check "synthetic" Version.Java_1_1 in
  let check_version_inner_class () =
    version_check "inner class" Version.Java_1_1 in
  let check_version_enclosing_method () =
    version_check "enclosing method" Version.Java_1_5 in
  let check_version_annotation () =
    version_check "annotation" Version.Java_1_5 in
  let check_version_local_variable_type_table () =
    version_check "local variable type table" Version.Java_1_5 in
  let check_version_signature () =
    version_check "signature" Version.Java_1_5 in
  let check_version_source_debug_extension () =
    version_check "source debug extension" Version.Java_1_5 in
  let check_version_frame () =
    version_check "frame" Version.Java_1_6 in
  let check_version_module () =
    version_check "module" Version.Java_1_8 in
  let continue = ref true in
  while !continue do
    try
      begin
        match read_tokens () with
        | (Lexer.Directive "class") :: l ->
            (match List.rev l with
            | (Lexer.Class_name n) :: flags ->
                state#check_scope_nothing;
                state#set_class_flags flags;
                state#set_class_name n;
                state#set_scope Class
            | _ -> state#fail_err Invalid_class)
        | (Lexer.Directive "extends") :: (Lexer.Class_name n) :: [] ->
            state#check_scope_class;
            state#set_class_extends n
        | (Lexer.Directive "implements") :: (Lexer.Class_name n) :: [] ->
            state#check_scope_class;
            state#add_class_implements n
        | (Lexer.Directive "field") :: l ->
            (match List.rev l with
            | (Lexer.Identifier id) :: t :: flags ->
                state#check_scope_not_nothing;
                state#flush;
                let f = try
                  AccessFlag.check_field_flags
                    state#is_interface
                    (List.map state#flag_of_token flags)
                with AccessFlag.Exception _ -> state#fail_err Invalid_field_flags in
                (try
                  check_version_flags
                    version
                    (f :> AccessFlag.t list)
                with Version.Exception e -> state#fail_err (Version_error e));
                let n = try
                  Name.make_for_field id
                with Name.Exception _ -> state#fail_err Invalid_field_name in
                let d = try
                  Descriptor.filter_void
                    Descriptor.Invalid_field_type
                    (match t with
                    | Lexer.Class_name cn -> `Class cn
                    | Lexer.Array_type at -> Descriptor.java_type_of_external_utf8 at
                    | Lexer.Primitive_type pt -> pt
                    | _ -> state#fail_err Invalid_field_type)
                with
                | Descriptor.Exception _ ->
                    state#fail_err Invalid_field_type in
                let field = {
                  Field.flags = f; 
                  Field.name = n;
                  Field.descriptor = d;
                  Field.attributes = [];
                } in state#set_scope (Field field)
            | _ -> state#fail_err Invalid_field_directive)
        | (Lexer.Directive "method") :: l ->
            (match List.rev l with
            | (Lexer.Method_signature (id, d)) :: r :: flags ->
                state#check_scope_not_nothing;
                state#flush;
                let f = List.map state#flag_of_token flags in
                (try
                  check_version_flags
                    version
                    (f :> AccessFlag.t list)
                with Version.Exception e -> state#fail_err (Version_error e));
                let rt = try
                  (match r with
                  | Lexer.Class_name cn -> `Class cn
                  | Lexer.Array_type at -> Descriptor.java_type_of_external_utf8 at
                  | Lexer.Primitive_type pt -> pt
                  | _ -> state#fail_err Invalid_return_type)
                with
                | Descriptor.Exception _ ->
                    state#fail_err Invalid_return_type in
                if UTF8.equal class_constructor (Name.utf8_for_method id) then
                  (if rt <> `Void then state#fail_err Invalid_constructor_return_type;
                   let f' = try
                     AccessFlag.check_constructor_flags f
                   with AccessFlag.Exception _ -> state#fail_err Invalid_constructor_flags in
                   let mc = { Method.cstr_flags = f'; cstr_descriptor = d; cstr_attributes = [] }in
                   state#set_scope (Method (state#get_line, Method.Constructor mc)))
                else if UTF8.equal class_initializer (Name.utf8_for_method id) then
                  (if (d, rt) <> ([], `Void) then state#fail_err Invalid_initializer_signature;
                   let f' = try
                     AccessFlag.check_initializer_flags f
                   with AccessFlag.Exception _ -> state#fail_err Invalid_constructor_flags in
                   let mi = { Method.init_flags = f'; init_attributes = [] } in
                   state#set_scope (Method (state#get_line, Method.Initializer mi)))
                else
                  let f' = try
                    AccessFlag.check_method_flags state#is_interface f
                  with AccessFlag.Exception _ -> state#fail_err Invalid_method_flags in
                  let mr = { Method.flags = f'; name = id; descriptor = (d, rt); attributes = [] } in
                  state#set_scope (Method (state#get_line, Method.Regular mr))
            | _ -> state#fail_err Invalid_method_directive)
        | (Lexer.Directive "max_stack") :: (Lexer.Int n) :: [] ->
            state#set_max_stack n
        | (Lexer.Directive "max_locals") :: (Lexer.Int n) :: [] ->
            state#set_max_locals n
        | (Lexer.Directive "catch") :: (Lexer.Label start_pc)
          :: (Lexer.Label end_pc) :: (Lexer.Label handler_pc) :: [] ->
            state#add_exception start_pc end_pc handler_pc None
        | (Lexer.Directive "catch") :: (Lexer.Label start_pc)
          :: (Lexer.Label end_pc) :: (Lexer.Label handler_pc)
          :: (Lexer.Class_name cn ) :: [] ->
            state#add_exception start_pc end_pc handler_pc (Some cn)
        | (Lexer.Directive "frame") :: (Lexer.Label l) :: def ->
            check_version_frame ();
            state#add_frame l def
        | (Lexer.Attribute "ConstantValue") :: const_value :: [] ->
            state#add_field_attribute_const const_value
        | (Lexer.Attribute "Exceptions") :: ((_ :: _) as l) ->
            state#add_method_attribute
              (`Exceptions
                 (List.map
                    (function
                      | Lexer.Class_name cn -> cn
                      | _ -> state#fail_err Exception_class_name_waited)
                    l))
        | (Lexer.Attribute "InnerClasses") :: ic :: oc :: n :: l ->
            check_version_inner_class ();
            state#check_scope_class;
            let ic' = match ic with
            | Lexer.Class_name cn -> Some cn
            | Lexer.Int 0L -> None
            | _ -> state#fail_err Invalid_inner_class in
            let oc' = match oc with
            | Lexer.Class_name cn -> Some cn
            | Lexer.Int 0L -> None
            | _ -> state#fail_err Invalid_outer_class in
            let n' = match n with
            | Lexer.Identifier id -> Some id
            | Lexer.Int 0L -> None
            | _ -> state#fail_err Invalid_inner_name in
            let flags = try
              AccessFlag.check_inner_class_flags (List.map state#flag_of_token l)
            with AccessFlag.Exception _ -> state#fail_err Invalid_inner_class_flags in
            state#add_class_attribute_inner
              { Attribute.inner_class = ic';
                Attribute.outer_class = oc';
                Attribute.inner_name = n';
                Attribute.inner_flags = flags }
        | (Lexer.Attribute "EnclosingMethod") :: (Lexer.Class_name cn) :: m :: [] ->
            check_version_enclosing_method ();
            let m' = match m with
            | Lexer.Dynamic_method (x, y) -> Some (x, y)
            | Lexer.Int 0L -> None
            | _ -> state#fail_err Invalid_method in
            state#add_class_attribute (`EnclosingMethod { Attribute.innermost_class = cn;
                                                          Attribute.enclosing_method = m' })
        | (Lexer.Attribute "Synthetic") :: [] ->
            check_version_synthetic ();
            (match state#get_scope with
            | Class -> state#add_class_attribute `Synthetic
            | Field _ -> state#add_field_attribute `Synthetic
            | Method _ -> state#add_method_attribute `Synthetic
            | Nothing -> state#fail_scope)
        | (Lexer.Attribute "Signature") :: (Lexer.String s) :: [] ->
            check_version_signature ();
            (match state#get_scope with
            | Class ->
                let s' = try
                  Signature.class_signature_of_utf8 s
                with _ -> state#fail_err Invalid_class_signature in
                state#add_class_attribute (`Signature (`Class s'))
            | Field _ ->
                let s' = try
                  Signature.field_type_signature_of_utf8 s
                with _ -> state#fail_err Invalid_field_signature in
                state#add_field_attribute (`Signature (`Field s'))
            | Method _ ->
                let s' = try
                  Signature.method_signature_of_utf8 s
                with _ -> state#fail_err Invalid_method_signature in
                state#add_method_attribute (`Signature (`Method s'))
            | Nothing -> state#fail_scope)
        | (Lexer.Attribute "SourceFile") :: (Lexer.String s) :: [] ->
            state#add_class_attribute (`SourceFile s)
        | (Lexer.Attribute "SourceDebugExtension") :: (Lexer.String s) :: [] ->
            check_version_source_debug_extension ();
            state#add_class_attribute (`SourceDebugExtension s)
        | (Lexer.Attribute "Deprecated") :: [] ->
            check_version_deprecated ();
            (match state#get_scope with
            | Class -> state#add_class_attribute `Deprecated
            | Field _ -> state#add_field_attribute `Deprecated
            | Method _ -> state#add_method_attribute `Deprecated
            | Nothing -> state#fail_scope)
        | (Lexer.Attribute "RuntimeVisibleAnnotations")
          :: (Lexer.Class_name name) :: l ->
            check_version_annotation ();
            state#check_scope_not_nothing;
            state#add_annotation
              true
              (Name.external_utf8_for_class name)
              l
        | (Lexer.Attribute "RuntimeInvisibleAnnotations")
          :: (Lexer.Class_name name) :: l ->
            check_version_annotation ();
            state#check_scope_not_nothing;
            state#add_annotation
              false
              (Name.external_utf8_for_class name)
              l
        | (Lexer.Attribute "RuntimeVisibleParameterAnnotations")
          :: (Lexer.Int n) :: (Lexer.Class_name name) :: l ->
            check_version_annotation ();
            state#check_scope_method;
            state#add_parameter_annotation
              true
              (Int64.to_int n)
              (Name.external_utf8_for_class name)
              l
        | (Lexer.Attribute "RuntimeInvisibleParameterAnnotations")
          :: (Lexer.Int n) :: (Lexer.Class_name name) :: l ->
            check_version_annotation ();
            state#check_scope_method;
            state#add_parameter_annotation
              false
              (Int64.to_int n)
              (Name.external_utf8_for_class name)
              l
        | (Lexer.Attribute "AnnotationDefault") :: l ->
            check_version_annotation ();
            state#check_scope_method;
            state#add_default_annotation l
        | (Lexer.Attribute "LineNumberTable") :: (Lexer.Int l) :: [] ->
            state#add_line_number (Some l)
        | (Lexer.Attribute "LineNumberTable") :: [] ->
            state#add_line_number None
        | (Lexer.Attribute "LocalVariableTable")
          :: (Lexer.Label start) :: (Lexer.Label finish)
          :: (Lexer.Identifier id) :: t :: (Lexer.Int idx) :: [] ->
            let desc = try
              Descriptor.filter_void
                Descriptor.Invalid_local_variable_type
                (match t with
                | Lexer.Class_name cn -> `Class cn
                | Lexer.Array_type at -> Descriptor.java_type_of_external_utf8 at
                | Lexer.Primitive_type pt -> pt
                | _ -> state#fail_err Invalid_local_variable_type)
                with
            | Name.Exception _
            | Descriptor.Exception _ -> state#fail_err Invalid_local_variable_type in
            state#add_local_variable start finish id desc idx
        | (Lexer.Attribute "LocalVariableTypeTable")
          :: (Lexer.Label start) :: (Lexer.Label finish)
          :: (Lexer.Identifier id) :: (Lexer.String s) :: (Lexer.Int idx) :: [] ->
            check_version_local_variable_type_table ();
            let sign = try
              Signature.field_type_signature_of_utf8 s
            with Signature.Exception _ -> state#fail_err Invalid_local_variable_signature in
            state#add_local_variable_type start finish id sign idx
        | (Lexer.Attribute "Module") :: (Lexer.String n) :: (Lexer.String v) :: [] ->
            check_version_module ();
            state#add_class_attribute (`Module (n, v))
        | (Lexer.Attribute "Unknown") :: (Lexer.String n) :: (Lexer.String v) :: [] ->
            let v' = UTF8.to_string v in
            (match state#get_scope with
            | Class -> state#add_class_attribute (`Unknown (n, v'))
            | Field _ -> state#add_field_attribute (`Unknown (n, v'))
            | Method _ ->
                if state#is_code_empty then
                  state#add_method_attribute (`Unknown (n, v'))
                else
                  state#add_code_attribute (`Unknown (n, v'))
            | Nothing -> state#fail_scope)
        | (Lexer.Attribute attr) :: _ ->
            if List.mem attr [
              "ConstantValue" ;
              "Exceptions" ;
              "InnerClasses" ;
              "EnclosingMethod" ;
              "Synthetic" ;
              "Signature" ;
              "SourceFile" ;
              "SourceDebugExtension" ;
              "Deprecated" ;
              "RuntimeVisibleAnnotations" ;
              "RuntimeInvisibleAnnotations" ;
              "RuntimeVisibleParameterAnnotations" ;
              "RuntimeInvisibleParameterAnnotations" ;
              "AnnotationDefault" ;
              "LineNumberTable" ;
              "LocalVariableTable" ;
              "LocalVariableTypeTable" ;
              "Module" ;
              "Unknown"
            ] then
              state#fail_err Invalid_attribute_arguments
            else
              state#fail_err Unknown_attribute
        | (Lexer.Directive dir) :: _ ->
            if List.mem dir [
              "class" ;
              "extends" ;
              "implements" ;
              "field" ;
              "method" ;
              "max_stack" ;
              "max_locals" ;
              "catch" ;
              "frame"
            ] then
              state#fail_err Invalid_directive_arguments
            else
              state#fail_err Invalid_directive
        | (Lexer.Label lbl) :: [] ->
            state#check_scope_method;
            state#add_label lbl
        | (Lexer.Label lbl) :: (Lexer.Identifier id) :: l ->
            state#check_scope_method;
            state#add_label lbl;
            compile_instr id l
        | (Lexer.Identifier id) :: l ->
            state#check_scope_method;
            compile_instr id l
        | [] -> ()
        | _ -> state#fail_err Syntax_error
      end
    with
    | (Exception _) as e ->
        raise e
    | Lexer.Exception e ->
        raise (Exception (state#get_line, (Lexer_error e)))
    | ClassDefinition.Exception e ->
        raise (Exception (state#get_line, (ClassDefinition_error e)))
    | ClassFile.Exception e ->
        raise (Exception (state#get_line, (ClassFile_error e)))
    | Method.Exception e ->
        raise (Exception (state#get_line, (Method_error e)))
    | Field.Exception e ->
        raise (Exception (state#get_line, (Field_error e)))
    | Attribute.Exception e ->
        raise (Exception (state#get_line, (Attribute_error e)))
    | Instruction.Exception e ->
        raise (Exception (state#get_line, (Instruction_error e)))
    | ByteCode.Exception e ->
        raise (Exception (state#get_line, (ByteCode_error e)))
    | Annotation.Exception e ->
        raise (Exception (state#get_line, (Annotation_error e)))
    | AccessFlag.Exception e ->
        raise (Exception (state#get_line, (AccessFlag_error e)))
    | ConstantPool.Exception e ->
        raise (Exception (state#get_line, (ConstantPool_error e)))
    | Signature.Exception e ->
        raise (Exception (state#get_line, (Signature_error e)))
    | Descriptor.Exception e ->
        raise (Exception (state#get_line, (Descriptor_error e)))
    | Name.Exception e ->
        raise (Exception (state#get_line, (Name_error e)))
    | End_of_file ->
        continue := false
    | _ ->
        raise (Exception (state#get_line, Syntax_error))
  done;
  state#flush;
  state#write_class dst;
  UTF8LineReader.close input;
  state#get_class_name

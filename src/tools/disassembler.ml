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


(* Constants *)

let dot_class = UTF8.of_string ".class "

let dot_method = UTF8.of_string ".method "

let dot_field = UTF8.of_string ".field "

let dot_implements = UTF8.of_string ".implements "

let dot_extends = UTF8.of_string ".extends "

let space = UTF8.of_string " "

let tab = UTF8.of_string "  "

let colon = UTF8.of_string ":"

let comma = UTF8.of_string ","

let dot = UTF8.of_string "."

let opening_parenthesis = UTF8.of_string "("

let closing_parenthesis = UTF8.of_string ")"

let tilde = UTF8.of_string " ~ "

let zero = UTF8.of_string "0"

let wide = UTF8.of_string "wide "

let at_constant_value = UTF8.of_string "@ConstantValue "

let at_module = UTF8.of_string "@Module "

let at_unknown = UTF8.of_string "@Unknown "

let at_exceptions = UTF8.of_string "@Exceptions "

let at_inner_class = UTF8.of_string "@InnerClass "

let at_enclosing_method = UTF8.of_string "@EnclosingMethod "

let at_synthetic = UTF8.of_string "@Synthetic"

let at_signature = UTF8.of_string "@Signature "

let at_source_file = UTF8.of_string "@SourceFile "

let at_source_debug_extension = UTF8.of_string "@SourceDebugExtension "

let at_deprecated = UTF8.of_string "@Deprecated"

let at_runtime_visible_annotations = UTF8.of_string "@RuntimeVisibleAnnotations "

let at_runtime_invisible_annotations = UTF8.of_string "@RuntimeInvisibleAnnotations "

let at_annotation_default = UTF8.of_string "@AnnotationDefault"

let class_constructor_name = Name.make_for_method class_constructor

let class_initializer_name = Name.make_for_method class_initializer


(* Functions *)

let utf8_of_method_desc name desc =
  let params, return = desc in
  (Descriptor.external_utf8_of_java_type return)
    ++ space
    ++ (Name.utf8_for_method name)
    ++ opening_parenthesis
    ++ (UTF8.concat_sep_map comma Descriptor.external_utf8_of_java_type (params :> Descriptor.java_type list))
    ++ closing_parenthesis

let utf8_of_method_call name desc =
  let params, return = desc in
  (Name.utf8_for_method name)
    ++ opening_parenthesis
    ++ (UTF8.concat_sep_map comma Descriptor.external_utf8_of_java_type (params :> Descriptor.java_type list))
    ++ closing_parenthesis
    ++ colon
    ++ (Descriptor.external_utf8_of_java_type return)

let extract_code_attributes l =
  let rec extract lnt lvt lvtt st unk l =
    match l with
    | hd :: tl ->
        (match hd with
        | `LineNumberTable l -> extract (lnt @ l) lvt lvtt st unk tl
        | `LocalVariableTable l -> extract lnt (lvt @ l) lvtt st unk tl
        | `LocalVariableTypeTable l -> extract lnt lvt (lvtt @ l) st unk tl
        | `StackMapTable l -> extract lnt lvt lvtt l unk tl
        | `Unknown (n, s) -> extract lnt lvt lvtt st ((n, s) :: unk) tl)
    | [] ->
        ((List.sort (fun (x, _) (y, _) -> compare x y) lnt),
         (List.sort (fun { Attribute.local_start = x; _ } { Attribute.local_start = y; _ } -> compare x y) lvt),
         (List.sort (fun { Attribute.local_type_start = x; _ } { Attribute.local_type_start = y; _ } -> compare x y) lvtt),
         st,
         (List.rev unk)) in
  extract [] [] [] [] [] l

let rec annotation_value_list prefix = function
  | Annotation.Boolean_value b ->
      if b then
        [prefix ++ (UTF8.of_string " boolean 1")]
      else
        [prefix ++ (UTF8.of_string " boolean 0")]
  | Annotation.Byte_value b ->
      [prefix ++ (UTF8.of_string (Printf.sprintf " byte %d" b))]
  | Annotation.Char_value c ->
      [prefix ++ (UTF8.of_string (Printf.sprintf " char %d" (UChar.to_code c)))]
  | Annotation.Double_value d ->
      [prefix ++ (UTF8.of_string (Printf.sprintf " double %f" d))]
  | Annotation.Float_value f ->
      [prefix ++ (UTF8.of_string (Printf.sprintf " float %f" f))]
  | Annotation.Int_value i ->
      [prefix ++ (UTF8.of_string (Printf.sprintf " int %ld" i))]
  | Annotation.Long_value l ->
      [prefix ++ (UTF8.of_string (Printf.sprintf " long %Ld" l))]
  | Annotation.Short_value s ->
      [prefix ++ (UTF8.of_string (Printf.sprintf " short %d" s))]
  | Annotation.String_value s ->
      [prefix ++ (UTF8.of_string " string ") ++ (UTF8.escape s)]
  | Annotation.Enum_value (cn, fn) ->
      [prefix
         ++ (UTF8.of_string " enum ")
         ++ (Name.external_utf8_for_class cn)
         ++ space
         ++ (Name.utf8_for_field fn)]
  | Annotation.Class_value cn ->
      [prefix ++ (UTF8.of_string " class ") ++ (Name.external_utf8_for_class cn)]
  | Annotation.Annotation_value a ->
      let prefix' = prefix ++ (UTF8.of_string " annotation") in
      annotation_list prefix' a
  | Annotation.Array_value l ->
      let idx = ref 0 in
      List.flatten
        (List.map
           (fun x ->
             let prefix' = prefix
                 ++ space
                 ++ (UTF8.of_string (string_of_int !idx)) in
             incr idx;
             annotation_value_list prefix' x)
           l)
and annotation_list prefix (name, pairs) =
  let prefix' = prefix
      ++ (Name.external_utf8_for_class name)
      ++ space in
  if pairs = [] then
    [prefix']
  else
    List.flatten
      (List.map
         (fun (id, vl) ->
           annotation_value_list (prefix' ++ id) vl)
         pairs)

let add_attribute buffer a =
  UTF8Buffer.add_string buffer tab;
  match a with
  | `ConstantValue cv ->
      UTF8Buffer.add_string buffer at_constant_value;
      (match cv with
      | Attribute.Long_value lv ->
          UTF8Buffer.add_endline buffer (UTF8.of_string (Int64.to_string lv))
      | Attribute.Float_value fv ->
          UTF8Buffer.add_endline buffer (UTF8.of_string (string_of_float fv))
      | Attribute.Double_value dv ->
          UTF8Buffer.add_endline buffer (UTF8.of_string (string_of_float dv))
      | Attribute.Boolean_value bv ->
          UTF8Buffer.add_endline
            buffer
            (UTF8.of_string (if bv then "1" else "0"))
      | Attribute.Byte_value bv ->
          UTF8Buffer.add_endline buffer (UTF8.of_string (string_of_int bv))
      | Attribute.Character_value cv ->
          UTF8Buffer.add_endline buffer (UTF8.of_string (string_of_int cv))
      | Attribute.Short_value sv ->
          UTF8Buffer.add_endline buffer (UTF8.of_string (string_of_int sv))
      | Attribute.Integer_value iv ->
          UTF8Buffer.add_endline buffer (UTF8.of_string (Int32.to_string iv))
      | Attribute.String_value sv ->
          UTF8Buffer.add_endline
            buffer
            (UTF8.escape sv))
  | `Code cv ->
      let line_number_table, local_variable_table, local_variable_type_table, stack_map_table, unknowns =
        extract_code_attributes cv.Attribute.attributes in
      let prev = ref (-1) in
      let ofs = ref 0 in
      UTF8Buffer.add_endline
        buffer
        (UTF8.of_string
           (Printf.sprintf ".max_stack %d" (cv.Attribute.max_stack :> int)));
      UTF8Buffer.add_endline
        buffer
        (UTF8.of_string
           (Printf.sprintf "  .max_locals %d" (cv.Attribute.max_locals :> int)));
      List.iter
        (fun i ->
          (try
            let line = snd (List.find (fun ((x : u2), _) -> !prev < (x :> int) && !ofs >= (x :> int)) line_number_table) in
            UTF8Buffer.add_endline buffer (UTF8.of_string (Printf.sprintf "  @LineNumberTable %d" (line :> int)));
          with Not_found -> ());
          List.iter
            (fun { Attribute.local_start; local_length; local_name; local_descriptor; local_index } ->
              let start = (local_start : u2 :> int) in
              let length = (local_length : u2 :> int) in
              let index = (local_index : u2 :> int) in
              let finish = start + length in
              if (!ofs >= start && !ofs < finish)
                  && not (!prev >= start && !prev < finish) then
                begin
                  UTF8Buffer.add_string buffer (UTF8.of_string (Printf.sprintf "  @LocalVariableTable code%08d: code%08d: " start finish));
                  UTF8Buffer.add_string buffer local_name;
                  UTF8Buffer.add_string buffer space;
                  UTF8Buffer.add_string buffer (Descriptor.external_utf8_of_java_type (local_descriptor :> Descriptor.java_type));
                  UTF8Buffer.add_endline buffer (UTF8.of_string (Printf.sprintf " %d" index))
                end)
            local_variable_table;
          List.iter
            (fun { Attribute.local_type_start; local_type_length; local_type_name; local_type_signature; local_type_index } ->
              let start = (local_type_start : u2 :> int) in
              let length = (local_type_length : u2 :> int) in
              let index = (local_type_index : u2 :> int) in
              let finish = start + length in
              if (!ofs >= start && !ofs < finish)
                  && not (!prev >= start && !prev < finish) then
                begin
                  UTF8Buffer.add_string buffer (UTF8.of_string (Printf.sprintf "  @LocalVariableTypeTable code%08d: code%08d: " start finish));
                  UTF8Buffer.add_string buffer local_type_name;
                  UTF8Buffer.add_string buffer space;
                  UTF8Buffer.add_string buffer (UTF8.escape (Signature.utf8_of_field_type_signature local_type_signature));
                  UTF8Buffer.add_endline buffer (UTF8.of_string (Printf.sprintf " %d" index))
                end)
            local_variable_type_table;
          let sz, is_wide, mnemo, p, t = Instruction.decompile !ofs i in
          UTF8Buffer.add_string buffer (UTF8.of_string (Printf.sprintf "  code%08d: " !ofs));
          (if is_wide then UTF8Buffer.add_string buffer wide);
          UTF8Buffer.add_string buffer (UTF8.of_string mnemo);
          let add_field (cn, fn, d) =
            UTF8Buffer.add_string
              buffer
              ((Name.external_utf8_for_class cn)
                 ++ dot
                 ++ (Name.utf8_for_field fn)
                 ++ colon
                 ++ (Descriptor.external_utf8_of_java_type (d :> Descriptor.java_type))) in
          let add_method (cn, mn, d) =
            UTF8Buffer.add_string
              buffer
              ((Name.external_utf8_for_class cn)
                 ++ dot
                 ++ (utf8_of_method_call mn d)) in
          let add_method_handle = function
            | `getField (cn, fn, d) ->
                UTF8Buffer.add_string buffer (UTF8.of_string "getField%");
                add_field (cn, fn, d)
            | `getStatic (cn, fn, d) ->
                UTF8Buffer.add_string buffer (UTF8.of_string "getStatic%");
                add_field (cn, fn, d)
            | `putField (cn, fn, d) ->
                UTF8Buffer.add_string buffer (UTF8.of_string "putField%");
                add_field (cn, fn, d)
            | `putStatic (cn, fn, d) ->
                UTF8Buffer.add_string buffer (UTF8.of_string "putStatic%");
                add_field (cn, fn, d)
            | `invokeVirtual (cn, mn, d) ->
                UTF8Buffer.add_string buffer (UTF8.of_string "invokeVirtual%");
                add_method (cn, mn, d)
            | `invokeStatic (cn, mn, d) ->
                UTF8Buffer.add_string buffer (UTF8.of_string "invokeStatic%");
                add_method (cn, mn, d)
            | `invokeSpecial (cn, mn, d) ->
                UTF8Buffer.add_string buffer (UTF8.of_string "invokeSpecial%");
                add_method (cn, mn, d)
            | `newInvokeSpecial (cn, d) ->
                UTF8Buffer.add_string buffer (UTF8.of_string "newInvokeSpecial%");
                add_method (cn, (Name.make_for_method class_constructor), (d, `Class cn))
            | `invokeInterface (cn, mn, d) ->
                UTF8Buffer.add_string buffer (UTF8.of_string "invokeInterface%");
                add_method (cn, mn, d) in
          let add_method_type (params, return) =
            UTF8Buffer.add_string
              buffer
              (opening_parenthesis
                 ++ (UTF8.concat_sep_map comma Descriptor.external_utf8_of_java_type (params :> Descriptor.java_type list))
                 ++ closing_parenthesis
                 ++ colon
                 ++ (Descriptor.external_utf8_of_java_type return)) in
          List.iter (fun x ->
            UTF8Buffer.add_string buffer space;
            match x with
            | Instruction.Int_constant ic ->
                UTF8Buffer.add_string buffer (UTF8.of_string (Int64.to_string ic))
            | Instruction.Offset o ->
                UTF8Buffer.add_string buffer (UTF8.of_string (Printf.sprintf "code%08d:" (!ofs + (Int32.to_int o))))
            | Instruction.Float_constant fc ->
                UTF8Buffer.add_string buffer (UTF8.of_string (string_of_float fc))
            | Instruction.String_constant sc ->
                UTF8Buffer.add_string buffer (UTF8.escape sc)
            | Instruction.Class_name cn ->
                UTF8Buffer.add_string buffer (Name.external_utf8_for_class cn)
            | Instruction.Array_type at ->
                UTF8Buffer.add_string buffer at
            | Instruction.Primitive_type pt ->
                UTF8Buffer.add_string buffer (Descriptor.external_utf8_of_java_type pt)
            | Instruction.Field (cn, fn, d) ->
                add_field (cn, fn, d)
            | Instruction.Dynamic_method ((mh, args), mn, d) ->
                add_method_handle mh;
                List.iter
                  (fun arg ->
                    UTF8Buffer.add_string buffer space;
                    match arg with
                    | `String s ->
                        UTF8Buffer.add_string buffer (UTF8.escape s)
                    | `Class cn ->
                        UTF8Buffer.add_string buffer (Name.external_utf8_for_class cn)
                    | `Integer i ->
                        UTF8Buffer.add_string buffer (UTF8.of_string "int ");
                        UTF8Buffer.add_string buffer (UTF8.of_string (Int32.to_string i))
                    | `Long l ->
                        UTF8Buffer.add_string buffer (UTF8.of_string "long ");
                        UTF8Buffer.add_string buffer (UTF8.of_string (Int64.to_string l))
                    | `Float f ->
                        UTF8Buffer.add_string buffer (UTF8.of_string "float ");
                        UTF8Buffer.add_string buffer (UTF8.of_string (string_of_float f))
                    | `Double d ->
                        UTF8Buffer.add_string buffer (UTF8.of_string "double ");
                        UTF8Buffer.add_string buffer (UTF8.of_string (string_of_float d))
                    | `MethodHandle mh ->
                        add_method_handle mh
                    | `MethodType mt ->
                        add_method_type mt)
                  args;
                UTF8Buffer.add_string buffer space;
                UTF8Buffer.add_string buffer (utf8_of_method_call mn d)
            | Instruction.Method (cn, mn, d) ->
                add_method (cn, mn, d)
            | Instruction.Array_method (at, mn, d) ->
                UTF8Buffer.add_string
                  buffer
                  ((Descriptor.external_utf8_of_java_type (at :> Descriptor.java_type))
                     ++ dot
                     ++ (utf8_of_method_call mn d))
            | Instruction.Method_type_constant (params, return) ->
                add_method_type (params, return)
            | Instruction.Method_handle_constant mh ->
                add_method_handle mh)
            p;
          (match t with
          | Instruction.No_tail -> ()
          | Instruction.Match_offset_pairs l ->
              List.iter
                (fun (m, o) ->
                  let m = (m : s4 :> int32) in
                  let o = (o : Instruction.long_offset :> int32) in
                  UTF8Buffer.add_endline buffer empty_utf8;
                  UTF8Buffer.add_string
                    buffer
                    (UTF8.of_string
                       (Printf.sprintf "    %ld => code%08d:" m (!ofs + (Int32.to_int o)))))
                l
          | Instruction.Long_offsets l ->
              List.iter
                (fun o ->
                  let o = (o : Instruction.long_offset :> int32) in
                  UTF8Buffer.add_endline buffer empty_utf8;
                  UTF8Buffer.add_string buffer (UTF8.of_string (Printf.sprintf "    => code%08d:" (!ofs + (Int32.to_int o)))))
                l);
          UTF8Buffer.add_endline buffer empty_utf8;
          prev := !ofs;
          ofs := !ofs + sz)
        cv.Attribute.code;
      List.iter
        (fun elem ->
          let start_pc = (elem.Attribute.try_start : u2 :> int) in
          let end_pc = (elem.Attribute.try_end : u2 :> int) in
          let handler_pc = (elem.Attribute.catch : u2 :> int) in
          UTF8Buffer.add_string
            buffer
            (UTF8.of_string (Printf.sprintf "  .catch code%08d: code%08d: code%08d:" start_pc end_pc handler_pc));
          UTF8Buffer.add_endline
            buffer
            (match elem.Attribute.caught with
            | Some n -> space ++ (Name.external_utf8_for_class n)
            | None -> empty_utf8))
        cv.Attribute.exception_table;
      let utf8_of_type = function
        | Attribute.Top_variable_info -> UTF8.of_string "top"
        | Attribute.Integer_variable_info -> UTF8.of_string "int"
        | Attribute.Float_variable_info -> UTF8.of_string "float"
        | Attribute.Long_variable_info -> UTF8.of_string "long"
        | Attribute.Double_variable_info -> UTF8.of_string "double"
        | Attribute.Null_variable_info -> UTF8.of_string "null"
        | Attribute.Uninitialized_this_variable_info -> UTF8.of_string "uninit_this"
        | Attribute.Object_variable_info (`Class_or_interface n) -> Name.external_utf8_for_class n
        | Attribute.Object_variable_info (`Array_type at) -> Descriptor.external_utf8_of_java_type (at :> Descriptor.java_type)
        | Attribute.Uninitialized_variable_info ofs -> UTF8.of_string (Printf.sprintf "uninit code%08d:" (ofs :> int)) in
      List.iter
        (function
          | Attribute.Same_frame ofs ->
              UTF8Buffer.add_endline
                buffer
                (UTF8.of_string (Printf.sprintf "  .frame code%08d: same" (ofs :> int)))
          | Attribute.Same_locals_1_stack_item_frame (ofs, t) ->
              UTF8Buffer.add_endline
                buffer
                ((UTF8.of_string (Printf.sprintf "  .frame code%08d: same_locals " (ofs :> int)))
                    ++ (utf8_of_type t))
          | Attribute.Chop_1_frame ofs ->
              UTF8Buffer.add_endline
                buffer
                (UTF8.of_string (Printf.sprintf "  .frame code%08d: chop 1" (ofs :> int)))
          | Attribute.Chop_2_frame ofs ->
              UTF8Buffer.add_endline
                buffer
                (UTF8.of_string (Printf.sprintf "  .frame code%08d: chop 2" (ofs :> int)))
          | Attribute.Chop_3_frame ofs ->
              UTF8Buffer.add_endline
                buffer
                (UTF8.of_string (Printf.sprintf "  .frame code%08d: chop 3" (ofs :> int)))
          | Attribute.Append_1_frame (ofs, t1) ->
              UTF8Buffer.add_endline
                buffer
                ((UTF8.of_string (Printf.sprintf "  .frame code%08d: append " (ofs :> int)))
                   ++ (utf8_of_type t1))
          | Attribute.Append_2_frame (ofs, t1, t2) ->
              UTF8Buffer.add_endline
                buffer
                ((UTF8.of_string (Printf.sprintf "  .frame code%08d: append " (ofs :> int)))
                   ++ (utf8_of_type t1) ++ space ++ (utf8_of_type t2))
          | Attribute.Append_3_frame (ofs, t1, t2, t3) ->
              UTF8Buffer.add_endline
                buffer
                ((UTF8.of_string (Printf.sprintf "  .frame code%08d: append " (ofs :> int)))
                   ++ (utf8_of_type t1) ++ space ++ (utf8_of_type t2) ++ space ++ (utf8_of_type t3))
          | Attribute.Full_frame (ofs, l1, l2) ->
              let utf8_of_type_list l = UTF8.concat_sep_map space utf8_of_type l in
              UTF8Buffer.add_endline
                buffer
                ((UTF8.of_string (Printf.sprintf "  .frame code%08d: full " (ofs :> int)))
                   ++ (utf8_of_type_list l1) ++ tilde ++ (utf8_of_type_list l2)))
        stack_map_table;
      List.iter
        (fun (n, s) ->
          UTF8Buffer.add_string buffer at_unknown;
          UTF8Buffer.add_string buffer (UTF8.escape n);
          UTF8Buffer.add_string buffer space;
          UTF8Buffer.add_endline buffer (UTF8.escape (UTF8.of_string s)))
        unknowns
  | `Exceptions e ->
      UTF8Buffer.add_endline
        buffer
        (at_exceptions
           ++ (UTF8.concat_sep_map
                 space
                 Name.external_utf8_for_class e))
  | `InnerClasses l ->
      let l' =
        List.map
          (function { Attribute.inner_class; outer_class; inner_name; inner_flags } ->
            at_inner_class
              ++ (match inner_class with
              | Some cn -> Name.external_utf8_for_class cn
              | None -> zero)
              ++ space
              ++ (match outer_class with
              | Some cn -> Name.external_utf8_for_class cn
              | None -> zero)
              ++ space
              ++ (match inner_name with
              | Some n -> n
              | None -> zero)
              ++ space
              ++ (AccessFlag.list_to_utf8 (inner_flags :> AccessFlag.t list)))
          l in
      UTF8Buffer.add_endline
        buffer
        (UTF8.concat_sep (UTF8.of_string "\n  ") l')
  | `EnclosingMethod { Attribute.innermost_class; enclosing_method } ->
      UTF8Buffer.add_string buffer at_enclosing_method;
      UTF8Buffer.add_string buffer (Name.external_utf8_for_class innermost_class);
      (match enclosing_method with
      | Some (n, d) ->
          UTF8Buffer.add_string buffer space;
          UTF8Buffer.add_endline buffer (utf8_of_method_call n d)
      | None -> UTF8Buffer.add_endline buffer empty_utf8)
  | `Synthetic ->
      UTF8Buffer.add_endline buffer at_synthetic
  | `Signature s ->
      let s' = (match s with
      | `Class cs -> Signature.utf8_of_class_signature cs
      | `Method ms -> Signature.utf8_of_method_signature ms
      | `Field fs -> Signature.utf8_of_field_type_signature fs) in
      UTF8Buffer.add_endline
        buffer
        (at_signature ++ (UTF8.escape s'))
  | `SourceFile sf ->
      UTF8Buffer.add_endline
        buffer
        (at_source_file ++ (UTF8.escape sf))
  | `SourceDebugExtension sde ->
      UTF8Buffer.add_endline
        buffer
        (at_source_debug_extension ++ (UTF8.escape sde))
  | `LineNumberTable _ -> ()
  | `LocalVariableTable _ -> ()
  | `LocalVariableTypeTable _ -> ()
  | `Deprecated ->
      UTF8Buffer.add_endline buffer at_deprecated
  | `RuntimeVisibleAnnotations l ->
      let l' = List.flatten
          (List.map
             (annotation_list at_runtime_visible_annotations)
             l) in
      UTF8Buffer.add_endline buffer (UTF8.concat_sep (UTF8.of_string "\n  ") l')
  | `RuntimeInvisibleAnnotations l ->
      let l' = List.flatten
          (List.map
             (annotation_list at_runtime_invisible_annotations)
             l) in
      UTF8Buffer.add_endline buffer (UTF8.concat_sep (UTF8.of_string "\n  ") l')
  | `RuntimeVisibleParameterAnnotations l ->
      let no = ref 0 in
      List.iter
        (fun x ->
          let l' = List.flatten
              (List.map
                 (annotation_list (UTF8.of_string ((if !no = 0 then "" else "  ") ^ "@RuntimeVisibleParameterAnnotations " ^ (string_of_int !no))))
                 x) in
          UTF8Buffer.add_endline buffer (UTF8.concat_sep (UTF8.of_string "\n  ") l');
          incr no)
        l
  | `RuntimeInvisibleParameterAnnotations l ->
      let no = ref 0 in
      List.iter
        (fun x ->
          let l' = List.flatten
              (List.map
                 (annotation_list (UTF8.of_string ((if !no = 0 then "" else "  ") ^ "@RuntimeInvisibleParameterAnnotations " ^ (string_of_int !no))))
                 x) in
          UTF8Buffer.add_endline buffer (UTF8.concat_sep (UTF8.of_string "\n  ") l');
          incr no)
        l
  | `RuntimeVisibleTypeAnnotations _ -> ()
  | `RuntimeInvisibleTypeAnnotations _ -> ()
  | `AnnotationDefault ad ->
      let l = annotation_value_list at_annotation_default ad in
      UTF8Buffer.add_endline buffer (UTF8.concat_sep (UTF8.of_string "\n  ") l)
  | `StackMapTable _ -> ()
  | `BootstrapMethods _ -> ()
  | `Module (mn, mv) ->
      UTF8Buffer.add_endline
        buffer
        (at_module ++ (UTF8.escape mn) ++ space ++ (UTF8.escape mv))
  | `ModuleRequires _ -> ()
  | `ModulePermits _ -> ()
  | `ModuleProvides _ -> ()
  | `Unknown (n, s) ->
      UTF8Buffer.add_string buffer at_unknown;
      UTF8Buffer.add_string buffer (UTF8.escape n);
      UTF8Buffer.add_string buffer space;
      UTF8Buffer.add_endline buffer (UTF8.escape (UTF8.of_string s))

let disassemble_to_buffer buffer cp s =
  let cl = ClassLoader.make cp in
  let cd = ClassLoader.find_class cl s in
  UTF8Buffer.add_endline
    buffer
    (dot_class
       ++ (AccessFlag.list_to_utf8 (cd.ClassDefinition.access_flags :> AccessFlag.t list))
       ++ (Name.external_utf8_for_class cd.ClassDefinition.name));
  (match cd.ClassDefinition.extends with
  | Some c ->
      UTF8Buffer.add_endline
        buffer
        (dot_extends ++ (Name.external_utf8_for_class c))
  | None -> ());
  let interfaces = List.map Name.external_utf8_for_class cd.ClassDefinition.implements in
  List.iter
    (fun x ->
      UTF8Buffer.add_endline
        buffer
        (dot_implements ++ x))
    (List.sort UTF8.compare interfaces);
  List.iter
    (add_attribute buffer)
    (List.sort
       Attribute.compare
       (cd.ClassDefinition.attributes :> Attribute.t list));
  if cd.ClassDefinition.fields <> [] then
    UTF8Buffer.add_endline buffer empty_utf8;
  List.iter
    (fun f ->
      UTF8Buffer.add_endline buffer empty_utf8;
      UTF8Buffer.add_endline
        buffer
        (dot_field
           ++ (AccessFlag.list_to_utf8 (List.sort AccessFlag.compare (f.Field.flags :> AccessFlag.t list)))
           ++ (Descriptor.external_utf8_of_java_type (f.Field.descriptor :> Descriptor.java_type))
           ++ space
           ++ (Name.utf8_for_field f.Field.name));
      List.iter
        (add_attribute buffer)
          (List.sort
             Attribute.compare
             (f.Field.attributes :> Attribute.t list)))
    (List.sort Field.compare cd.ClassDefinition.fields);
  if cd.ClassDefinition.methods <> [] then
    UTF8Buffer.add_endline buffer empty_utf8;
  List.iter
    (fun x ->
      let flags, name, desc, attrs =
        match x with
        | Method.Regular mr ->
            mr.Method.flags,
            mr.Method.name,
            mr.Method.descriptor,
            mr.Method.attributes
        | Method.Constructor mc ->
            (mc.Method.cstr_flags :> AccessFlag.for_method list),
            class_constructor_name,
            (mc.Method.cstr_descriptor, `Void),
            mc.Method.cstr_attributes
        | Method.Initializer mi ->
            (mi.Method.init_flags :> AccessFlag.for_method list),
            class_initializer_name,
            ([], `Void),
            mi.Method.init_attributes in
      UTF8Buffer.add_endline buffer empty_utf8;
      UTF8Buffer.add_endline
        buffer
        (dot_method
           ++ (AccessFlag.list_to_utf8 (flags :> AccessFlag.t list))
           ++ (utf8_of_method_desc name desc));
      List.iter
        (add_attribute buffer)
        (List.sort
           Attribute.compare
           (attrs :> Attribute.t list)))
    (List.sort Method.compare cd.ClassDefinition.methods)

let disassemble_to_stream chan cp s =
  let buffer = UTF8Buffer.make () in
  disassemble_to_buffer buffer cp s;
  LineUtils.output chan buffer

let disassemble cp s =
  disassemble_to_stream OutputStream.stdout cp s

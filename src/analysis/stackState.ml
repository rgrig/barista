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

(* Types *)

type locals = Attribute.verification_type_info array

type stack = Attribute.verification_type_info list (* stack top is list head *)

type t = {
    locals : locals;
    stack : stack;
  }


(* Exception *)

type error =
  | Unsupported_instruction of string
  | Empty_stack
  | Invalid_local_index of Utils.u2 * int
  | Invalid_stack_top of Attribute.verification_type_info * Attribute.verification_type_info
  | Invalid_local_contents of u2 * Attribute.verification_type_info * Attribute.verification_type_info
  | Reference_waited of Attribute.verification_type_info
  | Array_waited
  | Category1_waited
  | Category2_waited
  | Different_stack_sizes of int * int
  | Invalid_primitive_array_type
  | Empty_frame_list
  | Different_frames of Utils.u2

exception Exception of error

let fail e = raise (Exception e)

let string_of_error = function
  | Unsupported_instruction x ->
      Printf.sprintf "unsupported instruction: %S" x
  | Empty_stack -> "empty stack"
  | Invalid_local_index (i, l) ->
      Printf.sprintf "invalid local index (%d, length %d)" (i :> int) l
  | Invalid_stack_top (w, f) ->
      Printf.sprintf "invalid stack top: %S waited but %S found"
        (Attribute.string_of_verification_type_info w)
        (Attribute.string_of_verification_type_info f)
  | Invalid_local_contents (i, w, f) ->
      Printf.sprintf "invalid local contents at index %d: %S waited but %S found" 
        (i :> int)
        (Attribute.string_of_verification_type_info w)
        (Attribute.string_of_verification_type_info f)
  | Reference_waited f ->
      Printf.sprintf "reference waited but %S found"
        (Attribute.string_of_verification_type_info f)
  | Array_waited -> "array waited"
  | Category1_waited -> "category1 waited"
  | Category2_waited -> "category2 waited"
  | Different_stack_sizes (sz1, sz2) ->
      Printf.sprintf "different stack sizes (%d and %d)" sz1 sz2
  | Invalid_primitive_array_type -> "invalid primitive array type"
  | Empty_frame_list -> "empty frame list"
  | Different_frames ofs ->
      Printf.sprintf "different frames (at offset %d)" (ofs :> int)

let () =
  Printexc.register_printer
    (function
      | Exception e -> Some (string_of_error e)
      | _ -> None)


(* Construction *)

let make_empty () =
  { locals = [||]; stack = []; }

let java_lang_String = Name.make_for_class_from_external (UTF8.of_string "java.lang.String")

let java_lang_Class = Name.make_for_class_from_external (UTF8.of_string "java.lang.Class")

let java_lang_invoke_MethodType = Name.make_for_class_from_external (UTF8.of_string "java.lang.invoke.MethodType")

let java_lang_invoke_MethodHandle = Name.make_for_class_from_external (UTF8.of_string "java.lang.invoke.MethodHandle")

let verification_type_info_of_constant_descriptor = function
  | `Int _ -> Attribute.Integer_variable_info
  | `Float _ -> Attribute.Float_variable_info
  | `String _ -> Attribute.Object_variable_info (`Class_or_interface java_lang_String)
  | `Class_or_interface _ -> Attribute.Object_variable_info (`Class_or_interface java_lang_Class)
  | `Array_type _ -> Attribute.Object_variable_info (`Class_or_interface java_lang_Class)
  | `Long _ -> Attribute.Long_variable_info
  | `Double _ -> Attribute.Double_variable_info
  | `Interface_method _ -> Attribute.Object_variable_info (`Class_or_interface java_lang_invoke_MethodHandle)
  | `Method_type _ -> Attribute.Object_variable_info (`Class_or_interface java_lang_invoke_MethodType)
  | `Method_handle _ -> Attribute.Object_variable_info (`Class_or_interface java_lang_invoke_MethodHandle)

let of_list l =
  let l =
    List.map
      (function
        | Attribute.Long_variable_info ->
            [Attribute.Long_variable_info; Attribute.Top_variable_info]
        | Attribute.Double_variable_info ->
            [Attribute.Double_variable_info; Attribute.Top_variable_info]
        | x -> [x])
      l in
  let l = List.concat l in
  Array.of_list l

let make_of_parameters c l =
  let l = List.map Attribute.verification_type_info_of_parameter_descriptor l in
  let l = match c with
  | Some (_, true) -> Attribute.Uninitialized_this_variable_info :: l
  | Some (cn, false) -> (Attribute.Object_variable_info (`Class_or_interface cn)) :: l
  | None -> l in
  { locals = of_list l; stack = []; }

let make_of_method cn = function
  | Method.Regular { Method.flags; descriptor; _ } ->
      let l = fst descriptor in
      let l = List.map Attribute.verification_type_info_of_parameter_descriptor l in
      let l =
        if List.mem `Static flags then
          l
        else
          (Attribute.Object_variable_info (`Class_or_interface cn)) :: l in
      { locals = of_list l; stack = [] }
  | Method.Constructor { Method.cstr_descriptor = l ; _ } ->
      let l = List.map Attribute.verification_type_info_of_parameter_descriptor l in
      let l = Attribute.Uninitialized_this_variable_info :: l in
      { locals = of_list l; stack = [] }
  | Method.Initializer _ ->
      make_empty ()

let copy st =
  { locals = Array.copy st.locals; stack = st.stack; }


(* Access and modification *)

let locals_size st =
  Array.length st.locals

let stack_size st =
  List.fold_left
    (fun acc x ->
      acc +
        (match x with
        | Attribute.Double_variable_info
        | Attribute.Long_variable_info -> 2
        | _ -> 1))
    0
    st.stack

let array_for_all2 ?(n = max_int) p a1 a2 =
  let i = ref (pred (Array.length a1)) in
  while (!i >= 0) && (!i < n) && (p a1.(!i) a2.(!i)) do
    decr i
  done;
  !i < 0

let same_locals st1 st2 =
  ((locals_size st1) = (locals_size st2))
    && (array_for_all2 Attribute.equal_verification_type_info st1.locals st2.locals)

let same_stack st1 st2 =
  ((stack_size st1) = (stack_size st2))
    && (List.for_all2 Attribute.equal_verification_type_info st1.stack st2.stack)

let equal st1 st2 =
  (same_locals st1 st2) && (same_stack st1 st2)

let push v s =
printf "@["
  v :: s

let push_return_value x s =
  match x with
  | `Void -> s
  | #Descriptor.for_parameter as y ->
      push (Attribute.verification_type_info_of_parameter_descriptor y) s

let top = function
  | hd :: _ -> hd
  | [] -> fail Empty_stack

let pop = function
  | _ :: tl -> tl
  | [] -> fail Empty_stack

let pop_if v s =
  let v' = top s in
  let popable = match v with
  | Attribute.Object_variable_info _ -> true
  | _ -> Attribute.equal_verification_type_info v v' in
  if popable then
    pop s
  else
    fail (Invalid_stack_top (v, v'))

let is_category1 = function
  | Attribute.Top_variable_info
  | Attribute.Integer_variable_info
  | Attribute.Float_variable_info
  | Attribute.Null_variable_info
  | Attribute.Uninitialized_this_variable_info
  | Attribute.Object_variable_info _
  | Attribute.Uninitialized_variable_info _ -> true
  | Attribute.Long_variable_info
  | Attribute.Double_variable_info -> false

let pop_if_category1 = function
  | hd :: tl -> if is_category1 hd then hd, tl else fail Category1_waited
  | [] -> fail Empty_stack

let pop_if_cat2 = function
  | hd :: tl -> if not (is_category1 hd) then hd, tl else fail Category2_waited
  | [] -> fail Empty_stack

let empty () =
  []

let only_exception cn =
  [Attribute.Object_variable_info (`Class_or_interface cn)]

let load i l =
  let j = (i : u2 :> int) in
  let len = Array.length l in
  if j >= 0 && j < len then
    l.(j)
  else
    fail (Invalid_local_index (i, len))

let check_load i l v =
  let v' = load i l in
  if not (Attribute.equal_verification_type_info v v') then
    fail (Invalid_local_contents (i, v, v'))

let store i v l =
  let i = (i : u2 :> int) in
  let len = Array.length l in
  let sz = (succ i)
      + (match v with
      | Attribute.Long_variable_info
      | Attribute.Double_variable_info -> 1
      | _ -> 0) in
  let l' =
    Array.init
      (max sz len)
      (fun i -> if i < len then l.(i) else Attribute.Top_variable_info) in
  l'.(i) <- v;
  (match v with
  | Attribute.Long_variable_info
  | Attribute.Double_variable_info -> l'.(succ i) <- Attribute.Top_variable_info
  | _ -> ());
  l'


(* Operations *)

let check_reference x =
  match x with
  | Attribute.Integer_variable_info
  | Attribute.Float_variable_info
  | Attribute.Long_variable_info
  | Attribute.Double_variable_info
  | Attribute.Top_variable_info -> fail (Reference_waited x)
  | Attribute.Null_variable_info
  | Attribute.Uninitialized_this_variable_info
  | Attribute.Object_variable_info _
  | Attribute.Uninitialized_variable_info _ -> ()

let enclose (x : Descriptor.array_type) =
  `Array (x :> Descriptor.for_field)

let verification_type_info_of_array_element = function
  | `Array_type at -> Attribute.Object_variable_info (`Array_type (enclose at))
  | `Class_or_interface cn -> Attribute.Object_variable_info (`Array_type (`Array (`Class cn)))

let verification_type_info_of_array_primitive = function
  | `Boolean -> Attribute.Object_variable_info (`Array_type (`Array `Boolean))
  | `Char -> Attribute.Object_variable_info (`Array_type (`Array `Char))
  | `Float -> Attribute.Object_variable_info (`Array_type (`Array `Float))
  | `Double -> Attribute.Object_variable_info (`Array_type (`Array `Double))
  | `Byte -> Attribute.Object_variable_info (`Array_type (`Array `Byte))
  | `Short -> Attribute.Object_variable_info (`Array_type (`Array `Short))
  | `Int -> Attribute.Object_variable_info (`Array_type (`Array `Int))
  | `Long -> Attribute.Object_variable_info (`Array_type (`Array `Long))
  | _ -> fail Invalid_primitive_array_type

let update ofs i st =
  let locals = Array.copy st.locals in
  let stack = st.stack in
  match i with
  | Instruction.AALOAD ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      let stack =
        (match topv with
        | Attribute.Null_variable_info -> push Attribute.Null_variable_info stack
        | Attribute.Object_variable_info (`Array_type (`Array t)) -> push (Attribute.verification_type_info_of_parameter_descriptor t) stack
        | _ -> fail Array_waited) in
      { locals = locals; stack = stack; }
  | Instruction.AASTORE ->
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.ACONST_NULL ->
      let stack = push Attribute.Null_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ALOAD parameter ->
      let loc = load (u2_of_u1 parameter) locals in
      check_reference loc;
      let stack = push loc stack in
      { locals = locals; stack = stack; }
  | Instruction.ALOAD_0 ->
      let loc = load (u2 0) locals in
      check_reference loc;
      let stack = push loc stack in
      { locals = locals; stack = stack; }
  | Instruction.ALOAD_1 ->
      let loc = load (u2 1) locals in
      check_reference loc;
      let stack = push loc stack in
      { locals = locals; stack = stack; }
  | Instruction.ALOAD_2 ->
      let loc = load (u2 2) locals in
      check_reference loc;
      let stack = push loc stack in
      { locals = locals; stack = stack; }
  | Instruction.ALOAD_3 ->
      let loc = load (u2 3) locals in
      check_reference loc;
      let stack = push loc stack in
      { locals = locals; stack = stack; }
  | Instruction.ANEWARRAY parameter ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push (verification_type_info_of_array_element parameter) stack in
      { locals = locals; stack = stack; }
  | Instruction.ARETURN ->
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.ARRAYLENGTH ->
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ASTORE parameter ->
      let loc = top stack in
      let stack = pop stack in
      check_reference loc;
      let locals = store (u2_of_u1 parameter) loc locals in
      { locals = locals; stack = stack; }
  | Instruction.ASTORE_0 ->
      let loc = top stack in
      let stack = pop stack in
      check_reference loc;
      let locals = store (u2 0) loc locals in
      { locals = locals; stack = stack; }
  | Instruction.ASTORE_1 ->
      let loc = top stack in
      let stack = pop stack in
      check_reference loc;
      let locals = store (u2 1) loc locals in
      { locals = locals; stack = stack; }
  | Instruction.ASTORE_2 ->
      let loc = top stack in
      let stack = pop stack in
      check_reference loc;
      let locals = store (u2 2) loc locals in
      { locals = locals; stack = stack; }
  | Instruction.ASTORE_3 ->
      let loc = top stack in
      let stack = pop stack in
      check_reference loc;
      let locals = store (u2 3) loc locals in
      { locals = locals; stack = stack; }
  | Instruction.ATHROW ->
      let exc = top stack in
      check_reference exc;
      let stack = empty () in
      let stack = push exc stack in
      { locals = locals; stack = stack; }
  | Instruction.BALOAD ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.BASTORE ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.BIPUSH _ ->
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.CALOAD ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.CASTORE ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.CHECKCAST parameter ->
      let stack = pop stack in
      let stack = push (Attribute.verification_type_info_of_parameter_descriptor (match parameter with `Array_type at -> (at :> Descriptor.for_parameter) | `Class_or_interface cn -> `Class cn)) stack in
      { locals = locals; stack = stack; }
  | Instruction.D2F ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.D2I ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.D2L ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DADD ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DALOAD ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DASTORE ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.DCMPG ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DCMPL ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DCONST_0 ->
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DCONST_1 ->
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DDIV ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DLOAD parameter ->
      check_load (u2_of_u1 parameter) locals Attribute.Double_variable_info;
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DLOAD_0 ->
      check_load (u2 0) locals Attribute.Double_variable_info;
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DLOAD_1 ->
      check_load (u2 1) locals Attribute.Double_variable_info;
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DLOAD_2 ->
      check_load (u2 2) locals Attribute.Double_variable_info;
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DLOAD_3 ->
      check_load (u2 3) locals Attribute.Double_variable_info;
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DMUL ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DNEG ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DREM ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DRETURN ->
      let stack = pop_if Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DSTORE parameter ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let locals = store (u2_of_u1 parameter) Attribute.Double_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.DSTORE_0 ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let locals = store (u2 0) Attribute.Double_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.DSTORE_1 ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let locals = store (u2 1) Attribute.Double_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.DSTORE_2 ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let locals = store (u2 2) Attribute.Double_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.DSTORE_3 ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let locals = store (u2 3) Attribute.Double_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.DSUB ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = pop_if Attribute.Double_variable_info stack in
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.DUP ->
      let v, stack = pop_if_category1 stack in
      let stack = push v stack in
      let stack = push v stack in
      { locals = locals; stack = stack; }
  | Instruction.DUP2 ->
      let v1 = top stack in
      let stack = pop stack in
      let stack =
        if is_category1 v1 then
          let v2, stack = pop_if_category1 stack in
          let stack = push v2 stack in
          let stack = push v1 stack in
          let stack = push v2 stack in
          push v1 stack
        else
          push v1 (push v1 stack) in
      { locals = locals; stack = stack; }
  | Instruction.DUP2_X1 ->
      let v1 = top stack in
      let stack =
        if is_category1 v1 then
          let stack = pop stack in
          let v2, stack = pop_if_category1 stack in
          let v3, stack = pop_if_category1 stack in
          let stack = push v2 stack in
          let stack = push v1 stack in
          let stack = push v3 stack in
          let stack = push v2 stack in
          push v1 stack
        else
          let stack = pop stack in
          let v2, stack = pop_if_category1 stack in
          let stack = push v1 stack in
          let stack = push v2 stack in
          push v1 stack in
      { locals = locals; stack = stack; }
  | Instruction.DUP2_X2 ->
      let v1 = top stack in
      let stack =
        if is_category1 v1 then begin
          let stack = pop stack in
          let v2, stack = pop_if_category1 stack in
          let v3 = top stack in
          if is_category1 v3 then begin
            let stack = pop stack in
            let v4 = top stack in
            let stack = pop stack in
            let stack = push v2 stack in
            let stack = push v1 stack in
            let stack = push v4 stack in
            let stack = push v3 stack in
            let stack = push v2 stack in
            push v1 stack
          end else begin
            let stack = pop stack in
            let stack = push v2 stack in
            let stack = push v1 stack in
            let stack = push v3 stack in
            let stack = push v2 stack in
            push v1 stack
          end
        end else begin
          let stack = pop stack in
          let v2 = top stack in
          if is_category1 v2 then begin
            let stack = pop stack in
            let v3, stack = pop_if_category1 stack in
            let stack = push v1 stack in
            let stack = push v3 stack in
            let stack = push v2 stack in
            push v1 stack
          end else begin
            let stack = pop stack in
            let stack = push v1 stack in
            let stack = push v2 stack in
            push v1 stack
          end
        end in
      { locals = locals; stack = stack; }
  | Instruction.DUP_X1 ->
      let v1, stack = pop_if_category1 stack in
      let v2, stack = pop_if_category1 stack in
      let stack = push v1 stack in
      let stack = push v2 stack in
      let stack = push v1 stack in
      { locals = locals; stack = stack; }
  | Instruction.DUP_X2 ->
      let v1, stack = pop_if_category1 stack in
      let v2 = top stack in
      let stack =
        if is_category1 v2 then
          let v2, stack = pop_if_category1 stack in
          let v3, stack = pop_if_category1 stack in
          let stack = push v1 stack in
          let stack = push v3 stack in
          let stack = push v2 stack in
          push v1 stack
        else
          let v2, stack = pop_if_cat2 stack in
          let stack = push v1 stack in
          let stack = push v2 stack in
          push v1 stack in
      { locals = locals; stack = stack; }
  | Instruction.F2D ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.F2I ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.F2L ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FADD ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FALOAD ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FASTORE ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.FCMPG ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FCMPL ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FCONST_0 ->
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FCONST_1 ->
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FCONST_2 ->
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FDIV ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FLOAD parameter ->
      check_load (u2_of_u1 parameter) locals Attribute.Float_variable_info;
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FLOAD_0 ->
      check_load (u2 0) locals Attribute.Float_variable_info;
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FLOAD_1 ->
      check_load (u2 1) locals Attribute.Float_variable_info;
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FLOAD_2 ->
      check_load (u2 2) locals Attribute.Float_variable_info;
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FLOAD_3 ->
      check_load (u2 3) locals Attribute.Float_variable_info;
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FMUL ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FNEG ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FREM ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FRETURN ->
      let stack = pop_if Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.FSTORE parameter ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let locals = store (u2_of_u1 parameter) Attribute.Float_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.FSTORE_0 ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let locals = store (u2 0) Attribute.Float_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.FSTORE_1 ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let locals = store (u2 1) Attribute.Float_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.FSTORE_2 ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let locals = store (u2 2) Attribute.Float_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.FSTORE_3 ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let locals = store (u2 3) Attribute.Float_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.FSUB ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = pop_if Attribute.Float_variable_info stack in
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.GETFIELD (_, _, desc) ->
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      let stack = push (Attribute.verification_type_info_of_parameter_descriptor desc) stack in
      { locals = locals; stack = stack; }
  | Instruction.GETSTATIC (_, _, desc) ->
      let stack = push (Attribute.verification_type_info_of_parameter_descriptor desc) stack in
      { locals = locals; stack = stack; }
  | Instruction.GOTO _ ->
      { locals = locals; stack = stack; }
  | Instruction.GOTO_W _ ->
      { locals = locals; stack = stack; }
  | Instruction.I2B ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.I2C ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.I2D ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.I2F ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.I2L ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.I2S ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IADD ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IALOAD ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IAND ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IASTORE ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.ICONST_0 ->
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ICONST_1 ->
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ICONST_2 ->
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ICONST_3 ->
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ICONST_4 ->
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ICONST_5 ->
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ICONST_M1 ->
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IDIV ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IF_ACMPEQ _ ->
      let stack = pop stack in
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.IF_ACMPNE _ ->
      let stack = pop stack in
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.IF_ICMPEQ _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IF_ICMPGE _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IF_ICMPGT _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IF_ICMPLE _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IF_ICMPLT _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IF_ICMPNE _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IFEQ _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IFGE _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IFGT _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IFLE _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IFLT _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IFNE _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IFNONNULL _ ->
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.IFNULL _ ->
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.IINC (parameter, _) ->
      check_load (u2_of_u1 parameter) locals Attribute.Integer_variable_info;
      let locals = store (u2_of_u1 parameter) Attribute.Integer_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.ILOAD parameter ->
      check_load (u2_of_u1 parameter) locals Attribute.Integer_variable_info;
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ILOAD_0 ->
      check_load (u2 0) locals Attribute.Integer_variable_info;
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ILOAD_1 ->
      check_load (u2 1) locals Attribute.Integer_variable_info;
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ILOAD_2 ->
      check_load (u2 2) locals Attribute.Integer_variable_info;
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ILOAD_3 ->
      check_load (u2 3) locals Attribute.Integer_variable_info;
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IMUL ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.INEG ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.INSTANCEOF _ ->
      let stack = pop stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.INVOKEDYNAMIC (_, _, (params, ret)) ->
      let infos = List.rev_map Attribute.verification_type_info_of_parameter_descriptor params in
      let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      let stack = push_return_value ret stack in
      { locals = locals; stack = stack; }
  | Instruction.INVOKEINTERFACE ((_, _, (params, ret)), _) ->
      let infos = List.rev_map Attribute.verification_type_info_of_parameter_descriptor params in
      let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      let stack = push_return_value ret stack in
      { locals = locals; stack = stack; }
  | Instruction.INVOKESPECIAL (cn, mn, (params, ret)) ->
      let infos = List.rev_map Attribute.verification_type_info_of_parameter_descriptor params in
      let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      let stack = push_return_value ret stack in
      let locals, stack =
        if UTF8.equal Consts.class_constructor (Name.utf8_for_method mn) then
          match topv with
          | Attribute.Uninitialized_variable_info ofs ->
              let f = function Attribute.Uninitialized_variable_info ofs' when ofs = ofs' -> Attribute.Object_variable_info (`Class_or_interface cn) | x -> x in
              Array.map f locals, List.map f stack
          | Attribute.Uninitialized_this_variable_info ->
              let f = function Attribute.Uninitialized_this_variable_info -> Attribute.Object_variable_info (`Class_or_interface cn) | x -> x in
              Array.map f locals, List.map f stack
          | _ -> locals, stack
        else
          locals, stack in
      { locals = locals; stack = stack; }
  | Instruction.INVOKESTATIC (_, _, (params, ret)) ->
      let infos = List.rev_map Attribute.verification_type_info_of_parameter_descriptor params in
      let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
      let stack = push_return_value ret stack in
      { locals = locals; stack = stack; }
  | Instruction.INVOKEVIRTUAL (_, _, (params, ret)) ->
      let infos = List.rev_map Attribute.verification_type_info_of_parameter_descriptor params in
      let stack = List.fold_left (fun acc elem -> pop_if elem acc) stack infos in
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      let stack = push_return_value ret stack in
      { locals = locals; stack = stack; }
  | Instruction.IOR ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IREM ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IRETURN ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ISHL ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ISHR ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.ISTORE parameter ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let locals = store (u2_of_u1 parameter) Attribute.Integer_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.ISTORE_0 ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let locals = store (u2 0) Attribute.Integer_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.ISTORE_1 ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let locals = store (u2 1) Attribute.Integer_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.ISTORE_2 ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let locals = store (u2 2) Attribute.Integer_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.ISTORE_3 ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let locals = store (u2 3) Attribute.Integer_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.ISUB ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IUSHR ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.IXOR ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.JSR _ ->
      fail (Unsupported_instruction "JSR")
  | Instruction.JSR_W _ ->
      fail (Unsupported_instruction "JSR_W")
  | Instruction.L2D ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.L2F ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.L2I ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LADD ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LALOAD ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LAND ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LASTORE ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.LCMP ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LCONST_0 ->
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LCONST_1 ->
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LDC parameter ->
      let stack = push (verification_type_info_of_constant_descriptor parameter) stack in
      { locals = locals; stack = stack; }
  | Instruction.LDC2_W parameter ->
      let stack = push (verification_type_info_of_constant_descriptor parameter) stack in
      { locals = locals; stack = stack; }
  | Instruction.LDC_W parameter ->
      let stack = push (verification_type_info_of_constant_descriptor parameter) stack in
      { locals = locals; stack = stack; }
  | Instruction.LDIV ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LLOAD parameter ->
      check_load (u2_of_u1 parameter) locals Attribute.Long_variable_info;
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LLOAD_0 ->
      check_load (u2 0) locals Attribute.Long_variable_info;
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LLOAD_1 ->
      check_load (u2 1) locals Attribute.Long_variable_info;
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LLOAD_2 ->
      check_load (u2 2) locals Attribute.Long_variable_info;
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LLOAD_3 ->
      check_load (u2 3) locals Attribute.Long_variable_info;
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LMUL ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LNEG ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LOOKUPSWITCH _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LOR ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LREM ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LRETURN ->
      let stack = pop_if Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LSHL ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LSHR ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LSTORE parameter ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let locals = store (u2_of_u1 parameter) Attribute.Long_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.LSTORE_0 ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let locals = store (u2 0) Attribute.Long_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.LSTORE_1 ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let locals = store (u2 1) Attribute.Long_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.LSTORE_2 ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let locals = store (u2 2) Attribute.Long_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.LSTORE_3 ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let locals = store (u2 3) Attribute.Long_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.LSUB ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LUSHR ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.LXOR ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = pop_if Attribute.Long_variable_info stack in
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.MONITORENTER ->
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.MONITOREXIT ->
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.MULTIANEWARRAY (at, dims) ->
      let s = ref stack in
      for i = 1 to (dims :> int) do
        s := pop_if Attribute.Integer_variable_info !s;
      done;
      let stack = push (Attribute.Object_variable_info at) !s in
      { locals = locals; stack = stack; }
  | Instruction.NEW _ ->
      let stack = push (Attribute.Uninitialized_variable_info ofs) stack in
      { locals = locals; stack = stack; }
  | Instruction.NEWARRAY parameter ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = push (verification_type_info_of_array_primitive parameter) stack in
      { locals = locals; stack = stack; }
  | Instruction.NOP ->
      { locals = locals; stack = stack; }
  | Instruction.POP ->
      let _, stack = pop_if_category1 stack in
      { locals = locals; stack = stack; }
  | Instruction.POP2 ->
      let v1 = top stack in
      let stack =
        if is_category1 v1 then
          snd (pop_if_category1 (snd (pop_if_category1 stack)))
        else
          pop stack in
      { locals = locals; stack = stack; }
  | Instruction.PUTFIELD (_, _, desc) ->
      let stack = pop_if (Attribute.verification_type_info_of_parameter_descriptor desc) stack in
      let topv = top stack in
      check_reference topv;
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.PUTSTATIC (_, _, desc) ->
      let stack = pop_if (Attribute.verification_type_info_of_parameter_descriptor desc) stack in
      { locals = locals; stack = stack; }
  | Instruction.RET _ ->
      fail (Unsupported_instruction "RET")
  | Instruction.RETURN ->
      { locals = locals; stack = stack; }
  | Instruction.SALOAD ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.SASTORE ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop_if Attribute.Integer_variable_info stack in
      let stack = pop stack in
      { locals = locals; stack = stack; }
  | Instruction.SIPUSH _ ->
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.SWAP ->
      let v1, stack = pop_if_category1 stack in
      let v2, stack = pop_if_category1 stack in
      let stack = push v1 stack in
      let stack = push v2 stack in
      { locals = locals; stack = stack; }
  | Instruction.TABLESWITCH _ ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_ALOAD parameter ->
      let loc = load parameter locals in
      check_reference loc;
      let stack = push loc stack in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_ASTORE parameter ->
      let loc = top stack in
      let stack = pop stack in
      check_reference loc;
      let locals = store parameter loc locals in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_DLOAD parameter ->
      check_load parameter locals Attribute.Double_variable_info;
      let stack = push Attribute.Double_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_DSTORE parameter ->
      let stack = pop_if Attribute.Double_variable_info stack in
      let locals = store parameter Attribute.Double_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_FLOAD parameter ->
      check_load parameter locals Attribute.Float_variable_info;
      let stack = push Attribute.Float_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_FSTORE parameter ->
      let stack = pop_if Attribute.Float_variable_info stack in
      let locals = store parameter Attribute.Float_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_IINC (parameter, _) ->
      check_load parameter locals Attribute.Integer_variable_info;
      let locals = store parameter Attribute.Integer_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_ILOAD parameter ->
      check_load parameter locals Attribute.Integer_variable_info;
      let stack = push Attribute.Integer_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_ISTORE parameter ->
      let stack = pop_if Attribute.Integer_variable_info stack in
      let locals = store parameter Attribute.Integer_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_LLOAD parameter ->
      check_load parameter locals Attribute.Long_variable_info;
      let stack = push Attribute.Long_variable_info stack in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_LSTORE parameter ->
      let stack = pop_if Attribute.Long_variable_info stack in
      let locals = store parameter Attribute.Long_variable_info locals in
      { locals = locals; stack = stack; }
  | Instruction.WIDE_RET _ ->
      fail (Unsupported_instruction "WIDE_RET")

type 'a unifier = 'a -> 'a -> 'a

type instance =
  [ `Array_type of Descriptor.array_type
  | `Class_or_interface of Name.for_class ]

let java_lang_Object_name = Name.make_for_class_from_external (UTF8.of_string "java.lang.Object")

let java_lang_Object = `Class_or_interface java_lang_Object_name

let make_array_unifier (f : Name.for_class unifier) (x : Descriptor.array_type) (y : Descriptor.array_type) =
  let rec ua (x : Descriptor.java_type) (y : Descriptor.java_type) = match x, y with
  | (`Array x'), (`Array y') -> `Array (ua (x' :> Descriptor.java_type) (y' :> Descriptor.java_type))
  | (`Array _), _ -> `Class java_lang_Object_name
  | _, (`Array _) -> `Class java_lang_Object_name
  | (`Class x'), (`Class y') -> `Class (f x' y')
  | `Boolean, `Boolean -> `Boolean
  | `Byte, `Byte -> `Byte
  | `Char, `Char -> `Char
  | `Double, `Double -> `Double
  | `Float, `Float -> `Float
  | `Int, `Int -> `Int
  | `Long, `Long -> `Long
  | `Short, `Short -> `Short
  | _ -> raise Not_found in
  try
    (match ua (x :> Descriptor.java_type) (y :> Descriptor.java_type) with
    | `Array x -> `Array_type (`Array x)
    | _ -> java_lang_Object)
  with Not_found -> java_lang_Object

let make_unifier f x y =
  let array_unifier = make_array_unifier f in
  match x, y with
  | (`Array_type at1), (`Array_type at2) -> array_unifier at1 at2
  | (`Class_or_interface cn1), (`Class_or_interface cn2) -> `Class_or_interface (f cn1 cn2)
  | _ -> java_lang_Object

let unify_to_java_lang_Object =
  let utjlo x y =
    if Name.equal_for_class x y then
      x
    else
      java_lang_Object_name in
  make_unifier utjlo

let unify_to_closest_common_parent cl l =
  let rec parents cn =
    let hd, prn =
      try
        let c, p = List.find (fun (x, _) -> Name.equal_for_class cn x) l in
        (Name.internal_utf8_for_class c), p
      with Not_found ->
        let cd = ClassLoader.find_class cl (Name.external_utf8_for_class cn) in
        (Name.internal_utf8_for_class cd.ClassDefinition.name),
        cd.ClassDefinition.extends in
    let tl = match prn with
    | Some x -> parents x
    | None -> [] in
    hd :: tl in
  let rec common_parent l = function
  | hd :: tl -> if List.exists (UTF8.equal hd) l then hd else common_parent l tl
  | [] -> UTF8.of_string "java/lang/Object" in
  let utccp x y =
    let parents_x = parents x in
    let parents_y = parents y in
    Name.make_for_class_from_internal (common_parent parents_x parents_y) in
    make_unifier utccp

let unify_to_parent_list l =
  let rec parents cn =
    let hd, prn =
      try
        let c, p = List.find (fun (x, _) -> Name.equal_for_class cn x) l in
        (Name.internal_utf8_for_class c), p
      with Not_found ->
        (Name.internal_utf8_for_class cn), None in
    let tl = match prn with
    | Some x -> parents x
    | None -> [] in
    hd :: tl in
  let rec common_parent l = function
  | hd :: tl -> if List.exists (UTF8.equal hd) l then hd else common_parent l tl
  | [] -> UTF8.of_string "java/lang/Object" in
  let utccp x y =
    let parents_x = parents x in
    let parents_y = parents y in
    Name.make_for_class_from_internal (common_parent parents_x parents_y) in
    make_unifier utccp

let unify f st1 st2 =
  let unify_elements vti1 vti2 =
    match (vti1, vti2) with
    | Attribute.Top_variable_info, _
    | _, Attribute.Top_variable_info -> Attribute.Top_variable_info
    | (Attribute.Object_variable_info o1), (Attribute.Object_variable_info o2) -> Attribute.Object_variable_info (f o1 o2)
    | Attribute.Null_variable_info, (Attribute.Object_variable_info _) -> vti2
    | (Attribute.Object_variable_info _), Attribute.Null_variable_info -> vti1
    | _ -> if vti1 = vti2 then vti1 else Attribute.Top_variable_info in
  let sz1 = List.length st1.stack in
  let sz2 = List.length st2.stack in
  if sz1 = sz2 then begin
    let len1 = Array.length st1.locals in
    let len2 = Array.length st2.locals in
    let len = max len1 len2 in
    let locals =
      Array.init
        len
        (fun i ->
          if (i < len1) && (i < len2) then
            unify_elements st1.locals.(i) st2.locals.(i)
          else
            Attribute.Top_variable_info) in
    let stack = List.map2 unify_elements st1.stack st2.stack in
    { locals = locals; stack = stack; }
  end else
    fail (Different_stack_sizes (sz1, sz2))

let encode ?(optimize = true) l =
  let rec filter = function
    | Attribute.Long_variable_info :: Attribute.Top_variable_info :: tl -> Attribute.Long_variable_info :: (filter tl)
    | Attribute.Double_variable_info :: Attribute.Top_variable_info :: tl -> Attribute.Double_variable_info :: (filter tl)
    | hd :: tl -> hd :: (filter tl)
    | [] -> [] in
  let full_frame ofs st =
    Attribute.Full_frame (ofs, (filter (Array.to_list st.locals)), (List.rev st.stack)) in
  let encode2 ofs prev curr =
    if optimize && same_locals prev curr then
      match curr.stack with
      | [] -> Attribute.Same_frame ofs
      | [elem] -> Attribute.Same_locals_1_stack_item_frame (ofs, elem)
      | _ -> full_frame ofs curr
    else if curr.stack = [] then
      let curr_size = Array.length curr.locals in
      let prev_size = Array.length prev.locals in
      match curr_size - prev_size with
      | -3 when array_for_all2 ~n:curr_size Attribute.equal_verification_type_info prev.locals curr.locals ->
          Attribute.Chop_3_frame ofs
      | -2 when array_for_all2 ~n:curr_size Attribute.equal_verification_type_info prev.locals curr.locals ->
          Attribute.Chop_2_frame ofs
      | -1 when array_for_all2 ~n:curr_size Attribute.equal_verification_type_info prev.locals curr.locals ->
          Attribute.Chop_1_frame ofs
      | 1 when array_for_all2 ~n:prev_size Attribute.equal_verification_type_info prev.locals curr.locals ->
          Attribute.Append_1_frame (ofs, curr.locals.(curr_size - 1))
      | 2 when array_for_all2 ~n:prev_size Attribute.equal_verification_type_info prev.locals curr.locals ->
          Attribute.Append_2_frame (ofs,
                                    curr.locals.(curr_size - 2),
                                    curr.locals.(curr_size - 1))
      | 3 when array_for_all2 ~n:prev_size Attribute.equal_verification_type_info prev.locals curr.locals ->
          Attribute.Append_3_frame (ofs,
                                    curr.locals.(curr_size - 3),
                                    curr.locals.(curr_size - 2),
                                    curr.locals.(curr_size - 1))
      | _ ->
          full_frame ofs curr
    else
      full_frame ofs curr in
  let l = List.sort compare l in
  let _, res = match l with
  | hd :: tl ->
      List.fold_left
        (fun ((prev_ofs, prev_st), acc) (ofs, st) ->
          if prev_ofs <> ofs then
            (ofs, st), ((encode2 ofs prev_st st) :: acc)
          else if equal prev_st st then
            (prev_ofs, prev_st), acc
          else
            fail (Different_frames ofs))
        (hd, [full_frame (fst hd) (snd hd)])
        tl
  | [] -> fail Empty_frame_list in
  List.rev res

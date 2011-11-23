(**
  A truly high-level representation of Java bytecode.

    - agnostic of any byte counts (such as offsets or instruction sizes)
    - no instruction variants whose goal is to save speed/memory
      (such as ICONST/ICONST_0, LDC/LDC_W)
    - no constant pool
 *)

(* NOTE: This is supposed to be what [ClassDefinition] should have been in
the first place. *)

module Instruction : sig (* {{{ *)
  type label
  module LabelHash : Hashtbl.S with type key = label
end (* }}} *)

module Attribute : sig (* {{{ *)
  type constant_value =
    | Long_value of int64
    | Float_value of float
    | Double_value of float
    | Boolean_value of bool
    | Byte_value of int
    | Character_value of int
    | Short_value of int
    | Integer_value of int32
    | String_value of Utils.UTF8.t

  type inner_class_element = {
      inner_class : Name.for_class option;
      outer_class : Name.for_class option;
      inner_name : Utils.UTF8.t option;
      inner_flags : AccessFlag.for_inner_class list;
    }
  and enclosing_method_value = {
      innermost_class : Name.for_class;
      enclosing_method : (Name.for_method * Descriptor.for_method) option;
    }

  type for_class =
    [ `InnerClasses of inner_class_element list
    | `EnclosingMethod of enclosing_method_value
    | `Synthetic (** auto-generated element *)
    | `Signature of [`Class of Signature.class_signature]
    | `SourceFile of Utils.UTF8.t
    | `SourceDebugExtension of Utils.UTF8.t (** implementation specific *)
    | `Deprecated
    | `RuntimeVisibleAnnotations of Annotation.t list
    | `RuntimeInvisibleAnnotations of Annotation.t list
    | `RuntimeVisibleTypeAnnotations of Annotation.extended list
    | `RuntimeInvisibleTypeAnnotations of Annotation.extended list
    | `BootstrapMethods of Bootstrap.method_specifier list (** bootstrap for dynamic methods *)
    | `Module of Utils.UTF8.t * Utils.UTF8.t (** module name and version *)
    | `Unknown of Utils.UTF8.t * string ]
end (* }}} *)

module Method : sig (* {{{ *)
  type t
end (* }}} *)

type t = {
    access_flags : AccessFlag.for_class list;
    name : Name.for_class;
    extends : Name.for_class option;
    implements : Name.for_class list;
    fields : Field.t list;
    methods : Method.t list;
    attributes : Attribute.for_class list;
  }

type error =
  | Invalid_class_name

exception Exception of error

val check_version_high : ?version : Version.t -> t -> t
val decode : ?version : Version.t -> ClassFile.t -> t
val encode : ?version : Version.t -> t -> ClassFile.t

(*
vim:tw=0:
*)
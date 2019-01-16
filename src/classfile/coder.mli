val string_of_error : HighTypes.error -> string

val check_version_high : Version.t -> HighTypes.class_ -> HighTypes.class_
val decode : ClassFile.t -> (HighTypes.class_ * Version.t)
val encode : (HighTypes.class_ * Version.t) -> ClassFile.t

val fresh_label : unit -> HighTypes.label

(* TODO: This should be hidden, once we hide the size of primitives completely. *)
val size_of_bt : HighTypes.bytecode_type -> int

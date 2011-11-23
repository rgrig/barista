open BaristaLibrary
open Utils

let utf8 = UTF8.of_string
let utf8_for_class x = Name.make_for_class_from_external (utf8 x)
let utf8_for_field x = Name.make_for_field (utf8 x)
let utf8_for_method x = Name.make_for_method (utf8 x)

let example_Hello = utf8_for_class "example.Hello"
let java_lang_Object = utf8_for_class "java.lang.Object"
let java_lang_System = utf8_for_class "java.lang.System"
let java_lang_String = utf8_for_class "java.lang.String"
let java_io_PrintStream = utf8_for_class "java.io.PrintStream"
let out = utf8_for_field "out"
let println = utf8_for_method "println"
let main = utf8_for_method "main"

let () =
  let instructions = [
    Instruction.GETSTATIC (java_lang_System, out, `Class java_io_PrintStream);
    Instruction.LDC (`String (utf8 "hello."));
    Instruction.INVOKEVIRTUAL (`Class_or_interface java_io_PrintStream,
                               println,
                               ([`Class java_lang_String], `Void));
    Instruction.RETURN;
  ] in
  let code = {
    Attribute.max_stack = u2 2;
    Attribute.max_locals = u2 1;
    Attribute.code = instructions;
    Attribute.exception_table = [];
    Attribute.attributes = [];
  } in
  let main_method =
    Method.Regular { Method.flags = [`Public; `Static]; 
                     Method.name = main;
                     Method.descriptor = [`Array (`Class java_lang_String)], `Void;
                     Method.attributes = [`Code code] } in
  let hello = {
    ClassDefinition.access_flags = [`Public; `Super];
    ClassDefinition.name = example_Hello;
    ClassDefinition.extends = Some java_lang_Object;
    ClassDefinition.implements = [];
    ClassDefinition.fields = [];
    ClassDefinition.methods = [main_method];
    ClassDefinition.attributes = [];
  } in
  let cf = ClassDefinition.encode hello in
  ClassFile.write cf (OutputStream.make_of_channel (open_out "Hello.class"))

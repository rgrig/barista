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

open BaristaLibrary
open Common
open Instruction

let () =
  let fts = (Signature.field_type_signature_of_utf8 (utf8 "TA;")) in
  let x = { Field.flags = [`Public];
	    Field.name = utf8_for_field "x";
	    Field.descriptor = `Class (utf8_for_class "java.lang.Number");
            Field.attributes = [`Signature (`Field fts)] } in
  let instructions = [
    GETSTATIC ((utf8_for_class "java.lang.System"),
               (utf8_for_field "out"),
               class_PrintStream);
    LDC (`String (utf8 "hello\n..."));
    INVOKEVIRTUAL ((`Class_or_interface (utf8_for_class "java.io.PrintStream")),
                   (utf8_for_method "println"),
                   ([class_String], `Void));
    RETURN ] in
  let depr = ((utf8_for_class "java.lang.Deprecated"), []) in
  let annot = ((utf8_for_class "pack.MyAnnotation"),
	       [((utf8 "e"), (Annotation.Enum_value ((utf8_for_class "pack.MyAnnotation$E"),
						     (utf8_for_field "E3"))));
		((utf8 "c"), (Annotation.Array_value [(Annotation.Int_value 5l);
						      (Annotation.Int_value 7l)]));
		((utf8 "b"), (Annotation.Float_value 3.14));		
		((utf8 "a"), (Annotation.String_value (utf8 "xyz")))]) in
  let main = compile_method
      ~meth_attributes:[`RuntimeVisibleAnnotations [annot; depr]]
      instructions in
  let cs = Signature.class_signature_of_utf8 (utf8 "<A:Ljava/lang/Number;>Ljava/lang/Object;") in
  let cls = compile_class
      ~fields:[x]
      ~attributes:[`SourceFile (utf8 "<<Filename>>");
		   `Signature (`Class cs)]
      [main] in
  ClassFile.write cls (OutputStream.make_of_channel (open_out "pack/Test.class"))

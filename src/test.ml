(** An executable to test small bits while developing. Once a feature is done,
  the code is either scrapped or turned into a proper test. *)

open Format

module IS = InputStream
module CF = ClassFile
module HC = HighClass

let check fn =
  let ch = open_in fn in
  let cl_in = IS.make_of_channel ch in
  let cf = CF.read cl_in in
  let _ = HC.decode cf in
  printf "@[%s@." fn

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    check Sys.argv.(i)
  done

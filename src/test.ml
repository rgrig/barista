(** An executable to test small bits while developing. Once a feature is done,
  the code is either scrapped or turned into a proper test. *)

open Format

module IS = InputStream
module OS = OutputStream
module CF = ClassFile
module HC = HighClass

let write = ref false

let check fn =
  let ch = open_in fn in
  let cl_in = IS.make_of_channel ch in
  let cf = CF.read cl_in in
  let hc, version = HC.decode cf in
  close_in_noerr ch;
  let bc = HC.encode (hc, version) in
  if !write then begin
    let ch = open_out (fn ^ ".round") in
    let ch_out = OS.make_of_channel ch in
    CF.write bc ch_out;
    close_out ch
  end

let handle fn =
  try begin
    printf "@[%s" fn;
    check fn;
    printf " ✓@."
  end with
    | Version.Exception e -> printf "  %s@." (Version.string_of_error e)
    | Name.Exception e -> printf "  %s@." (Name.string_of_error e)
    | AccessFlag.Exception e -> printf "  %s@." (AccessFlag.string_of_error e)
    | HC.Exception e -> printf "  %s@." (HC.string_of_error e)

let () =
  let flags =
    [ "-w", Arg.Set write, "write the result of encoding after decoding" ] in
  Arg.parse_argv Sys.argv flags handle "test [-w] classfiles"

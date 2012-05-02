module Config = struct
  (* This is almost like [CamomileLibraryDyn.Config]. The main difference is
  that it tries to use the subdirecory "camomile" relative to the location of
  the current executable. *)

  let ( / ) = Filename.concat

  let bin_dir_linux () = Filename.dirname (Unix.readlink "/proc/self/exe")
  let bin_dir_shell () = Filename.dirname (Sys.getenv "_")
  let bin_dir_bsd () = Filename.dirname (Unix.readlink "/proc/curproc/file")
  let bin_dir_path () =
    let f = Sys.argv.(0) in
    if Sys.file_exists f then Filename.dirname f else
    let ds = Sys.getenv "PATH" in
    let sep = if Sys.os_type = "Unix" then ":" else ";" in
    let ds = Str.split (Str.regexp sep) ds in
    let rec loop = function
      | [] -> raise Not_found
      | d :: ds -> if Sys.file_exists (d / f) then d else loop ds in
    loop ds

  let find_dir env subdir default =
    let fs =
      [ (fun () -> Sys.getenv env)
      ; (fun () -> (Sys.getenv "CAMOMILE_DIR")/subdir)
      ; (fun () -> bin_dir_linux()/"camomile"/subdir)
      ; (fun () -> bin_dir_shell()/"camomile"/subdir)
      ; (fun () -> bin_dir_bsd()/"camomile"/subdir)
      ; (fun () -> bin_dir_path()/"camomile"/subdir)
      ; (fun () -> default) ] in
    let rec iter = function
      | [] -> failwith "can't open camomile data files; please set CAMOMILE_DIR"
      | f :: fs ->
          (try
            let r = f () in
            if Sys.is_directory r then r else iter fs
          with Not_found | Sys_error _ | Unix.Unix_error _ -> iter fs) in
    iter fs

  module D = CamomileLibrary.DefaultConfig

  let datadir = find_dir "CAMOMILE_DATADIR" "database" D.datadir
  let localedir = find_dir "CAMOMILE_LOCALEDIR" "locales" D.localedir
  let charmapdir = find_dir "CAMOMILE_CHARMAPDIR" "charmaps" D.charmapdir
  let unimapdir = find_dir "CAMOMILE_UNIMAPDIR" "mappings" D.unimapdir
end


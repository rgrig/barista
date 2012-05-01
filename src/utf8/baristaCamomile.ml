module Config = struct
  (* This is almost like [CamomileLibraryDyn.Config]. The main difference is
  that it tries to use the subdirecory "camomile" relative to the location of
  the current executable. *)

  (* TODO: Add other mechanisms for guessing the bindir. *)
  let bin_dir () = Filename.dirname (Unix.readlink "/proc/self/exe")

  let find_dir env subdir default =
    let ( / ) = Filename.concat in
    let fs =
      [ (fun () -> Sys.getenv env)
      ; (fun () -> (Sys.getenv "CAMOMILE_DIR")/subdir)
      ; (fun () -> bin_dir()/"camomile"/subdir)
      ; (fun () -> default) ] in
    let rec iter = function
      | [] -> raise Not_found
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


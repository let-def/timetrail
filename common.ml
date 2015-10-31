(* Misc *)

let (+:=) r v = r := v :: !r

let key table s =
  try Hashtbl.find table s
  with Not_found ->
    let r = ref [] in
    Hashtbl.add table s r;
    r

let protect f ~finally =
  try
    let r = f () in
    finally ();
    r
  with exn ->
    finally ();
    raise exn

let is_prefix_of prefix ~subject =
  String.length subject >= String.length prefix &&
  try
    for i = String.length prefix - 1 downto 0 do
      if subject.[i] <> prefix.[i] then
        raise Not_found
    done;
    true
  with Not_found -> false

let fatal msg exn =
    failwith (Printf.sprintf "Cannot %s: %s" msg (Printexc.to_string exn))

let attempt msg f =
  try f ()
  with exn -> fatal msg exn

let recover msg f =
  try f ()
  with exn -> Printf.eprintf "Failed %s: %s\n" msg (Printexc.to_string exn)

let user_entry = lazy begin
  attempt "read user settings" @@ fun () ->
  Unix.getpwuid (Unix.getuid ())
end

let create_directory dir =
  if not (Sys.file_exists dir) then
    begin
      attempt ("create data directory " ^ dir)
        (fun () -> Unix.mkdir dir 0o770);
      prerr_endline ("# Created " ^ dir)
    end
  else if not (Sys.is_directory dir) then
    failwith (dir ^ " should be a directory")

let launch_time = lazy (Unix.time ())

let trail_path = lazy begin
  let lazy launch_time = launch_time in
  let lazy user_entry = user_entry in
  let {Unix. tm_mon; tm_year} = Unix.gmtime launch_time in
  let dir = Filename.concat user_entry.Unix.pw_dir ".timetrail" in
  create_directory dir;
  let dir = Filename.concat dir (string_of_int (tm_year + 1900)) in
  create_directory dir;
  Filename.concat dir (string_of_int (tm_mon + 1) ^ "." ^ Unix.gethostname ())
end

(* Entries *)

type entry = {
  time : float;
  text : string;
}

let rec load_entries filter ic acc =
  match Scanf.fscanf ic "%f\t%s\n" (fun time text -> {time; text}) with
  | entry when filter entry -> load_entries filter ic (entry :: acc)
  | _ -> load_entries filter ic acc
  | exception End_of_file -> acc

let load_entries ?(filter=fun _ -> true) ic acc =
  load_entries filter ic acc

let save_entry oc entry =
  Printf.fprintf oc "%0.0f\t%s\n" entry.time entry.text

let save_entries oc entries =
  List.iter (save_entry oc) entries

let compare_entry_text_only t1 t2 =
  String.compare t1.text t2.text

let compare_entry_text t1 t2 =
  match String.compare t1.text t2.text with
  | 0 -> compare t1.time t2.time
  | n -> n

let compare_entry_time t1 t2 =
  match compare t1.time t2.time with
  | 0 -> String.compare t1.text t2.text
  | n -> n

(* *)

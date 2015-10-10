open Common

type tree = {
  sep      : string;
  time_min : float;
  time_max : float;
  entries  : entry list;
  children : tree list;
}

let make_tree l =
  let rec subtree offset sep = function
    | [] ->
      {time_min = infinity; time_max = neg_infinity; sep;
       entries = []; children = []}
    | (x :: _) as all ->
      let table = Hashtbl.create 7 in
      let entries = ref [] in
      let rec aux = function
        | [] -> ()
        | l :: ls ->
          begin match String.index_from l.text offset '/' with
            | n ->
              key table (n+1, String.sub l.text offset (n - offset)) +:= l
            | exception Not_found ->
              entries +:= l
          end;
          aux ls
      in
      aux all;
      let children = Hashtbl.fold
          (fun (offset, sep) subentries children ->
             subtree offset sep !subentries :: children)
          table []
      and entries = !entries
      in
      let minmax_entry (t_min, t_max) entry =
        min t_min entry.time, max t_max entry.time
      and minmax_tree (t_min, t_max) tree =
        min t_min tree.time_min, max t_max tree.time_max
      in
      let time_minmax = x.time, x.time in
      let time_minmax = List.fold_left minmax_entry time_minmax entries in
      let time_minmax = List.fold_left minmax_tree time_minmax children in
      let time_min, time_max = time_minmax in
      { sep; time_min; time_max; entries; children }
  in
  subtree 0 "" l

let paths = ref []
let min_date = ref neg_infinity
let max_date = ref infinity

let pass_filter entry =
  entry.time >= !min_date &&
  entry.time <= !max_date &&
  !paths = [] || List.exists (is_prefix_of ~subject:entry.text) !paths

let rec loadinput path acc =
  try
    if Sys.is_directory path then
      Array.fold_left
        (fun acc file -> loadinput (Filename.concat path file) acc)
        acc (Sys.readdir path)
    else
      let ic = open_in path in
      protect (fun () -> load_entries ~filter:pass_filter ic acc)
        ~finally:(fun () -> close_in_noerr ic)
  with exn ->
    Printf.eprintf "Couldn't load %s: %s\n"
      path (Printexc.to_string exn);
    acc

let dataset = ref []
let default_dataset () =
  let entry = Unix.getpwuid (Unix.getuid ()) in
  [Filename.concat entry.Unix.pw_dir ".timetrail"]

let string_of_tree_time tree =
  let open Unix in
  let time_min = Unix.localtime tree.time_min in
  let time_max = Unix.localtime tree.time_max in
  if time_min.tm_year = time_max.tm_year &&
     time_min.tm_mon  = time_max.tm_mon  &&
     time_min.tm_mday = time_max.tm_mday then
    Printf.sprintf "from %d/%d/%d %d:%d to %d:%d"
      time_min.tm_mday time_min.tm_mon (time_min.tm_year + 1900)
      time_min.tm_hour time_min.tm_min
      time_max.tm_hour time_max.tm_min
  else
    Printf.sprintf "from %d/%d/%d %d:%d to %d/%d/%d %d:%d"
      time_min.tm_mday time_min.tm_mon (time_min.tm_year + 1900)
      time_min.tm_hour time_min.tm_min
      time_max.tm_mday time_max.tm_mon (time_max.tm_year + 1900)
      time_max.tm_hour time_max.tm_min

module Counter : sig
  type t
  val create : unit -> t
  val add : t -> float -> unit
  val estimate : t -> float
  val merge : into:t -> t -> unit
end = struct
  let precision = 600.0 (* 10 minutes per chunk *)

  type t = Hll.t
  let create () = Hll.make ~error:0.01

  let add t f =
    let n = Int64.of_float (f /. precision) in
    let n = Hll.hash_int64 n in
    Hll.add t n

  let estimate t = Hll.card t *. precision

  let merge ~into t = Hll.merge into t
end

let string_of_time_spent s =
  let s = int_of_float s in
  let m = s / 60 in
  let h = m / 60 in
  Printf.sprintf "%02d:%02d" h (m mod 60)

let rec print_tree offset depth0 counter0 tree =
  if tree.time_min <> infinity && tree.time_max <> neg_infinity then begin
    Printf.printf "%s%s\n" depth0 tree.sep;
    let depth = depth0 ^ "  " in
    let counter = Counter.create () in
    List.iter (fun entry ->
        Counter.add counter entry.time)
      tree.entries;
    List.iter (fun entry ->
        Counter.add counter entry.time;
        Printf.printf "%s%s\n"
          depth
          (if offset = 0 then entry.text else
             let len = String.length entry.text in
             String.sub entry.text offset (len - offset)))
      (List.sort_uniq compare_entry_text_only tree.entries);
    List.iter (fun child ->
        print_tree (offset + String.length child.sep + 1) depth counter child)
      tree.children;
    Counter.merge ~into:counter0 counter;
    match tree with
    | { children = [_]; entries = [] } -> ()
    | _ -> Printf.printf "%sSpent %s %s\n"
             depth0
             (string_of_time_spent (Counter.estimate counter))
             (string_of_tree_time tree);
  end

let rec short_print_tree offset = function
  | { children = [child]; entries = [] } ->
    short_print_tree (offset + String.length child.sep + 1) child
  | tree -> print_tree offset "" (Counter.create ()) tree

let main () =
  let args = [
    ("-min", Arg.Set_float min_date, "Enables verbose mode");
    ("-max", Arg.Set_float max_date, "Sets maximum number of files to list");
    ("-p", Arg.String ((+:=) paths), "Filter paths");
  ] in
  let usage_msg = "Format timetrail records" in
  Arg.parse args ((+:=) dataset) usage_msg;
  let dataset = match !dataset with
    | [] -> default_dataset ()
    | l -> l
  in
  let entries = List.fold_right loadinput dataset [] in
  let tree = make_tree entries in
  short_print_tree 0 tree;
  ()

let () = main ()

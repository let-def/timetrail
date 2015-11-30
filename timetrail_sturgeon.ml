open Common
open Sturgeon.Tui

module Time_tree : sig
  type 'a t = private {
    name     : 'a;
    time_min : float;
    time_max : float;
    entries  : entry list;
    children : 'a t list;
  }
  val make : 'a -> entry list -> 'a t list -> 'a t
end = struct
  type 'a t = {
    name     : 'a;
    time_min : float;
    time_max : float;
    entries  : entry list;
    children : 'a t list;
  }
  let make name entries children =
    let minmax_entry (t_min, t_max) entry =
      min t_min entry.time, max t_max entry.time
    and minmax_tree (t_min, t_max) tree =
      min t_min tree.time_min, max t_max tree.time_max
    in
    let time_minmax = infinity, neg_infinity in
    let time_minmax = List.fold_left minmax_entry time_minmax entries in
    let time_minmax = List.fold_left minmax_tree time_minmax children in
    let time_min, time_max = time_minmax in
    { name; time_min; time_max; entries; children }
end
open Time_tree

let make_tree l =
  let rec subtree offset name = function
    | [] -> Time_tree.make (name, ref false) [] []
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
          (fun (offset, name) subentries children ->
             subtree offset name !subentries :: children)
          table []
      and entries = !entries in
      Time_tree.make (name, ref false) entries
        (List.sort (fun t1 t2 -> compare t1.name t2.name) children)
  in
  subtree 0 "" l

let paths = ref (try [Sys.getenv "HOME"] with Not_found -> [])

let pass_filter entry =
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

let string_of_time_span time_min time_max =
  let open Unix in
  if max (abs_float time_min) (abs_float time_max) <> infinity then
    let time_min = localtime time_min and time_max = localtime time_max in
    if time_min.tm_year = time_max.tm_year &&
       time_min.tm_mon  = time_max.tm_mon  &&
       time_min.tm_mday = time_max.tm_mday then
      begin
        if time_min.tm_min = time_max.tm_min then
          Printf.sprintf "on %02d/%02d/%04d at %02d:%02d"
            time_min.tm_mday (time_min.tm_mon + 1) (time_min.tm_year + 1900)
            time_min.tm_hour time_min.tm_min
        else
          Printf.sprintf "on %02d/%02d/%04d between %02d:%02d and %02d:%02d"
            time_min.tm_mday (time_min.tm_mon + 1) (time_min.tm_year + 1900)
            time_min.tm_hour time_min.tm_min
            time_max.tm_hour time_max.tm_min
      end
    else
      Printf.sprintf "between %02d/%02d/%04d %02d:%02d and %02d/%02d/%04d %02d:%02d"
        time_min.tm_mday (time_min.tm_mon + 1) (time_min.tm_year + 1900)
        time_min.tm_hour time_min.tm_min
        time_max.tm_mday (time_max.tm_mon + 1) (time_max.tm_year + 1900)
        time_max.tm_hour time_max.tm_min
  else
   ""

let string_of_tree_time tree =
  string_of_time_span tree.time_min tree.time_max

let string_of_entries_time entries =
  let tmin = List.fold_left (fun t e -> min t e.time) infinity entries in
  let tmax = List.fold_left (fun t e -> max t e.time) neg_infinity entries in
  string_of_time_span tmin tmax

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

let rec annotate counter0 offset0 tree =
  let counter = Counter.create () in
  List.iter (fun entry ->
      Counter.add counter entry.time)
    tree.entries;
  let local = Counter.estimate counter in
  let offset = offset0 + String.length (fst tree.name) + 1 in
  let children = List.map (annotate counter offset) tree.children in
  let total = Counter.estimate counter in
  Counter.merge ~into:counter0 counter;
  Time_tree.make (tree.name, offset, local, total) tree.entries children

let annotate tree = annotate (Counter.create ()) (-1) tree

let rec print_node ui tree =
  let render ui' =
    let ((name, _), offset, local, total) = tree.name in
    List.iter (fun entry ->
        text (Tree.add ui') @@
        if offset = 0 then entry.text else
          let len = String.length entry.text in
          String.sub entry.text offset (len - offset)
      )
      (List.sort_uniq compare_entry_text_only tree.entries);
    if tree.entries <> [] && tree.children <> [] then
      printf (Tree.add ui')
        "- spent %s %s"
        (string_of_time_spent local)
        (string_of_entries_time tree.entries);
    List.iter (print_node ui') tree.children;
  in
  let ((name, opened), offset, local, total) = tree.name in
  printf (Tree.add ui ~opened ~children:render)
    "%s (spent %s %s)" name
    (string_of_time_spent total)
    (string_of_tree_time tree)

let print_node ui =
  let rec aux = function
    | { entries = []; children = [tree] } -> aux tree
    | tree -> print_node ui tree
  in
  aux

let months t =
  let months = Hashtbl.create 7 in
  let days = Hashtbl.create 7 in
  let add_time tm =
    let day = int_of_float (tm /. 60.0 /. 60.0 /. 24.0) in
    if not (Hashtbl.mem days day) then begin
      let open Unix in
      let tm = localtime tm in
      Hashtbl.add days day ();
      Hashtbl.replace months (tm.tm_year, tm.tm_mon) ()
    end
  in
  let update_entry entry = add_time entry.time in
  let rec update_tree t =
    List.iter update_entry t.entries;
    List.iter update_tree t.children
  in
  update_tree t;
  List.sort compare
    (Hashtbl.fold (fun (year,mon) () xs -> (year,mon) :: xs) months [])

let print_months k months f =
  let selected = ref None in
  let action month _ =
    if !selected = month then
      selected := None
    else
      selected := month;
    f !selected
  in
  let print (year,mon) =
    text k " [";
    link k (Printf.sprintf "%04d-%02d" (year + 1900) (mon + 1))
      (action (Some (year,mon)));
    text k "]"
  in
  List.iter print months;
  text k "\n"

let time_of = function
  | None -> (neg_infinity, infinity)
  | Some (tm_year, tm_mon) ->
    let open Unix in
    let tm = { Unix. tm_year; tm_mon; tm_mday = 1;
               tm_sec = 0; tm_min = 0; tm_hour = 0;
               tm_wday = 0; tm_yday = 0; tm_isdst = false } in
    let min, _ = Unix.mktime tm in
    let max, _ = Unix.mktime {tm with tm_mon = tm.tm_mon + 1; tm_mday = 1} in
    (min, max)

let filter_time (min,max) =
  let filter_entry entry =
    min <= entry.time && entry.time <= max in
  let filter_children tree' =
    not (tree'.time_max < min || max < tree'.time_min)
  in
  let rec filter_tree tree =
    let entries = List.filter filter_entry tree.entries in
    let children = List.filter filter_children tree.children in
    let children = List.map filter_tree children in
    Time_tree.make tree.name entries children
  in
  filter_tree

let main ~args:_ ~set_title cursor =
  let dataset = match !dataset with
    | [] -> default_dataset ()
    | l -> l
  in
  let entries = List.fold_right loadinput dataset [] in
  let tree = make_tree entries in
  let k_months = sub cursor in
  let widget = Tree.make cursor in
  print_months k_months (months tree) @@ fun month ->
  Tree.clear widget;
  let tree = filter_time (time_of month) tree in
  let tree = annotate tree in
  print_node widget tree

let () = Sturgeon.Recipes.text_command main

open Common

let drop x y =
  x.text = y.text && y.time <= x.time +. 300.

let filter_entries l =
  let rec f acc = function
    | [] -> acc
    | x :: y :: xs when drop x y ->
      f acc (x :: xs)
    | x :: xs ->
      f (x :: acc) xs
  in
  l |> List.sort compare_entry_text |> f [] |> List.sort compare_entry_time

let process_path p =
  let ic =
    attempt ("open trail file for input " ^ p) @@ fun () ->
    open_in p
  in
  let entries = load_entries ic [] in
  close_in_noerr ic;
  let entries = filter_entries entries in
  let oc =
    attempt ("open trail file for output " ^ p) @@ fun () ->
    open_out_gen [Open_creat; Open_wronly; Open_trunc] 0o660 p
  in
  save_entries oc entries;
  close_out_noerr oc

let main () =
  let args =
    if Array.length Sys.argv = 1 then
      [Lazy.force trail_path]
    else
      List.tl (Array.to_list Sys.argv)
  in
  List.iter (fun p ->
      if not (Sys.file_exists p) then
        Printf.eprintf "# Skipping %s, file doesn't not exist\n" p
      else
        recover ("filtering " ^ p) (fun () -> process_path p)
    ) args

let () = main ()

open Common

let lazy launch_time = launch_time
let lazy trail_path = trail_path

let trail_channel =
  attempt ("open trail file " ^ trail_path) @@ fun () ->
  open_out_gen [Open_creat; Open_append] 0o660 trail_path

let stamp = Printf.sprintf "%0.0f\t" launch_time

let process arg =
  let line = stamp ^ String.escaped arg ^ "\n" in
  output_string trail_channel line

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    process Sys.argv.(i)
  done

let () =
  attempt ("close trail file " ^ trail_path) @@ fun () ->
  close_out trail_channel

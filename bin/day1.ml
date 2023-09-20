let input =
  let read_line_opt () = try Some (read_line ()) with End_of_file -> None in

  let rec readall rest =
    match read_line_opt () with
    | Some line -> readall (line :: rest)
    | None -> rest
  in
  readall [] |> List.rev

let partial_sums =
  let rec next input output sum =
    match input with
    | [] -> sum :: output
    | num :: rest -> (
        match int_of_string_opt num with
        | Some num -> next rest output (sum + num)
        | None -> next rest (sum :: output) 0)
  in
  next input [] 0

let () = Printf.printf "Result: %d\n" (List.fold_left max min_int partial_sums)

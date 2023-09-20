let input =
  let read_line_opt () = try Some (read_line ()) with End_of_file -> None in

  let rec readall rest =
    match read_line_opt () with
    | Some line -> readall (line :: rest)
    | None -> rest
  in
  readall [] |> List.rev

let overlap_count =
  let parse_line line =
    let tuple_of_list = function
      | a :: b :: _ -> (a, b)
      | _ -> raise (Failure "")
    in
    let ranges = String.split_on_char ',' line in
    let range_of_string str =
      str |> String.split_on_char '-' |> List.map int_of_string |> tuple_of_list
    in
    (List.nth ranges 0 |> range_of_string, List.nth ranges 1 |> range_of_string)
  in
  let a_in_b ((a_1, a_2), (b_1, b_2)) = a_1 >= b_1 && a_2 <= b_2 in
  let overlaps (r1, r2) = a_in_b (r1, r2) || a_in_b (r2, r1) in
  List.filter (fun x -> parse_line x |> overlaps) input |> List.length

let () =
  Printf.printf "Total lines: %d\n" (List.length input);
  Printf.printf "Result: %d\n" overlap_count

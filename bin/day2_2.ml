let input =
  let read_line_opt () = try Some (read_line ()) with End_of_file -> None in

  let rec readall rest =
    match read_line_opt () with
    | Some line -> readall (line :: rest)
    | None -> rest
  in
  readall [] |> List.rev

let total_scores =
  let parse_line line =
    match String.split_on_char ' ' line with
    | opp :: me :: _ ->
        ( int_of_char opp.[0] - int_of_char 'A',
          int_of_char me.[0] - int_of_char 'X' )
    | _ -> raise (Invalid_argument "Not enough characters")
  in
  let my_choice (opp, res) =
    match res with
    | 0 ->
        (opp + 2) mod 3
        (* we lost *)
        (* mod is weird in ocaml !!! *)
    | 1 -> opp (* draw *)
    | 2 -> (opp + 1) mod 3 (* we won *)
    | _ -> raise (Invalid_argument "Invalid character")
  in
  let win_score res = res * 3 in
  let score (opp, res) = win_score res + my_choice (opp, res) + 1 in
  List.map (fun line -> line |> parse_line |> score) input

let () =
  Printf.printf "Result: %d\n" (List.fold_left ( + ) 0 total_scores);
  Printf.printf "Total lines: %d\n" (List.length total_scores)

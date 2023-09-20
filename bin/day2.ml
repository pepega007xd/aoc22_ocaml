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
  let win_score (opp, me) =
    match opp - me with
    | -2 | 1 -> 0 (* we lost *)
    | 0 -> 3 (* dwaw *)
    | -1 | 2 -> 6 (* we won *)
    | _ -> raise (Invalid_argument "Invalid character")
  in
  let choice_score me = me + 1 in
  let score (opp, me) = win_score (opp, me) + choice_score me in
  List.map (fun line -> line |> parse_line |> score) input

let () =
  Printf.printf "Result: %d\n" (List.fold_left ( + ) 0 total_scores);
  Printf.printf "Total lines: %d\n" (List.length total_scores)

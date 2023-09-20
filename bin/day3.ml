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
    let half_len = String.length line / 2 in
    let seq = String.to_seq line in
    ( seq |> Seq.take half_len |> List.of_seq,
      seq |> Seq.drop half_len |> List.of_seq )
  in
  let contains_letter lst l =
    List.find_opt (Char.equal l) lst |> Option.is_some
  in
  let common_letter (a, b) = List.find (contains_letter a) b in
  let letter_score l =
    match l with
    | 'A' .. 'Z' -> int_of_char l - int_of_char 'A' + 27
    | 'a' .. 'z' -> int_of_char l - int_of_char 'a' + 1
    | _ -> raise (Invalid_argument "Invalid letter")
  in
  List.map
    (fun line -> line |> parse_line |> common_letter |> letter_score)
    input

let () =
  Printf.printf "Total lines: %d\n" (List.length total_scores);
  Printf.printf "Result: %d\n" (List.fold_left ( + ) 0 total_scores)

let input =
  let read_line_opt () = try Some (read_line ()) with End_of_file -> None in

  let rec readall rest =
    match read_line_opt () with
    | Some line -> readall (line :: rest)
    | None -> rest
  in
  readall [] |> List.rev

let total_scores =
  let rec group_lines input =
    match input with
    | a :: b :: c :: rest -> (a, b, c) :: group_lines rest
    | [] -> []
    | _ -> raise (Invalid_argument "Line count not divisible by 3")
  in
  let parse_line line =
    let len = String.length line in
    String.to_seq line |> Seq.take len |> List.of_seq
  in
  let contains_letter lst l =
    List.find_opt (Char.equal l) lst |> Option.is_some
  in
  let common_letter (a, b, c) =
    List.filter (contains_letter a) b |> List.find (contains_letter c)
  in
  let letter_score l =
    match l with
    | 'A' .. 'Z' -> int_of_char l - int_of_char 'A' + 27
    | 'a' .. 'z' -> int_of_char l - int_of_char 'a' + 1
    | _ -> raise (Invalid_argument "Invalid letter")
  in
  let groups = List.map parse_line input |> group_lines in
  List.map (fun g -> common_letter g |> letter_score) groups

let () =
  Printf.printf "Total lines: %d\n" (List.length total_scores);
  Printf.printf "Result: %d\n" (List.fold_left ( + ) 0 total_scores)

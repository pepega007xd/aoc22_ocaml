let input =
  let read_line_opt () = try Some (read_line ()) with End_of_file -> None in

  let rec readall rest =
    match read_line_opt () with
    | Some line -> readall (line :: rest)
    | None -> rest
  in
  readall [] |> List.rev

type instr = { src : int; dst : int; count : int }
type stacks = char list list

let parse_input lines =
  let inst_lines, stack_lines =
    List.partition (String.starts_with ~prefix:"move") lines
  in
  let instrs =
    List.map
      (fun l ->
        Scanf.sscanf l "move %d from %d to %d" (fun src dst count ->
            { src; dst; count }))
      inst_lines
  in
  let stacks: stacks = 

let do_instr (stacks : stacks) instr : stacks =
  let split_pop lst n =
    let rec split_pop_inner pop rest n =
      match (rest, n) with
      | _, 0 -> (pop, rest)
      | h :: t, n -> split_pop_inner (h :: rest) t (n - 1)
      | _, _ -> raise (Invalid_argument "list is too short")
    in
    split_pop_inner [] lst n
  in
  let pop, rest = split_pop (List.nth stacks instr.src) instr.count in
  let rec assemble new_lst old_lst =
    match old_lst with
    | [] -> new_lst
    | h :: t ->
        if List.length new_lst == instr.src - 1 then
          assemble (rest :: new_lst) t
        else if List.length new_lst == instr.dst - 1 then
          assemble ((pop @ h) :: new_lst) t
        else assemble (h :: new_lst) t
  in
  assemble [] stacks

let () =
  Printf.printf "Total lines: %d\n" (List.length input);
  Printf.printf "Result: %d\n" overlap_count

open Printf

let merge_ranges ranges =
  let sorted = List.sort (fun (a, _) (b, _) -> compare a b) ranges in
  match sorted with
  | [] -> []
  | (start, finish) :: rest ->
      let rec merge acc current_start current_finish = function
        | [] -> (current_start, current_finish) :: acc
        | (s, f) :: rest ->
            if s <= current_finish + 1 then
              merge acc current_start (max current_finish f) rest
            else
              merge ((current_start, current_finish) :: acc) s f rest
      in
      List.rev (merge [] start finish rest)

let () =
  let ranges = ref [] in
  let ids = ref [] in
  let in_ranges = ref true in
  
  (try
     while true do
       let line = input_line stdin in
       let line = String.trim line in
       if String.length line = 0 then
         in_ranges := false
       else if !in_ranges then begin
         match String.split_on_char '-' line with
         | [start_str; end_str] ->
             let start = int_of_string (String.trim start_str) in
             let finish = int_of_string (String.trim end_str) in
             ranges := (start, finish) :: !ranges
         | _ -> ()
       end else begin
         let id = int_of_string line in
         ids := id :: !ids
       end
     done
   with End_of_file -> ());
  
  let ranges = List.rev !ranges in
  let ids = List.rev !ids in
  
  (* Part 1: Count how many available IDs are fresh *)
  let is_fresh id =
    List.exists (fun (start, finish) -> id >= start && id <= finish) ranges
  in
  
  let fresh_count = ref 0 in
  List.iter (fun id ->
    if is_fresh id then incr fresh_count
  ) ids;
  
  (* Part 2: Count all unique IDs that are in any range *)
  let merged_ranges = merge_ranges ranges in
  let total_fresh_ids = ref 0 in
  List.iter (fun (start, finish) ->
    total_fresh_ids := !total_fresh_ids + (finish - start + 1)
  ) merged_ranges;
  
  printf "Part 1: %d\n" !fresh_count;
  printf "Part 2: %d\n" !total_fresh_ids


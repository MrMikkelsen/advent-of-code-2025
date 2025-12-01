open Printf

let () =
  let lines = ref [] in
  (try
     while true do
       let line = input_line stdin in
       let line = String.trim line in
       if String.length line > 0 then
         lines := line :: !lines
     done
   with End_of_file -> ());
  let lines = List.rev !lines in
  
  let position1 = ref 50 in
  let zero_count1 = ref 0 in
  List.iter (fun line ->
    let dir = line.[0] in
    let n =
      int_of_string
        (String.sub line 1 (String.length line - 1))
    in
    let step = n mod 100 in
    let delta =
      match dir with
      | 'L' -> -step
      | 'R' ->  step
      | _   -> failwith "wrong direction"
    in
    position1 := (!position1 + delta + 100) mod 100;
    if !position1 = 0 then incr zero_count1
  ) lines;
  
  let position2 = ref 50 in
  let zero_count2 = ref 0 in
  List.iter (fun line ->
    let dir = line.[0] in
    let n =
      int_of_string
        (String.sub line 1 (String.length line - 1))
    in
    let step = n mod 100 in
    let delta =
      match dir with
      | 'L' -> -step
      | 'R' ->  step
      | _   -> failwith "wrong direction"
    in
    let start = !position2 in
    let new_pos = (start + delta + 100) mod 100 in
    let full_delta = match dir with 'L' -> -n | 'R' -> n | _ -> 0 in
    if full_delta <> 0 then begin
      let abs_full_delta = abs full_delta in
      for i = 1 to abs_full_delta do
        let pos = if full_delta > 0 then (start + i) mod 100 else (start - i + 100) mod 100 in
        if pos = 0 then incr zero_count2
      done
    end;
    position2 := new_pos
  ) lines;
  
  printf "Part 1: %d\n" !zero_count1;
  printf "Part 2: %d\n" !zero_count2



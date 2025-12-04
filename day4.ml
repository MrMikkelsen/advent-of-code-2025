open Printf

(* Count adjacent rolls in 8 directions on an immutable string grid *)
let count_adjacent_rolls_strings grid row col =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  let count = ref 0 in
  for dr = -1 to 1 do
    for dc = -1 to 1 do
      if dr <> 0 || dc <> 0 then begin
        let r = row + dr in
        let c = col + dc in
        if r >= 0 && r < rows && c >= 0 && c < cols then
          if grid.(r).[c] = '@' then incr count
      end
    done
  done;
  !count

(* Count adjacent rolls in 8 directions on a mutable char grid *)
let count_adjacent_rolls_chars grid row col =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let count = ref 0 in
  for dr = -1 to 1 do
    for dc = -1 to 1 do
      if dr <> 0 || dc <> 0 then begin
        let r = row + dr in
        let c = col + dc in
        if r >= 0 && r < rows && c >= 0 && c < cols then
          if grid.(r).(c) = '@' then incr count
      end
    done
  done;
  !count

let () =
  (* Read grid as list of lines *)
  let lines_rev = ref [] in
  (try
     while true do
       let line = input_line stdin in
       let line = String.trim line in
       if String.length line > 0 then
         lines_rev := line :: !lines_rev
     done
   with End_of_file -> ());
  let grid_strings = Array.of_list (List.rev !lines_rev) in
  let rows = Array.length grid_strings in
  if rows = 0 then (
    printf "Part 1: 0\n";
    printf "Part 2: 0\n"
  ) else begin
    let cols = String.length grid_strings.(0) in

    (* Part 1: count accessible rolls in the original grid *)
    let accessible = ref 0 in
    for row = 0 to rows - 1 do
      for col = 0 to cols - 1 do
        if grid_strings.(row).[col] = '@' then begin
          let adjacent_count = count_adjacent_rolls_strings grid_strings row col in
          if adjacent_count < 4 then incr accessible
        end
      done
    done;

    (* Build mutable char grid for Part 2 simulation *)
    let grid_chars =
      Array.init rows (fun r ->
        Array.of_seq (String.to_seq grid_strings.(r)))
    in

    (* Part 2: repeatedly remove all currently accessible rolls *)
    let total_removed = ref 0 in
    let changed = ref true in
    while !changed do
      changed := false;
      let to_remove = ref [] in
      for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
          if grid_chars.(r).(c) = '@' then begin
            let adj = count_adjacent_rolls_chars grid_chars r c in
            if adj < 4 then
              to_remove := (r, c) :: !to_remove
          end
        done
      done;
      match !to_remove with
      | [] -> ()
      | lst ->
          changed := true;
          List.iter
            (fun (r, c) ->
               if grid_chars.(r).(c) = '@' then (
                 grid_chars.(r).(c) <- '.';
                 incr total_removed))
            lst
    done;

    printf "Part 1: %d\n" !accessible;
    printf "Part 2: %d\n" !total_removed
  end



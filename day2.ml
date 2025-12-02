open Printf

let is_invalid_id n =
  let s = string_of_int n in
  let len = String.length s in
  let rec check_pattern pattern_len =
    if pattern_len > len / 2 then false
    else if len mod pattern_len <> 0 then check_pattern (pattern_len + 1)
    else begin
      let num_parts = len / pattern_len in
      if num_parts < 2 then check_pattern (pattern_len + 1)
      else begin
        let pattern = String.sub s 0 pattern_len in
        let all_same = ref true in
        for i = 1 to num_parts - 1 do
          let part = String.sub s (i * pattern_len) pattern_len in
          if part <> pattern then all_same := false
        done;
        if !all_same then true
        else check_pattern (pattern_len + 1)
      end
    end
  in
  check_pattern 1

let () =
  let line = input_line stdin in
  let ranges = String.split_on_char ',' line in
  let total = ref 0 in
  List.iter (fun range_str ->
    let range_str = String.trim range_str in
    if String.length range_str > 0 then begin
      match String.split_on_char '-' range_str with
      | [start_str; end_str] ->
          let start = int_of_string (String.trim start_str) in
          let finish = int_of_string (String.trim end_str) in
          for id = start to finish do
            if is_invalid_id id then
              total := !total + id
          done
      | _ -> ()
    end
  ) ranges;
  printf "%d\n" !total


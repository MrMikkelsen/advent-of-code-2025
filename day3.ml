open Printf

let max_joltage bank =
  let len = String.length bank in
  if len < 2 then 0
  else begin
    let max_val = ref 0 in
    for i = 0 to len - 2 do
      for j = i + 1 to len - 1 do
        let digit1 = (int_of_char bank.[i]) - (int_of_char '0') in
        let digit2 = (int_of_char bank.[j]) - (int_of_char '0') in
        let joltage = digit1 * 10 + digit2 in
        if joltage > !max_val then max_val := joltage
      done
    done;
    !max_val
  end

let max_joltage_12 bank =
  let len = String.length bank in
  let k = 12 in
  if len < k then 0
  else begin
    let result = ref [] in
    let last_pos = ref (-1) in
    for pos = 0 to k - 1 do
      let start = !last_pos + 1 in
      let remaining = k - pos in
      let end_pos = len - remaining in
      let max_digit = ref (-1) in
      let max_pos = ref start in
      for i = start to end_pos do
        let digit = (int_of_char bank.[i]) - (int_of_char '0') in
        if digit > !max_digit then begin
          max_digit := digit;
          max_pos := i
        end
      done;
      result := !max_digit :: !result;
      last_pos := !max_pos
    done;
    let num = ref 0 in
    List.iter (fun digit ->
      num := !num * 10 + digit
    ) (List.rev !result);
    !num
  end

let () =
  let total1 = ref 0 in
  let total2 = ref 0 in
  (try
     while true do
       let line = input_line stdin in
       let line = String.trim line in
       if String.length line >= 2 then
         total1 := !total1 + max_joltage line;
       if String.length line >= 12 then
         total2 := !total2 + max_joltage_12 line
     done
   with End_of_file -> ());
  printf "Part 1: %d\n" !total1;
  printf "Part 2: %d\n" !total2


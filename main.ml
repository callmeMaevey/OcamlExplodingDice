(* Attempting to learn OCAML *)
let numSides = 6

let rec sum listofRolls = 
  match listofRolls with 
  | h :: t -> h + (sum t)
  | [] -> 0 

let rec rollXtimes (x:int) : int list =
  if x = 0 then []
  else
    let rec roll () =
        let curr = ( Random.int numSides ) + 1 in
        if curr = numSides
        then curr :: roll ()
        else [curr]
    in
    let rec prepend a b =
      match a with
      | h :: t -> h :: prepend t b 
      | [] -> b
    in
    prepend (roll()) (rollXtimes (x - 1))

let _ =
  Random.self_init();
  let printResults input =
    let rec print_list = function 
    | [] -> ()
    | h::t -> 
      print_int h ;
      print_char ' ' ;
      print_list t
    in
    print_list input;
    print_newline ();
    print_int (sum input)
  in
  let rollList = rollXtimes 10 in
  printResults rollList

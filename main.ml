let rec roll numSides=
  let curr = Random.int numSides in
  match curr with
  6 -> curr :: roll numSides
  | _ -> [curr]  
 

let rec sum listofRolls = 
  match listofRolls with 
  | [] -> 0 
  | h :: t -> h + (sum t);;

let rec append (a:int list) (b: int list) : int list =
  match a with
  | [] -> b
  | h::t -> h::append t b 

let rec rollXtimes (x: int) : int list =
  if x = 0 then []
  else 
    let value : int list = roll 6 in
    let recur : int list = rollXtimes (x-1) in
    append value recur


let rec print_list = function 
  | [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l

let _ = 
  let rollList = rollXtimes 20 in
  print_list rollList;
  print_newline ();
  print_int (sum rollList)

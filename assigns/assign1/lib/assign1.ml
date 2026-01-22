
let sqrt (_n : int) : int = (* CHANGE _n to n! *)
  assert false

let pow (_n : int) (_k : int) : int = (* CHANGE _n to n and _k to k! *)
  assert false

let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let string_of_char (c : char) : string =
  String.init 1 (fun _ -> c)

let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then List.rev acc
    else loop (String.get s i :: acc) (i + 1)
  in loop [] 0

let implode (cs : char list) : string =
  String.init
    (List.length cs)
    (fun i -> List.nth cs i)

let implode_all (css : char list list) : string list =
  let rec loop acc css =
    match css with
    | [] -> List.rev acc
    | cs :: rest -> loop (implode cs :: acc) rest
  in loop [] css

let split_on_ws_helper (_cs : char list) : char list list =
  assert false

let split_on_ws (s : string) : string list =
  implode_all (split_on_ws_helper (explode s))

let eval (_stack : int list) (_prog : string list) : int list =
  assert false

let interp (input : string) : int =
  match eval [] (split_on_ws input) with
  | [output] -> output
  | _ -> failwith "whoops!"

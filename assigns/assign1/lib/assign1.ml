
let sqrt (n : int) : int = (* CHANGE _n to n! *)
  let rec help k =
    if k * k >= n 
    then k
    else help(k+1)
  in 
  help(0)


let pow (n : int) (k : int) : int = (* CHANGE _n to n and _k to k! *)
  if k < 0 then
    0
  else
    let rec help exp acc =
      if exp = 0 then
        acc
      else
        help (exp - 1) (acc * n)
    in
    help k 1


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
    
    let rec loop chars current result =
      match chars with
      | [] -> 
      if current = [] then
        List.rev result
      else
         List.rev ((List.rev current) :: result)
      | x :: xs -> 
        if is_ws x then
          if current = [] then
            loop xs [] result
          else
            loop xs [] ((List.rev current) :: result)
        else
          loop xs (x :: current) result
      in 
        loop _cs [] []
    


let split_on_ws (s : string) : string list =
  implode_all (split_on_ws_helper (explode s))

let eval (_stack : int list) (_prog : string list) : int list =
  let rec loop stack prog =
    match prog with
    | [] -> stack
    | tok :: rest ->
        match tok with
        | "+" ->
            (match stack with
             | a :: b :: s' -> loop ((b + a) :: s') rest
             | _ -> failwith "stack underflow")
        | "-" ->
            (match stack with
             | a :: b :: s' -> loop ((b - a) :: s') rest
             | _ -> failwith "stack underflow")
        | "*" ->
            (match stack with
             | a :: b :: s' -> loop ((b * a) :: s') rest
             | _ -> failwith "stack underflow")
        | "/" ->
            (match stack with
             | a :: b :: s' -> loop ((b / a) :: s') rest
             | _ -> failwith "stack underflow")
        | "mod" ->
            (match stack with
             | a :: b :: s' -> loop ((b mod a) :: s') rest
             | _ -> failwith "stack underflow")
        | "sqrt" ->
            (match stack with
             | a :: s' -> loop ((sqrt a) :: s') rest
             | _ -> failwith "stack underflow")
        | "^" ->
            (match stack with
             | a :: b :: s' -> loop ((pow b a) :: s') rest
             | _ -> failwith "stack underflow")
        | _ ->
            (* treat it as an integer token *)
            let n = int_of_string tok in
            loop (n :: stack) rest
  in
  loop _stack _prog
 

let interp (input : string) : int =
  match eval [] (split_on_ws input) with
  | [output] -> output
  | _ -> failwith "whoops!"

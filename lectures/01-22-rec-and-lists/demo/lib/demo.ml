
let sos x y =
  let xsq = x *. x in
  let ysq = y *. y in
  xsq +. ysq

(*
   def fib(n):
       if n == 0 or n == 1:
           return n
       return fib (n - 1) * fib (n - 2)
   *)

let rec fib (n : int) : int =
  if n = 0 || n = 1
  then n
  else fib (n - 1) + fib (n - 2)

(*
   def fib(n):
       curr = 0
       next = 1
       for i in range(n):
           (curr, next) = (next, curr + next)
       return curr
   *)

let rec fib_loop n curr next i =
  if i = n
  then curr
  else fib_loop n next (curr + next) (i + 1)

let fib2 n = fib_loop n 0 1 0

let fib3 n =
  let rec loop curr next i =
    if i = n
    then curr
    else loop next (curr + next) (i + 1)
  in loop 0 1 0

let generate (n : int) : int list =
  let rec loop acc i =
    if i > n
    then List.rev acc
    else loop (i :: acc) (i + 1)
  in loop [] 1

let rec double (l : int list) : int list =
  match l with
  | [] -> []
  | x :: xs -> (2 * x) :: double xs

let rec every_other l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x :: _ :: xs -> x :: every_other xs

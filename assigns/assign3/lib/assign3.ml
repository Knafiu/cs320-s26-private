
let is_ws c =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let is_upper c =
  let c = int_of_char c in
  65 <= c && c <= 90

let all_upper x =
  let rec loop i =
    if i >= String.length x
    then true
    else if not (is_upper x.[i])
    then false
    else loop (i + 1)
  in loop 0

let lex (s : string) : string list =
    let n = String.length s in
    let rec read_while pred i j =
      if j < n && pred s.[j] then read_while pred i (j + 1) else j
    in
    let rec go acc i =
      if i >= n then List.rev acc
      else if is_ws s.[i] then go acc (i + 1)
      else
        match s.[i] with
        | '(' | ')' | '+' | '-' | '*' | '/' | '=' as ch ->
            go ((String.make 1 ch) :: acc) (i + 1)
        | c when is_digit c ->
            let j = read_while is_digit i i in
            let tok = String.sub s i (j - i) in
            go (tok :: acc) j
        | c when is_upper c ->
            let j = read_while is_upper i i in
            let tok = String.sub s i (j - i) in
            go (tok :: acc) j
        | _ ->
            (* Undefined behavior per spec; we can fail fast. *)
            failwith "lex: unexpected character"
    in
    go [] 0

let rec eval (env : (string * int) list) (expr : string list) : int =
  assert false (* TODO *)

let insert_uniq (k : 'k) (v : 'v) (r : ('k * 'v) list) : ('k * 'v) list =
  assert false (* TODO *)

let interp (input : string) (env : (string * int) list) : int * (string * int) list =
  match lex input with
  | var :: "=" :: expr -> (
    match eval env expr with
    | ouput -> ouput, insert_uniq var ouput env
    | exception _ -> failwith "whoops!"
  )
  | _
  | exception _ -> failwith "whoops!"

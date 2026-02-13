
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
            go (tok ::cc) j
        | _ ->
            failwith "lex: unexpected character"
    in
    go [] 0

let rec eval (env : (string * int) list) (expr : string list) : int =
   let precedence op =
    match op with
    | "+" | "-" -> 1
    | "*" | "/" -> 2
    | _ -> 0
  in

  let is_op t =
    t = "+" || t = "-" || t = "*" || t = "/"
  
    let rec to_rpn tokens ops output =
    match tokens with
    | [] ->
        let rec drain ops output =
          match ops with
          | [] -> List.rev output
          | op :: rest -> drain rest (op :: output)
        in
        drain ops output
    | t :: rest ->
        if t = "(" then
          to_rpn rest (t :: ops) output
        else if t = ")" then
          let rec pop_until ops output =
            match ops with
            | [] -> failwith "mismatched parentheses"
            | "(" :: ops' -> to_rpn rest ops' output
            | op :: ops' -> pop_until ops' (op :: output)
          in
          pop_until ops output
        else if is_op t then
          let rec pop_ops ops output =
            match ops with
            | op :: ops'
              when is_op op && precedence op >= precedence t ->
                pop_ops ops' (op :: output)
            | _ -> to_rpn rest (t :: ops) output
          in
          pop_ops ops output
        else
          to_rpn rest ops (t :: output)
          in

        let rpn = to_rpn expr [] [] in

        let rec eval_rpn stack tokens =
          match tokens with
          | [] ->
              (match stack with
              | [v] -> v
              | _ -> failwith "invalid expression")
          | t :: rest ->
              if is_op t then
                match stack with
                | b :: a :: stack' ->
                    let result =
                      match t with
                      | "+" -> a + b
                      | "-" -> a - b
                      | "*" -> a * b
                      | "/" -> a / b
                      | _ -> failwith "unknown operator"
                    in
                    eval_rpn (result :: stack') rest
                | _ -> failwith "not enough operands"
              else
                let value =
                  if String.length t > 0 && is_digit t.[0] then
                    int_of_string t
                  else
                    List.assoc t env
                in
                eval_rpn (value :: stack) rest
        in
        eval_rpn [] rpn

let insert_uniq (k : 'k) (v : 'v) (r : ('k * 'v) list) : ('k * 'v) list =
  

let interp (input : string) (env : (string * int) list) : int * (string * int) list =
  match lex input with
  | var :: "=" :: expr -> (
    match eval env expr with
    | ouput -> ouput, insert_uniq var ouput env
    | exception _ -> failwith "whoops!"
  )
  | _
  | exception _ -> failwith "whoops!"

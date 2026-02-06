
let is_ws c =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let lex s =
  let rec go acc i =
    let rec go_digits acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_digit (String.get s (i + j))
      then go_digits acc i (j + 1)
      else go (String.sub s i j :: acc) (i + j)
    in
    if i >= String.length s
    then List.rev acc
    else
      match String.get s i with
      | '+' -> go ("+" :: acc) (i + 1)
      | '-' -> go ("-" :: acc) (i + 1)
      | '*' -> go ("*" :: acc) (i + 1)
      | '/' -> go ("/" :: acc) (i + 1)
      | '(' -> go ("(" :: acc) (i + 1)
      | ')' -> go (")" :: acc) (i + 1)
      | c ->
        if is_digit c
        then go_digits acc i 1
        else if is_ws c
        then go acc (i + 1)
        else assert false
  in go [] 0

let eval e =
  let rec parse_expr toks =
    let (v, toks) = parse_term toks in
    parse_expr_tail v toks

  and parse_expr_tail acc toks =
    match toks with
    | "+" :: rest ->
        let (v2, rest2) = parse_term rest in
        parse_expr_tail (acc + v2) rest2
    | "-" :: rest ->
        let (v2, rest2) = parse_term rest in
        parse_expr_tail (acc - v2) rest2
    | _ ->
        (acc, toks)

  and parse_term toks =
    let (v, toks) = parse_factor toks in
    parse_term_tail v toks

  and parse_term_tail acc toks =
    match toks with
    | "*" :: rest ->
        let (v2, rest2) = parse_factor rest in
        parse_term_tail (acc * v2) rest2
    | "/" :: rest ->
        let (v2, rest2) = parse_factor rest in
        parse_term_tail (acc / v2) rest2
    | _ ->
        (acc, toks)
  and parse_factor toks =
    match toks with
    | n :: rest when
        n <> "(" && n <> ")" &&
        n <> "+" && n <> "-" &&
        n <> "*" && n <> "/" ->
        (int_of_string n, rest)
    | "(" :: rest ->
    let (v, rest2) = parse_expr rest in
    (match rest2 with
     | ")" :: rest3 -> (v, rest3)
     | _ -> failwith "missing )")
    | _ ->
        failwith "bad factor"
  in
  let (v, rest) = parse_expr e in
  match rest with
  | [] -> v
  | _ -> failwith "extra tokens"


  
let interp (input : string) : int =
  match eval (lex input) with
  | output -> output
  | exception _ -> failwith "whoops!"

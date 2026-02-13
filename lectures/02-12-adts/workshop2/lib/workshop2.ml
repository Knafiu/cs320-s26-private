
(* Problem 1 *)

type 'a tree =
  | Leaf of 'a
  | Node2 of 'a * 'a tree * 'a tree
  | Node3 of 'a * 'a tree * 'a tree * 'a tree
  
let reverse (_t : 'a tree) : 'a tree =
  match t with
  | Leaf x -> Leaf x
  | Node2 (v, t1, t2) ->
      Node2 (v, reverse t2, reverse t1)
  | Node3 (v, t1, t2, t3) ->
      Node3 (v, reverse t3, reverse t2, reverse t1)

(* Problem 2 *)

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let split_at (k : int) (s : string) : string * string =
  if k < 0
  then "", s
  else if k > String.length s
  then s, ""
  else String.sub s 0 k, String.sub s k (String.length s - k)

let get_int (_s : string) : (int * string) option =
    let len = String.length s in
    if len = 0 then None
    else
      (* allow an optional leading '-' *)
      let start =
        if s.[0] = '-' then 1 else 0
      in
      let rec scan i =
        if i < len && is_digit s.[i] then scan (i + 1) else i
      in
      let stop = scan start in
      (* must have at least one digit *)
      if stop = start then None
      else
        let num_str, rest = split_at stop s in
        Some (int_of_string num_str, rest)

(* Problem 3 *)

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Exp of expr * expr

type error =
  | DivByZero
  | NegExp

let int_pow m n =
  let rec go acc i =
    if i <= 0
    then acc
    else go (m * acc) (i - 1)
  in go 1 n

let rec eval (_e : expr) : (int, error) result =
    match e with
  | Int n -> Ok n

  | Add (e1, e2) ->
      (match eval e1 with
       | Error err -> Error err
       | Ok v1 ->
           match eval e2 with
           | Error err -> Error err
           | Ok v2 -> Ok (v1 + v2))

  | Sub (e1, e2) ->
      (match eval e1 with
       | Error err -> Error err
       | Ok v1 ->
           match eval e2 with
           | Error err -> Error err
           | Ok v2 -> Ok (v1 - v2))

  | Mul (e1, e2) ->
      (match eval e1 with
       | Error err -> Error err
       | Ok v1 ->
           match eval e2 with
           | Error err -> Error err
           | Ok v2 -> Ok (v1 * v2))

  | Div (e1, e2) ->
      (match eval e1 with
       | Error err -> Error err
       | Ok v1 ->
           match eval e2 with
           | Error err -> Error err
           | Ok 0 -> Error DivByZero
           | Ok v2 -> Ok (v1 / v2))

  | Exp (e1, e2) ->
      (match eval e1 with
       | Error err -> Error err
       | Ok base ->
           match eval e2 with
           | Error err -> Error err
           | Ok exp when exp < 0 -> Error NegExp
           | Ok exp -> Ok (int_pow base exp))

(* Problem 4 *)

type content_type =
  | PlainText
  | Html 
  | Pdf
  | Png

type content_encoding =
  | Base64
  | QuotePrintable
  | Binary

type header =
  {
    content_type : content_type;
    content_encoding : content_encoding;
  }

type 'a email =
  | Attachment of header * 'a
  | Body of header * string
  | Multipart of (header * 'a email option) list

let get_attachments (_t : content_type) (_e : 'a email) : 'a list =
  assert false

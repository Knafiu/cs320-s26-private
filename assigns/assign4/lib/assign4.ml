
let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

type token = Lpar | Rpar | Word of string

let ( let* ) o f = match o with Some x -> f x | None -> None

let tokens_of_string (s : string) : token list =
  let rec go acc i =
    if i >= String.length s
    then acc
    else
      match s.[i] with
      | '(' -> go (Lpar :: acc) (i + 1)
      | ')' -> go (Rpar :: acc) (i + 1)
      | c ->
        if is_ws c
        then go acc (i + 1)
        else
          let rec go' j =
            if i + j >= String.length s
            then Word (String.sub s i j) :: acc
            else
              let c = s.[i + j] in
              if List.mem c ['('; ')'] || is_ws c
              then go (Word (String.sub s i j) :: acc) (i + j)
              else go' (j + 1)
          in go' 1
  in List.rev (go [] 0)

type sexpr =
  | Atom of string
  | List of sexpr list

let sexpr_of_tokens_opt (ts : token list) : sexpr option =
  let rec go (ts : token list) : (sexpr * token list) option =
    match ts with
    | [] -> None
    | t :: ts -> (
        match t with
        | Word a -> Some (Atom a, ts)
        | Lpar -> (
            match go' ts with
            | es, Rpar :: ts -> Some (List es, ts)
            | _ -> None
          )
        | Rpar -> None
      )
  and go' (ts : token list) : sexpr list * token list =
    match go ts with
    | Some (e, ts) ->
      let (es, ts) = go' ts in
      e :: es, ts
    | None -> [], ts
  in
  match go ts with
  | Some (e, []) -> Some e
  | _ -> None

let sexpr_of_string_opt (s : string) : sexpr option =
  sexpr_of_tokens_opt (tokens_of_string s)

let rec string_of_sexpr (e : sexpr) : string =
  match e with
  | Atom s -> s
  | List ss -> "(" ^ String.concat " " (List.map string_of_sexpr ss) ^ ")"

type op = Add | Mul | Eq

let string_of_op (op : op) : string =
  match op with
  | Add -> "+"
  | Mul -> "*"
  | Eq -> "="

let op_of_sexpr_opt (s : sexpr) : op option =
  match s with
  | Atom "+" -> Some Add
  | Atom "*" -> Some Mul
  | Atom "=" -> Some Eq
  | _ -> None

type expr =
  | Int of int
  | Bop of op * expr * expr
  | If of expr * expr * expr

let rec expr_of_sexpr_opt (s : sexpr) : expr option =
  match s with
  | Atom a -> (
      match int_of_string_opt a with
      | Some n -> Some (Int n)
      | None -> None
    )
  | List xs -> (
      match xs with
      | [op_s; e1_s; e2_s] -> (
          match op_of_sexpr_opt op_s with
          | None -> None
          | Some op -> (
              match expr_of_sexpr_opt e1_s, expr_of_sexpr_opt e2_s with
              | Some e1, Some e2 -> Some (Bop (op, e1, e2))
              | _ -> None
            )
        )
      | [Atom "if"; c_s; t_s; f_s] -> (
          match expr_of_sexpr_opt c_s, expr_of_sexpr_opt t_s, expr_of_sexpr_opt f_s with
          | Some c, Some t, Some f -> Some (If (c, t, f))
          | _ -> None
        )
      | _ -> None
    )

let expr_of_string_opt (s : string) : expr option =
  match sexpr_of_string_opt s with
  | None -> None
  | Some sx -> expr_of_sexpr_opt sx

let rec sexpr_of_expr (e : expr) : sexpr =
  match e with
  | Int n -> Atom (string_of_int n)
  | Bop (op, e1, e2) ->
    List [Atom (string_of_op op); sexpr_of_expr e1; sexpr_of_expr e2]
  | If (c, t, f) ->
    List [Atom "if"; sexpr_of_expr c; sexpr_of_expr t; sexpr_of_expr f]

let string_of_expr (e : expr) : string =
  string_of_sexpr (sexpr_of_expr e)

type ty = BoolT | IntT

let ty_of_sexpr_opt (e : sexpr) : ty option =
  match e with
  | Atom s -> (
      match s with
      | "bool" -> Some BoolT
      | "int" -> Some IntT
      | _ -> None
    )
  | _ -> None

let string_of_ty (ty : ty) : string =
  match ty with
  | BoolT -> "bool"
  | IntT -> "int"

type ty_jmt =
  {
    expr : expr;
    ty : ty;
  }

let string_of_ty_jmt (j : ty_jmt) : string =
  string_of_expr j.expr ^ " : " ^ string_of_ty j.ty

type ty_rule =
  | Int_lit
  | Add_int
  | Mul_int
  | Eq_rule
  | If_rule

let string_of_ty_rule (r : ty_rule) =
  match r with
  | Int_lit -> "intLit"
  | Add_int -> "addInt"
  | Mul_int -> "mulInt"
  | Eq_rule -> "eq"
  | If_rule -> "if"

let ty_rule_of_sexpr_opt (e : sexpr) : ty_rule option =
  match e with
  | Atom s -> (
      match s with
      | "INTLIT" -> Some Int_lit
      | "ADDINT" -> Some Add_int
      | "MULINT" -> Some Mul_int
      | "EQ" -> Some Eq_rule
      | "IF" -> Some If_rule
      | _ -> None
    )
  | _ -> None

type ty_deriv =
  | Rule_app of {
      prem_derivs : ty_deriv list;
      concl : ty_jmt;
      rname : ty_rule;
    }
  | Hole

let rec ty_deriv_of_sexpr_opt (s : sexpr) : ty_deriv option =
  match s with
  | Atom "???" -> Some Hole
  | Atom _ -> None
  | List xs -> (
      match xs with
      | expr_s :: ty_s :: rname_s :: prem_sxs ->
        let* e = expr_of_sexpr_opt expr_s in
        let* ty = ty_of_sexpr_opt ty_s in
        let* rname = ty_rule_of_sexpr_opt rname_s in
        let rec map_opt f = function
          | [] -> Some []
          | y :: ys ->
            let* y' = f y in
            let* ys' = map_opt f ys in
            Some (y' :: ys')
        in
        let* prem_derivs = map_opt ty_deriv_of_sexpr_opt prem_sxs in
        Some (Rule_app { prem_derivs; concl = { expr = e; ty }; rname })
      | _ -> None
    )

let ty_deriv_of_string_opt (s : string) : ty_deriv option =
  match sexpr_of_string_opt s with
  | None -> None
  | Some sx -> ty_deriv_of_sexpr_opt sx

let string_of_ty_deriv (d : ty_deriv) : string =
  let rec go d =
    match d with
    | Hole -> [("???", "hole")]
    | Rule_app d ->
      (string_of_ty_jmt d.concl, string_of_ty_rule d.rname) :: go' [] d.prem_derivs
  and go' has_line ds =
    let lines =
      List.fold_left
        (fun acc b -> (if b then "│  " else "   ") ^ acc)
        ""
        has_line
    in
    match ds with
    | [] -> []
    | [Hole] ->
      [lines ^ "└──???" , "hole"]
    | [Rule_app d] ->
      let next_line =
        ( lines ^ "└──" ^ string_of_ty_jmt d.concl
        , string_of_ty_rule d.rname
        )
      in next_line :: go' (false :: has_line) d.prem_derivs
    | Hole :: ds -> (lines ^ "├──???" , "hole") :: go' has_line ds
    | Rule_app d :: ds ->
      let next_line =
        ( lines ^ "├──" ^ string_of_ty_jmt d.concl
        , string_of_ty_rule d.rname
        )
      in
      next_line
      :: go' (true :: has_line) d.prem_derivs
      @ go' has_line ds
  in
  let lines = go d in
  let length = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun x _ -> x + 1) 0 in
  let width =
    List.fold_left
      (fun acc (line, _) -> max acc (length line))
      0
      lines
  in
  let lines =
    List.map
      (fun (line, rname) ->
         String.concat ""
           [
             line;
             String.init (width - length line + 2) (fun _ -> ' ');
             "("; rname; ")";
           ])
      lines
  in
  String.concat "\n" lines

let check_rule (r : ty_rule) (prems : ty_jmt option list) (concl : ty_jmt) : bool =
  let prem_len_ok n = List.length prems = n in
  let prem_expr_matches i expected_expr =
    match List.nth_opt prems i with
    | Some (Some j) -> j.expr = expected_expr
    | Some None -> true
    | None -> false
  in
  let prem_ty_is i expected_ty =
    match List.nth_opt prems i with
    | Some (Some j) -> j.ty = expected_ty
    | Some None -> true
    | None -> false
  in
  let prem_ty_opt i =
    match List.nth_opt prems i with
    | Some (Some j) -> Some j.ty
    | _ -> None
  in
  match r with
  | Int_lit ->
    prem_len_ok 0
    && (match concl.expr with Int _ -> concl.ty = IntT | _ -> false)

  | Add_int ->
    prem_len_ok 2
    && (match concl.expr with
        | Bop (Add, e1, e2) ->
          concl.ty = IntT
          && prem_expr_matches 0 e1 && prem_expr_matches 1 e2
          && prem_ty_is 0 IntT && prem_ty_is 1 IntT
        | _ -> false)

  | Mul_int ->
    prem_len_ok 2
    && (match concl.expr with
        | Bop (Mul, e1, e2) ->
          concl.ty = IntT
          && prem_expr_matches 0 e1 && prem_expr_matches 1 e2
          && prem_ty_is 0 IntT && prem_ty_is 1 IntT
        | _ -> false)

  | Eq_rule ->
    prem_len_ok 2
    && (match concl.expr with
        | Bop (Eq, e1, e2) ->
          concl.ty = BoolT
          && prem_expr_matches 0 e1 && prem_expr_matches 1 e2
          && (match prem_ty_opt 0, prem_ty_opt 1 with
              | Some t1, Some t2 -> t1 = t2
              | _ -> true)
        | _ -> false)

  | If_rule ->
    prem_len_ok 3
    && (match concl.expr with
        | If (e1, e2, e3) ->
          prem_expr_matches 0 e1
          && prem_expr_matches 1 e2
          && prem_expr_matches 2 e3
          && prem_ty_is 0 BoolT
          && prem_ty_is 1 concl.ty
          && prem_ty_is 2 concl.ty
          && (match prem_ty_opt 1, prem_ty_opt 2 with
              | Some t2, Some t3 -> t2 = t3
              | _ -> true)
        | _ -> false)

type status =
  | Complete
  | Invalid
  | Partial

let check_deriv (d : ty_deriv) : status =
  let rec go d =
    match d with
    | Hole -> (true, true)
    | Rule_app ra ->
      let prem_infos = List.map go ra.prem_derivs in
      let has_hole = List.exists (fun (h, _) -> h) prem_infos in
      let prems_valid = List.for_all (fun (_, v) -> v) prem_infos in
      if not prems_valid then (has_hole, false)
      else
        let prem_jmts =
          List.map
            (function
              | Hole -> None
              | Rule_app d' -> Some d'.concl)
            ra.prem_derivs
        in
        let this_ok = check_rule ra.rname prem_jmts ra.concl in
        (has_hole, this_ok)
  in
  let (has_hole, valid) = go d in
  if not valid then Invalid
  else if has_hole then Partial
  else Complete

type value = BoolV of bool | IntV of int

let string_of_value (v : value) : string =
  match v with
  | BoolV b -> string_of_bool b
  | IntV n -> string_of_int n

let rec value_of_expr (e : expr) : value =
  match e with
  | Int n -> IntV n
  | Bop (Add, e1, e2) -> (
      match value_of_expr e1, value_of_expr e2 with
      | IntV a, IntV b -> IntV (a + b)
      | _ -> assert false
    )
  | Bop (Mul, e1, e2) -> (
      match value_of_expr e1, value_of_expr e2 with
      | IntV a, IntV b -> IntV (a * b)
      | _ -> assert false
    )
  | Bop (Eq, e1, e2) ->
    let v1 = value_of_expr e1 in
    let v2 = value_of_expr e2 in
    BoolV (v1 = v2)
  | If (c, t, f) -> (
      match value_of_expr c with
      | BoolV true -> value_of_expr t
      | BoolV false -> value_of_expr f
      | _ -> assert false
    )

type error = Parse_error | Invalid_deriv of ty_deriv

let interp (s : string) : (ty_deriv * value option, error) result  =
  match ty_deriv_of_string_opt s with
  | Some deriv -> (
    match check_deriv deriv with
    | Complete -> (
      match deriv with
      | Rule_app d -> Ok (deriv, Some (value_of_expr d.concl.expr))
      | _ -> assert false
    )
    | Partial -> Ok (deriv, None)
    | Invalid -> Error (Invalid_deriv deriv)
  )
  | None -> Error Parse_error

let example_deriv : string =
"((if (= (= 5 (+ 1 4)) (= 0 1)) (+ 2 3) (* (+ 4 5) 67)) int IF
  ((= (= 5 (+ 1 4)) (= 0 1)) bool EQ
    ((= 5 (+ 1 4)) bool EQ
      (5 int INTLIT)
      ((+ 1 4) int ADDINT
        (1 int INTLIT)
        (4 int INTLIT)))
    ((= 0 1) bool EQ
      (0 int INTLIT)
      (1 int INTLIT)))
  ((+ 2 3) int ADDINT
    (2 int INTLIT)
    (3 int INTLIT))
  ((* (+ 4 5) 67) int MULINT
    ((+ 4 5) int ADDINT
      (4 int INTLIT)
      (5 int INTLIT))
    (67 int INTLIT)))"

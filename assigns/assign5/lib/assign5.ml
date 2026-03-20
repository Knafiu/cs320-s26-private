module Tensor = Tensor
type tensor = Tensor.t

type 'a sexpr = 'a Sexpr.t =
  | Atom of 'a
  | List of 'a sexpr list

type op = Syntax.op = Add | Mul

type expr = Syntax.expr =
  | Ident of string * string list
  | Map of op * expr * expr
  | Fold of op * string * expr

type stmt = Syntax.stmt =
  | Init of string * int list * float sexpr
  | Set of string * string list * expr

let rec assoc_opt x = function
  | [] -> None
  | (y, v) :: ys -> if x = y then Some v else assoc_opt x ys

let rec distinct = function
  | [] -> true
  | x :: xs -> not (List.mem x xs) && distinct xs

let rec remove_key k = function
  | [] -> []
  | (x, v) :: xs ->
      if x = k then remove_key k xs
      else (x, v) :: remove_key k xs
let rec consistent i j =
  match i with
  | [] -> true
  | (lbl, sz) :: rest ->
      (match assoc_opt lbl j with
       | None -> consistent rest j
       | Some sz' -> sz = sz' && consistent rest j)

let union_idx i j =
  let rec add_missing acc = function
    | [] -> acc
    | (lbl, sz) :: rest ->
        if List.mem_assoc lbl acc then add_missing acc rest
        else add_missing ((lbl, sz) :: acc) rest
  in
  add_missing i j

let remove_axis lbl idx_space = remove_key lbl idx_space

let apply_op op x y =
  match op with
  | Add -> x +. y
  | Mul -> x *. y

let dim_check (env : (string * tensor) list) (e : expr) : ((string * int) list) option =
  let rec go e =
    match e with
    | Ident (name, labels) ->
        (match assoc_opt name env with
         | None -> None
         | Some t ->
             let old_idx = Tensor.idx_space t in
             if List.length labels <> List.length old_idx then None
             else if not (distinct labels) then None
             else Some (List.combine labels (List.map snd old_idx)))

    | Map (_, e1, e2) ->
        (match go e1, go e2 with
         | Some i1, Some i2 ->
             if consistent i1 i2 then Some (union_idx i1 i2)
             else None
         | _ -> None)

    | Fold (_, lbl, e1) ->
        (match go e1 with
         | None -> None
         | Some idx_space -> Some (remove_axis lbl idx_space))
  in
  go e
let eval (env : (string * tensor) list) (e : expr) : tensor =
  let fold_axis op lbl size t idx_without_lbl =
    let first = Tensor.get t ((lbl, 0) :: idx_without_lbl) in
    let rec loop acc k =
      if k = size then acc
      else
        let v = Tensor.get t ((lbl, k) :: idx_without_lbl) in
        loop (apply_op op acc v) (k + 1)
    in
    loop first 1
  in
  let rec go e =
    match e with
    | Ident (name, labels) ->
        let t = List.assoc name env in
        Tensor.relabel_axes t labels

    | Map (op, e1, e2) ->
        let t1 = go e1 in
        let t2 = go e2 in
        let i1 = Tensor.idx_space t1 in
        let i2 = Tensor.idx_space t2 in
        let idx_space = union_idx i1 i2 in
        Tensor.init idx_space (fun idx ->
          apply_op op (Tensor.get t1 idx) (Tensor.get t2 idx))

    | Fold (op, lbl, e1) ->
        let t = go e1 in
        let idx_space = Tensor.idx_space t in
        let size = List.assoc lbl idx_space in
        let result_idx = remove_axis lbl idx_space in
        Tensor.init result_idx (fun idx ->
          fold_axis op lbl size t idx)
  in
  go e

type error =
  | Parse_error
  | Dim_error
  | Init_error

let interp (env : (string * tensor) list) (s : string) : ((string * tensor) list, error) result =
  let interp_stmt env stmt =
    match Syntax.stmt_of_sexpr_opt stmt with
    | Some (Init (a, shape, expr)) -> (
        match Tensor.of_sexpr_opt shape expr with
        | Some t -> Ok ((a, t) :: env)
        | _ -> Error Init_error
      )
    | Some (Set (a, idx, e)) -> (
        let sort  = List.sort String.compare in
        match dim_check env e with
        | Some idx_space when sort idx = sort (List.map fst idx_space) ->
          let idx_space = List.map (fun x -> (x, List.assoc x idx_space)) idx in
          let t = Tensor.init idx_space (Tensor.get (eval env e)) in
          Ok ((a, t) :: env)
        | _ -> Error Dim_error
      )
    | _ -> Error Parse_error
  in
  let rec interp_prog env = function
    | [] -> Ok env
    | e :: es -> (
        match interp_stmt env e with
        | Ok env -> interp_prog env es
        | Error e -> Error e
      )
  in
  match Sexpr.list_of_string_opt s with
  | Some ss -> interp_prog env ss
  | None -> Error Parse_error

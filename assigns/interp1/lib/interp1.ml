
(* Syntax *)

type ty = Ast.Interp1.ty =
  | Unit
  | Bool
  | Int
  | Fun of ty * ty

type bop = Ast.Interp1.bop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or

type expr = Ast.Interp1.expr =
  | Unit
  | Bool of bool
  | Int of int
  | Var of string
  | Let of string * expr * expr
  | LetRec of {
      name : string;
      arg : string;
      arg_ty : ty;
      out_ty : ty;
      binding : expr;
      body : expr;
    }
  | If of expr * expr * expr
  | Fun of string * ty * expr
  | App of expr * expr
  | Bop of bop * expr * expr
  | Negate of expr
  | Assert of expr

(* Environments *)

module Env = Map.Make (String)

(* Values *)

type value =
  | Unit
  | Bool of bool
  | Int of int
  | Clos of value Env.t * string option * expr

(* Contexts *)

type ctxt = ty Env.t

(* Dynamic Environments *)

type dyn_env = value Env.t

(* Type Checking *)

let rec type_of (ctxt : ctxt) (e : expr) : ty option =
  match e with
  | Unit -> Some Unit
  | Bool _ -> Some Bool
  | Int _ -> Some Int
  | Var x -> Env.find_opt x ctxt
  | Let (x, e1, e2) ->
      (match type_of ctxt e1 with
       | Some t1 -> type_of (Env.add x t1 ctxt) e2
       | None -> None)
  | LetRec { name; arg; arg_ty; out_ty; binding; body } ->
      let f_ty = Fun (arg_ty, out_ty) in
      let ctxt' = ctxt |> Env.add name f_ty |> Env.add arg arg_ty in
      (match type_of ctxt' binding with
       | Some t when t = out_ty ->
           type_of (Env.add name f_ty ctxt) body
       | _ -> None)
  | If (e1, e2, e3) ->
      (match type_of ctxt e1, type_of ctxt e2, type_of ctxt e3 with
       | Some Bool, Some t2, Some t3 when t2 = t3 -> Some t2
       | _ -> None)
  | Fun (x, t1, body) ->
      (match type_of (Env.add x t1 ctxt) body with
       | Some t2 -> Some (Fun (t1, t2))
       | None -> None)
  | App (e1, e2) ->
      (match type_of ctxt e1, type_of ctxt e2 with
       | Some (Fun (arg_ty, out_ty)), Some t2 when arg_ty = t2 -> Some out_ty
       | _ -> None)
  | Bop (op, e1, e2) ->
      (match op, type_of ctxt e1, type_of ctxt e2 with
       | (Add | Sub | Mul | Div | Mod), Some Int, Some Int -> Some Int
       | (And | Or), Some Bool, Some Bool -> Some Bool
       | (Eq | Neq | Lt | Lte | Gt | Gte), Some t1, Some t2 when t1 = t2 -> Some Bool
       | _ -> None)
  | Negate e1 ->
      (match type_of ctxt e1 with
       | Some Int -> Some Int
       | _ -> None)
  | Assert e1 ->
      (match type_of ctxt e1 with
       | Some Bool -> Some Unit
       | _ -> None)

(* Evaluation *)

exception Div_by_zero
exception Assert_fail

let rec eval (env : dyn_env) (e : expr) : value =
  match e with
  | Unit -> Unit
  | Bool b -> Bool b
  | Int n -> Int n
  | Var x -> Env.find x env
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      eval (Env.add x v1 env) e2
  | LetRec { name; arg; arg_ty; out_ty = _; binding; body } ->
      let fun_expr = Fun (arg, arg_ty, binding) in
      let rec_closure = Clos (env, Some name, fun_expr) in
      eval (Env.add name rec_closure env) body
  | If (e1, e2, e3) ->
      (match eval env e1 with
       | Bool true -> eval env e2
       | Bool false -> eval env e3
       | _ -> assert false)
  | Fun (_, _, _) ->
      Clos (env, None, e)
  | App (e1, e2) ->
      let vf = eval env e1 in
      let va = eval env e2 in
      (match vf with
       | Clos (clos_env, None, Fun (x, _, body)) ->
           eval (Env.add x va clos_env) body
       | Clos (clos_env, Some f, Fun (x, _, body)) ->
           let env' = clos_env |> Env.add f vf |> Env.add x va in
           eval env' body
       | _ -> assert false)
  | Negate e1 ->
      (match eval env e1 with
       | Int n -> Int (-n)
       | _ -> assert false)
  | Assert e1 ->
      (match eval env e1 with
       | Bool true -> Unit
       | Bool false -> raise Assert_fail
       | _ -> assert false)
  | Bop _ -> assert false


(* Interpretation *)

let interp ~(filename : string) : value option =
  let e_ty =
    match Syntax.parse ~filename with
    | Ok p -> Ast.Interp1.expr_of_prog p
    | Error e -> Error e
  in
  match e_ty with
  | Ok e -> (
      match type_of Env.empty e with
      | Some _ -> Some (eval Env.empty e)
      | _ ->
        let _type_error_msg = print_endline "Type error"
        in None
    )
  | Error e ->
    let _parse_error_msg =
      In_channel.with_open_text filename
        (fun ic ->
           let text = In_channel.input_all ic in
           let msg = Error_msg.to_string ~filename ~text e in
           Format.eprintf "%s" msg)
    in None

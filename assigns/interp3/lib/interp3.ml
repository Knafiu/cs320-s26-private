open Utils
module Error_msg = Error_msg
module Ast = Ast

(* SYNTAX
   ----------------------------------------------------------------------
*)

type ty = Ast.Type.t =
  | TUnit
  | TBool
  | TInt
  | TString
  | TTuple of ty list
  | TAdt of ty list * string
  | TFun of ty * ty
  | TParam of string

type _pattern = Ast.Pattern.pattern =
  | PWild
  | PVar of string
  | PUnit
  | PBool of bool
  | PInt of int
  | PString of string
  | PTuple of pattern list
  | PCons of string * pattern option
and pattern = Ast.Pattern.t =
  {
    pos : pos;
    pattern : _pattern;
  }

type bop = Ast.Expr.bop =
  | Add | Sub | Mul
  | Div | Mod
  | And | Or
  | Concat
  | Eq | Neq | Lt | Lte | Gt | Gte

type _expr = Ast.Expr.expr =
  | Unit
  | Bool of bool
  | Int of int
  | String of string
  | Negate of expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Annot of expr * ty
  | Tuple of expr list
  | Assert of expr
  | Var of string
  | Cons of string * expr option
  | Fun of (string * ty option) * expr
  | App of expr * expr
  | Let of
      {
        is_rec : bool;
        name : string;
        binding : expr;
        body : expr;
      }
  | Match of expr * (pattern * expr) list
and expr = Ast.Expr.t =
  {
    pos : pos;
    expr : _expr;
  }

type _stmt = Ast.Stmt.stmt =
  | SLet of
      {
        is_rec : bool;
        name : string;
        binding : expr;
      }

  | SAdt of
      {
        tpars : string list;
        name : string;
        constrs : (string * ty option) list
      }
and stmt = Ast.Stmt.t =
  {
    pos : pos;
    stmt : _stmt;
  }

module Env = Map.Make(String)


(* TYPE ERRORS
   ----------------------------------------------------------------------
*)

let dummy_error = Error_msg.mk dummy_pos "Dummy error"

let unknown_var pos x = Error_msg.mk pos (Format.asprintf "Unbound value %s" x)

let exp_ty pos t1 t2 =
  let msg =
    Format.asprintf
      "This expression has type %a but an expression was expected of type %a"
      Ast.Type.pp t1 Ast.Type.pp t2
  in Error_msg.mk pos msg

let invalid_app pos = Error_msg.mk pos "Invalid application"

let invalid_tuple pos = Error_msg.mk pos "Invalid tuple"

let unknown_cons pos x = Error_msg.mk pos (Format.asprintf "Unbound constructor %s" x)

let cons_exp_no_args pos x =
  Error_msg.mk
    pos
    (Format.asprintf "The constructor %s expects 0 arguments" x)

let cons_exp_args pos x =
  Error_msg.mk
    pos
    (Format.asprintf "The constructor %s expects arguments" x)

let exp_pat pos t1 t2 =
  let msg =
    Format.asprintf
      "This pattern matches values of type %a but a pattern was expected which matches values of type %a"
      Ast.Type.pp t1 Ast.Type.pp t2
  in Error_msg.mk pos msg

let bound_several_times pos x =
  let msg =
    Format.asprintf
      "Variable %s is bound several times in this matching"
      x
  in Error_msg.mk pos msg

let dup_ty_name pos x =
  let msg =
    Format.asprintf
      "Type using name %s is already defined"
      x
  in Error_msg.mk pos msg

let unbound_ty_var pos n =
  Error_msg.mk
    pos
    (Format.asprintf "The type variable %s is unbound in this type declaration" n)

let ty_param_several_times pos =
  Error_msg.mk
    pos
    "A type parameter occurs several times"

(* TYPING
   ----------------------------------------------------------------------
*)

type ty_scheme = string list * ty
type ctxt = ty_scheme Env.t
type constr = ty * ty

let fresh () = TParam (_gensym ())

let type_of_expr (ctxt : ctxt) (e : expr) : (ty_scheme, Error_msg.t) result =
  let ( let* ) = Result.bind in

  let rec remove_dups xs =
    match xs with
    | [] -> []
    | x :: rest ->
      if List.mem x rest then remove_dups rest
      else x :: remove_dups rest
  in

  let rec apply_subst subst ty =
    match ty with
    | TParam a ->
      begin match List.assoc_opt a subst with
      | Some t -> t
      | None -> ty
      end
    | TTuple ts -> TTuple (List.map (apply_subst subst) ts)
    | TAdt (ts, name) -> TAdt (List.map (apply_subst subst) ts, name)
    | TFun (t1, t2) -> TFun (apply_subst subst t1, apply_subst subst t2)
    | _ -> ty
  in

  let rec occurs a ty =
    match ty with
    | TParam b -> a = b
    | TTuple ts | TAdt (ts, _) -> List.exists (occurs a) ts
    | TFun (t1, t2) -> occurs a t1 || occurs a t2
    | _ -> false
  in

  let bind a ty =
    if ty = TParam a then Ok []
    else if occurs a ty then Error dummy_error
    else Ok [(a, ty)]
  in

  let rec unify cs =
    match cs with
    | [] -> Ok []
    | (t1, t2) :: rest ->
      begin match t1, t2 with
      | TUnit, TUnit
      | TBool, TBool
      | TInt, TInt
      | TString, TString ->
        unify rest

      | TParam a, t | t, TParam a ->
        let* s1 = bind a t in
        let rest =
          List.map
            (fun (x, y) -> (apply_subst s1 x, apply_subst s1 y))
            rest
        in
        let* s2 = unify rest in
        Ok (s1 @ s2)

      | TFun (a1, b1), TFun (a2, b2) ->
        unify ((a1, a2) :: (b1, b2) :: rest)

      | TTuple ts1, TTuple ts2 ->
        if List.length ts1 = List.length ts2 then
          unify (List.combine ts1 ts2 @ rest)
        else Error dummy_error

      | TAdt (ts1, n1), TAdt (ts2, n2) ->
        if n1 = n2 && List.length ts1 = List.length ts2 then
          unify (List.combine ts1 ts2 @ rest)
        else Error dummy_error

      | _ -> Error dummy_error
      end
  in

  let instantiate (vars, ty) =
    let pairs = List.map (fun a -> (a, fresh ())) vars in
    apply_subst pairs ty
  in

  let rec infer_pattern ctxt p =
    match p.pattern with
    | PWild ->
      Ok (fresh (), [], Env.empty)

    | PVar x ->
      let a = fresh () in
      Ok (a, [], Env.add x ([], a) Env.empty)

    | PUnit ->
      Ok (TUnit, [], Env.empty)

    | PBool _ ->
      Ok (TBool, [], Env.empty)

    | PInt _ ->
      Ok (TInt, [], Env.empty)

    | PString _ ->
      Ok (TString, [], Env.empty)

    | PTuple ps ->
      let rec loop ps =
        match ps with
        | [] -> Ok ([], [], Env.empty)
        | p :: rest ->
          let* (t, c, env1) = infer_pattern ctxt p in
          let* (ts, cs, env2) = loop rest in
          let duplicate =
            Env.exists (fun x _ -> Env.mem x env2) env1
          in
          if duplicate then Error (bound_several_times p.pos "")
          else
            Ok (t :: ts, c @ cs, Env.union (fun _ a _ -> Some a) env1 env2)
      in
      let* (ts, cs, env) = loop ps in
      Ok (TTuple ts, cs, env)

    | PCons (name, arg_opt) ->
      begin match Env.find_opt name ctxt with
      | None -> Error (unknown_cons p.pos name)
      | Some scheme ->
        let cons_ty = instantiate scheme in
        begin match arg_opt, cons_ty with
        | None, TFun _ ->
          Error (cons_exp_args p.pos name)

        | None, t ->
          Ok (t, [], Env.empty)

        | Some pat, TFun (arg_ty, ret_ty) ->
          let* (pat_ty, cs, env) = infer_pattern ctxt pat in
          Ok (ret_ty, (pat_ty, arg_ty) :: cs, env)

        | Some _, _ ->
          Error (cons_exp_no_args p.pos name)
        end
      end
  in

  let rec infer ctxt e =
    match e.expr with
    | Unit -> Ok (TUnit, [])
    | Bool _ -> Ok (TBool, [])
    | Int _ -> Ok (TInt, [])
    | String _ -> Ok (TString, [])

    | Negate e1 ->
      let* (t, cs) = infer ctxt e1 in
      Ok (TInt, (t, TInt) :: cs)

    | Bop (op, e1, e2) ->
      let* (t1, c1) = infer ctxt e1 in
      let* (t2, c2) = infer ctxt e2 in
      begin match op with
      | Add | Sub | Mul | Div | Mod ->
        Ok (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2)
      | And | Or ->
        Ok (TBool, (t1, TBool) :: (t2, TBool) :: c1 @ c2)
      | Concat ->
        Ok (TString, (t1, TString) :: (t2, TString) :: c1 @ c2)
      | Eq | Neq | Lt | Lte | Gt | Gte ->
        Ok (TBool, (t1, t2) :: c1 @ c2)
      end

    | If (e1, e2, e3) ->
      let* (t1, c1) = infer ctxt e1 in
      let* (t2, c2) = infer ctxt e2 in
      let* (t3, c3) = infer ctxt e3 in
      Ok (t2, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3)

    | Annot (e1, ty) ->
      let* (t, cs) = infer ctxt e1 in
      Ok (ty, (t, ty) :: cs)

    | Tuple es ->
      let rec loop es =
        match es with
        | [] -> Ok ([], [])
        | e :: rest ->
          let* (t, c) = infer ctxt e in
          let* (ts, cs) = loop rest in
          Ok (t :: ts, c @ cs)
      in
      let* (ts, cs) = loop es in
      Ok (TTuple ts, cs)

    | Assert e1 ->
      begin match e1.expr with
      | Bool false ->
        Ok (fresh (), [])
      | _ ->
        let* (t, cs) = infer ctxt e1 in
        Ok (TUnit, (t, TBool) :: cs)
      end

    | Var x ->
      begin match Env.find_opt x ctxt with
      | Some scheme -> Ok (instantiate scheme, [])
      | None -> Error (unknown_var e.pos x)
      end

    | Cons (name, arg_opt) ->
      begin match Env.find_opt name ctxt with
      | None -> Error (unknown_cons e.pos name)
      | Some scheme ->
        let cons_ty = instantiate scheme in
        begin match arg_opt, cons_ty with
        | None, TFun _ ->
          Error (cons_exp_args e.pos name)

        | None, t ->
          Ok (t, [])

        | Some arg_expr, TFun (arg_ty, ret_ty) ->
          let* (t, cs) = infer ctxt arg_expr in
          Ok (ret_ty, (t, arg_ty) :: cs)

        | Some _, _ ->
          Error (cons_exp_no_args e.pos name)
        end
      end

    | Fun ((x, ty_opt), body) ->
      let arg_ty =
        match ty_opt with
        | Some ty -> ty
        | None -> fresh ()
      in
      let ctxt2 = Env.add x ([], arg_ty) ctxt in
      let* (body_ty, cs) = infer ctxt2 body in
      Ok (TFun (arg_ty, body_ty), cs)

    | App (e1, e2) ->
      let* (t1, c1) = infer ctxt e1 in
      let* (t2, c2) = infer ctxt e2 in
      let result_ty = fresh () in
      Ok (result_ty, (t1, TFun (t2, result_ty)) :: c1 @ c2)

    | Let { is_rec; name; binding; body } ->
      if is_rec then
        let a = fresh () in
        let ctxt1 = Env.add name ([], a) ctxt in
        let* (bind_ty, c1) = infer ctxt1 binding in
        let ctxt2 = Env.add name ([], bind_ty) ctxt in
        let* (body_ty, c2) = infer ctxt2 body in
        Ok (body_ty, (a, bind_ty) :: c1 @ c2)
      else
        let* (bind_ty, c1) = infer ctxt binding in
        let ctxt2 = Env.add name ([], bind_ty) ctxt in
        let* (body_ty, c2) = infer ctxt2 body in
        Ok (body_ty, c1 @ c2)

    | Match (scrutinee, branches) ->
      let* (scrut_ty, scrut_cs) = infer ctxt scrutinee in
      let result_ty = fresh () in
      let rec loop branches =
        match branches with
        | [] -> Ok []
        | (pat, branch_expr) :: rest ->
          let* (pat_ty, pat_cs, pat_env) = infer_pattern ctxt pat in
          let ctxt2 = Env.union (fun _ a _ -> Some a) pat_env ctxt in
          let* (branch_ty, branch_cs) = infer ctxt2 branch_expr in
          let* rest_cs = loop rest in
          Ok ((pat_ty, scrut_ty) :: (branch_ty, result_ty) ::
              pat_cs @ branch_cs @ rest_cs)
      in
      let* cs = loop branches in
      Ok (result_ty, scrut_cs @ cs)
  in

  let rec vars_in_order ty =
    match ty with
    | TParam a -> [a]
    | TFun (t1, t2) -> vars_in_order t1 @ vars_in_order t2
    | TTuple ts | TAdt (ts, _) -> List.concat (List.map vars_in_order ts)
    | _ -> []
  in

  let normalize ty =
    let vars = remove_dups (vars_in_order ty) in
    let pairs =
      List.mapi
        (fun i old_name ->
          let new_name = String.make 1 (Char.chr (97 + i)) in
          (old_name, TParam new_name))
        vars
    in
    let ty = apply_subst pairs ty in
    let params =
      List.map
        (fun (_, t) ->
          match t with
          | TParam a -> a
          | _ -> assert false)
        pairs
    in
    (params, ty)
  in

  match infer ctxt e with
  | Error e -> Error e
  | Ok (ty, constraints) ->
    begin match unify constraints with
    | Error _ -> Error (exp_ty e.pos ty ty)
    | Ok subst ->
      let final_ty = apply_subst subst ty in
      Ok (normalize final_ty)
    end

let rec nub l =
  match l with
  | [] -> []
  | x :: xs -> x :: List.filter ((<>) x) (nub xs)

let free_vars ty =
  let rec go = function
    | TTuple ts | TAdt (ts, _) -> List.concat_map go ts
    | TFun (t1, t2) -> go t1 @ go t2
    | TParam a -> [a]
    | _ -> []
  in nub (go ty)

let well_typed (p : stmt list) : (unit, Error_msg.t) result =
  let rec go (used_ty_names : string list) (ctxt : ctxt) p =
    match p with
    | [] -> Ok ()
    | {pos; stmt=SLet {is_rec;name;binding}} :: ps ->
      let body = Ast.Expr.var dummy_pos name in
      let e = Ast.Expr.let_ pos is_rec name [] None binding body in
      begin
        match type_of_expr ctxt e with
        | Ok ty -> go used_ty_names (Env.add name ty ctxt) ps
        | Error e -> Error e
      end
    | {pos; stmt=SAdt {tpars; name; constrs}} :: ps ->
      if nub tpars = tpars
      then
        if List.mem name used_ty_names
        then Error (dup_ty_name pos name)
        else
          let rec process ctxt cs =
            match cs with
            | [] -> Ok ctxt
            | (cons_name, None) :: cs ->
              let tparams = List.map (fun x -> TParam x) tpars in
              process (Env.add cons_name (tpars, TAdt(tparams, name)) ctxt) cs
            | (cons_name, Some ty) :: cs ->
              begin
                match List.(find_opt (fun x -> not (mem x tpars)) (free_vars ty)) with
                | None ->
                  let tparams = List.map (fun x -> TParam x) tpars in
                  let ctxt = Env.add cons_name (tpars, TFun (ty, TAdt(tparams, name))) ctxt in
                  process ctxt cs
                | Some a -> Error (unbound_ty_var pos a)
              end
          in
          match process ctxt constrs with
          | Ok ctxt -> go (name :: used_ty_names) ctxt ps
          | Error e-> Error e
      else Error (ty_param_several_times pos)
  in
  let ctxt =
    Env.(
      empty
      |> add "print_endline" ([], TFun (TString, TUnit))
      |> add "Nil" (["a"], TAdt ([TParam "a"], "list"))
      |> add "Cons" (["a"], TFun (TTuple [TParam "a"; TAdt ([TParam "a"], "list")], TAdt ([TParam "a"], "list")))
    )
  in go [] ctxt p

(* EVALUATION
   ----------------------------------------------------------------------
*)

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VString of string
  | VCons of string * value option
  | VTuple of value list
  | VClos of {
      env : value Env.t;
      name : string option;
      arg : string;
      body : expr;
    }

type dyn_env = value Env.t

exception Div_by_zero of pos
exception Assert_fail of pos
exception Match_fail of pos
exception Compare_fun_val of pos

let eval_expr (env : dyn_env) (e : Ast.Expr.t) : value =
  let rec pattern_match v p =
    match v, p.pattern with
    | _, PWild -> Some Env.empty
    | _, PVar x -> Some (Env.add x v Env.empty)

    | VUnit, PUnit -> Some Env.empty
    | VBool b1, PBool b2 when b1 = b2 -> Some Env.empty
    | VInt n1, PInt n2 when n1 = n2 -> Some Env.empty
    | VString s1, PString s2 when s1 = s2 -> Some Env.empty

    | VTuple vs, PTuple ps ->
      if List.length vs = List.length ps then
        let rec loop vs ps =
          match vs, ps with
          | [], [] -> Some Env.empty
          | v :: vs, p :: ps ->
            begin
              match pattern_match v p, loop vs ps with
              | Some env1, Some env2 ->
                Some (Env.union (fun _ a _ -> Some a) env1 env2)
              | _ -> None
            end
          | _ -> None
        in
        loop vs ps
      else None

    | VCons (c1, None), PCons (c2, None) when c1 = c2 ->
      Some Env.empty

    | VCons (c1, Some v), PCons (c2, Some p) when c1 = c2 ->
      pattern_match v p

    | _ -> None
  in

  let rec eval env e =
    match e.expr with
    | Unit -> VUnit
    | Bool b -> VBool b
    | Int n -> VInt n
    | String s -> VString s

    | Negate e1 ->
      begin match eval env e1 with
      | VInt n -> VInt (-n)
      | _ -> assert false
      end

    | Bop (op, e1, e2) ->
      begin match op with
      | And ->
        begin match eval env e1 with
        | VBool false -> VBool false
        | VBool true -> eval env e2
        | _ -> assert false
        end

      | Or ->
        begin match eval env e1 with
        | VBool true -> VBool true
        | VBool false -> eval env e2
        | _ -> assert false
        end

      | Add | Sub | Mul | Div | Mod ->
        begin match eval env e1, eval env e2 with
        | VInt n1, VInt n2 ->
          begin match op with
          | Add -> VInt (n1 + n2)
          | Sub -> VInt (n1 - n2)
          | Mul -> VInt (n1 * n2)
          | Div ->
            if n2 = 0 then raise (Div_by_zero e.pos)
            else VInt (n1 / n2)
          | Mod ->
            if n2 = 0 then raise (Div_by_zero e.pos)
            else VInt (n1 mod n2)
          | _ -> assert false
          end
        | _ -> assert false
        end

      | Concat ->
        begin match eval env e1, eval env e2 with
        | VString s1, VString s2 -> VString (s1 ^ s2)
        | _ -> assert false
        end

      | Eq -> VBool (eval env e1 = eval env e2)
      | Neq -> VBool (eval env e1 <> eval env e2)
      | Lt -> VBool (eval env e1 < eval env e2)
      | Lte -> VBool (eval env e1 <= eval env e2)
      | Gt -> VBool (eval env e1 > eval env e2)
      | Gte -> VBool (eval env e1 >= eval env e2)
      end

    | If (e1, e2, e3) ->
      begin match eval env e1 with
      | VBool true -> eval env e2
      | VBool false -> eval env e3
      | _ -> assert false
      end

    | Annot (e1, _) -> eval env e1

    | Tuple es ->
      VTuple (List.map (eval env) es)

    | Assert e1 ->
      begin match eval env e1 with
      | VBool true -> VUnit
      | VBool false -> raise (Assert_fail e.pos)
      | _ -> assert false
      end

    | Var x -> Env.find x env

    | Cons (name, None) ->
      VCons (name, None)

    | Cons (name, Some e1) ->
      VCons (name, Some (eval env e1))

    | Fun ((x, _), body) ->
      VClos { env; name = None; arg = x; body }

    | App (e1, e2) ->
      let f = eval env e1 in
      let v = eval env e2 in
      begin match f with
      | VClos { arg = "$print_endline"; _ } ->
        begin match v with
        | VString s ->
          print_endline s;
          VUnit
        | _ -> assert false
        end

      | VClos { env = clos_env; name = None; arg; body } ->
        eval (Env.add arg v clos_env) body

      | VClos ({ env = clos_env; name = Some f_name; arg; body } as clos) ->
        let env1 = Env.add f_name (VClos clos) clos_env in
        let env2 = Env.add arg v env1 in
        eval env2 body

      | _ -> assert false
      end

    | Let { is_rec; name; binding; body } ->
      if is_rec then
        begin match binding.expr with
        | Fun ((arg, _), fun_body) ->
          let clos = VClos { env; name = Some name; arg; body = fun_body } in
          eval (Env.add name clos env) body
        | _ -> assert false
        end
      else
        let v = eval env binding in
        eval (Env.add name v env) body

    | Match (e1, branches) ->
      let v = eval env e1 in
      let rec try_branches branches =
        match branches with
        | [] -> raise (Match_fail e.pos)
        | (pat, branch_expr) :: rest ->
          begin match pattern_match v pat with
          | Some pat_env ->
            let new_env =
              Env.union (fun _ pattern_value _ -> Some pattern_value) pat_env env
            in
            eval new_env branch_expr
          | None -> try_branches rest
          end
      in
      try_branches branches
  in
  eval env e

let eval (p : stmt list) : value =
  let rec go env v p =
    match p with
    | [] -> Option.value ~default:VUnit v
    | {pos; stmt=SLet {is_rec; name; binding}} :: ps ->
      let body = {pos=dummy_pos; expr=Var name} in
      let e = Ast.Expr.let_ pos is_rec name [] None binding body in
      let v = eval_expr env e in
      go (Env.add name v env) (Some v) ps
    | _ :: ps -> go env v ps
  in
  let env =
    Env.(
      empty
      |> add "print_endline"
        (VClos
           {
             env = empty;
             name = None;
             arg = "$print_endline";
             body = Ast.Expr.mk dummy_pos Unit;
           })
    )
  in go env None p


(* INTERPRETER
   ----------------------------------------------------------------------
*)

let interp ~(filename : string) : (value, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let* prog = Syntax.parse ~filename in
  let* () = well_typed prog in
  let* v =
    match eval prog with
    | v -> Ok v
    | exception Assert_fail pos -> Error (Error_msg.mk pos "(Exception) Assert_fail")
    | exception Div_by_zero pos -> Error (Error_msg.mk pos "(Exception) Div_by_zero")
    | exception Match_fail pos -> Error (Error_msg.mk pos "(Exception) Match_fail")
    | exception Compare_fun_val pos -> Error (Error_msg.mk pos "(Exception) Compare_fun_val")
  in
  Ok v

(* TESTING STUFF
   ----------------------------------------------------------------------
*)

let parse_expr s =
  let s = "let _x = " ^ s in
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | [{pos=_;stmt=SLet {binding=e;_}}] -> e
  | _ -> assert false

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
  let rec apply_subst subst ty =
    match ty with
    | TParam a ->
      begin match List.assoc_opt a subst with
      | Some t -> t
      | None -> ty
      end
    | TTuple ts -> TTuple (List.map (apply_subst subst) ts)
    | TAdt (ts, n) -> TAdt (List.map (apply_subst subst) ts, n)
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

  let bind a t =
    if t = TParam a then Ok []
    else if occurs a t then Error dummy_error
    else Ok [(a, t)]
  in

  let rec unify = function
    | [] -> Ok []
    | (t1, t2) :: rest ->
      begin
        match (t1, t2) with
        | TUnit, TUnit
        | TBool, TBool
        | TInt, TInt
        | TString, TString -> unify rest

        | TParam a, t | t, TParam a ->
          begin match bind a t with
          | Error e -> Error e
          | Ok s1 ->
            let rest =
              List.map (fun (x,y) -> (apply_subst s1 x, apply_subst s1 y)) rest
            in
            match unify rest with
            | Error e -> Error e
            | Ok s2 -> Ok (s1 @ s2)
          end

        | TFun(a1,b1), TFun(a2,b2) ->
          unify ((a1,a2)::(b1,b2)::rest)

        | TTuple ts1, TTuple ts2 when List.length ts1 = List.length ts2 ->
          unify (List.combine ts1 ts2 @ rest)

        | TAdt(ts1,n1), TAdt(ts2,n2)
          when n1 = n2 && List.length ts1 = List.length ts2 ->
          unify (List.combine ts1 ts2 @ rest)

        | _ -> Error dummy_error
      end
  in

  let instantiate (vars, ty) =
    let pairs = List.map (fun a -> (a, fresh())) vars in
    apply_subst pairs ty
  in

  let rec infer ctxt e =
    match e.expr with
    | Unit -> Ok (TUnit, [])
    | Bool _ -> Ok (TBool, [])
    | Int _ -> Ok (TInt, [])
    | String _ -> Ok (TString, [])

    | Negate e1 ->
      let* (t,c) = infer ctxt e1 in
      Ok (TInt, (t,TInt)::c)

    | Bop(op,e1,e2) ->
      let* (t1,c1) = infer ctxt e1 in
      let* (t2,c2) = infer ctxt e2 in
      begin match op with
      | Add|Sub|Mul|Div|Mod ->
        Ok (TInt, (t1,TInt)::(t2,TInt)::c1@c2)
      | And|Or ->
        Ok (TBool, (t1,TBool)::(t2,TBool)::c1@c2)
      | Concat ->
        Ok (TString, (t1,TString)::(t2,TString)::c1@c2)
      | Eq|Neq|Lt|Lte|Gt|Gte ->
        Ok (TBool, (t1,t2)::c1@c2)
      end

    | If(e1,e2,e3) ->
      let* (t1,c1) = infer ctxt e1 in
      let* (t2,c2) = infer ctxt e2 in
      let* (t3,c3) = infer ctxt e3 in
      Ok (t2, (t1,TBool)::(t2,t3)::c1@c2@c3)

    | Var x ->
      begin match Env.find_opt x ctxt with
      | Some sch -> Ok (instantiate sch, [])
      | None -> Error (unknown_var e.pos x)
      end

    | Fun((x,_),body) ->
      let a = fresh() in
      let ctxt = Env.add x ([],a) ctxt in
      let* (t,c) = infer ctxt body in
      Ok (TFun(a,t), c)

    | App(e1,e2) ->
      let* (t1,c1) = infer ctxt e1 in
      let* (t2,c2) = infer ctxt e2 in
      let a = fresh() in
      Ok (a, (t1,TFun(t2,a))::c1@c2)

    | Let{is_rec;name;binding;body} ->
      if is_rec then
        let a = fresh() in
        let ctxt1 = Env.add name ([],a) ctxt in
        let* (t1,c1) = infer ctxt1 binding in
        let ctxt2 = Env.add name ([],t1) ctxt in
        let* (t2,c2) = infer ctxt2 body in
        Ok (t2, (a,t1)::c1@c2)
      else
        let* (t1,c1) = infer ctxt binding in
        let ctxt = Env.add name ([],t1) ctxt in
        let* (t2,c2) = infer ctxt body in
        Ok (t2, c1@c2)

    | _ -> Error dummy_error
  in

  match infer ctxt e with
  | Error e -> Error e
  | Ok (t,cs) ->
    match unify cs with
    | Error _ -> Error (exp_ty e.pos t t)
    | Ok s ->
      let t = apply_subst s t in
      Ok ([], t)

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
  let rec eval env e =
    match e.expr with
    | Unit -> VUnit
    | Bool b -> VBool b
    | Int n -> VInt n
    | String s -> VString s

    | Negate e1 ->
      let VInt n = eval env e1 in
      VInt (-n)

    | Bop(op,e1,e2) ->
      begin match op with
      | And ->
        let VBool b = eval env e1 in
        if b then eval env e2 else VBool false

      | Or ->
        let VBool b = eval env e1 in
        if b then VBool true else eval env e2

      | Add|Sub|Mul|Div|Mod ->
        let VInt n1 = eval env e1 in
        let VInt n2 = eval env e2 in
        begin match op with
        | Add -> VInt(n1+n2)
        | Sub -> VInt(n1-n2)
        | Mul -> VInt(n1*n2)
        | Div ->
          if n2=0 then raise(Div_by_zero e.pos)
          else VInt(n1/n2)
        | Mod ->
          if n2=0 then raise(Div_by_zero e.pos)
          else VInt(n1 mod n2)
        | _ -> assert false
        end

      | Concat ->
        let VString s1 = eval env e1 in
        let VString s2 = eval env e2 in
        VString (s1 ^ s2)

      | Eq -> VBool (eval env e1 = eval env e2)
      | Neq -> VBool (eval env e1 <> eval env e2)
      | Lt -> VBool (eval env e1 < eval env e2)
      | Lte -> VBool (eval env e1 <= eval env e2)
      | Gt -> VBool (eval env e1 > eval env e2)
      | Gte -> VBool (eval env e1 >= eval env e2)
      end

    | If(e1,e2,e3) ->
      let VBool b = eval env e1 in
      if b then eval env e2 else eval env e3

    | Var x -> Env.find x env

    | Fun((x,_),body) ->
      VClos {env; name=None; arg=x; body}

    | App(e1,e2) ->
      let f = eval env e1 in
      let v = eval env e2 in
      begin match f with
      | VClos {env; arg; body; _} ->
        eval (Env.add arg v env) body
      | _ -> assert false
      end

    | Let{is_rec;name;binding;body} ->
      if is_rec then
        let VClos clos = eval env binding in
        let env = Env.add name (VClos {clos with name=Some name}) env in
        eval env body
      else
        let v = eval env binding in
        eval (Env.add name v env) body

    | _ -> VUnit
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

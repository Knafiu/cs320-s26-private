open Utils
module Error_msg = Error_msg

type ty = Ast.Interp2.ty =
  | TUnit
  | TBool
  | TInt
  | TInt_list
  | TFun of ty * ty
  | TTuple of ty list

let rec pp_ty ppf ty =
  let open Fmt in
  let pp_parens ppf ty =
    match ty with
    | TFun (_, _)
    | TTuple _
    | _ -> pp_ty ppf ty
  in
  match ty with
  | TUnit -> pf ppf "unit"
  | TBool -> pf ppf "bool"
  | TInt -> pf ppf "int"
  | TFun (t1, t2) -> pf ppf "%a -> %a" pp_parens t1 pp_ty t2
  | TTuple ts -> list ~sep:(Fmt.any " * ") pp_ty ppf ts
  | TInt_list -> pf ppf "int list"

type _pattern = Ast.Interp2._pattern =
  | PUnit
  | PBool of bool
  | PInt of int
  | PNil
  | PCons of pattern * pattern
  | PTuple of pattern list
  | PVar of string
and pattern = Ast.Interp2.pattern =
  {
    pos : pos;
    pattern : _pattern;
  }

type bop = Ast.Interp2.bop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or | Cons

type _expr = Ast.Interp2._expr =
  | Unit
  | Bool of bool
  | Int of int
  | Var of string
  | Nil
  | Assert of expr
  | Negate of expr
  | Tuple of expr list
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Fun of (string * ty) list * expr
  | App of expr * expr list
  | Let of
      {
        is_rec : bool;
        name : string;
        args : (string * ty) list;
        annot : ty option;
        binding : expr;
        body : expr;
      }
  | Match of expr * (pattern * expr) list
and expr = Ast.Interp2.expr =
  {
    pos : pos;
    expr : _expr;
  }

type _stmt = Ast.Interp2._stmt =
  | SLet of {
      is_rec : bool;
      name : string;
      args : (string * ty) list;
      annot : ty option;
      binding : expr;
    }
and stmt = Ast.Interp2.stmt =
  {
    pos : pos;
    stmt : _stmt;
  }

type prog = stmt list

module Env = Map.Make(String)

let unknown_var pos x = Error_msg.mk pos (Format.asprintf "Unbound value %s" x)

let exp_ty pos t1 t2 =
  let msg =
    Format.asprintf
      "This expression has type %a but an expression was expected of type %a"
      pp_ty t1 pp_ty t2
  in Error_msg.mk pos msg

let exp_pat pos t1 t2 =
  let msg =
    Format.asprintf
      "This pattern matches values of type %a but a pattern was expected which matches values of type %a"
      pp_ty t1 pp_ty t2
  in Error_msg.mk pos msg

let exp_tuple_pat pos t =
  let msg =
    Format.asprintf
      "This pattern matches values of a tuple type but a pattern was expected which matches values of type %a"
      pp_ty t
  in Error_msg.mk pos msg

let exp_diff_tuple_pat pos ty =
  let msg =
    Format.asprintf
      "This pattern matches values of a tuple type but a pattern was expected which matches values of a different tuple type %a"
      pp_ty ty
  in Error_msg.mk pos msg

let not_func pos ty =
  let msg =
    Format.asprintf
      "This expression has type %a. This is not a function; it cannot be applied"
      pp_ty ty
  in Error_msg.mk pos msg

let too_many_args pos ty =
  let msg =
    Format.asprintf
      "This function has type %a. It is applied to to many arguments"
      pp_ty ty
  in Error_msg.mk pos msg

let missing_rec_annot pos =
  Error_msg.mk pos "Must provide output type annotation for recursive function"

let missing_rec_arg pos =
  Error_msg.mk pos "Must provide argument for recursive function"

let bound_several_times pos x =
  let msg =
    Format.asprintf
      "Variable %s is bound several times in this matching"
      x
  in Error_msg.mk pos msg

type ctxt = ty Env.t

let mk_fun_ty (args : (string * ty) list) (out_ty : ty) : ty =
  List.fold_right (fun (_, arg_ty) acc -> TFun (arg_ty, acc)) args out_ty

let add_args_to_ctxt (ctxt : ctxt) (args : (string * ty) list) : ctxt =
  List.fold_left (fun acc (x, t) -> Env.add x t acc) ctxt args

let rec comparable_ty (t : ty) : bool =
  match t with
  | TUnit | TBool | TInt | TInt_list -> true
  | TTuple ts -> List.for_all comparable_ty ts
  | TFun _ -> false

let rec type_pattern (ctxt : ctxt) (p : pattern) (ty : ty) : (ctxt, Error_msg.t) result =
  let ( let* ) = Result.bind in
  match p.pattern with
  | PUnit ->
      if ty = TUnit then Ok ctxt else Error (exp_pat p.pos TUnit ty)
  | PBool _ ->
      if ty = TBool then Ok ctxt else Error (exp_pat p.pos TBool ty)
  | PInt _ ->
      if ty = TInt then Ok ctxt else Error (exp_pat p.pos TInt ty)
  | PNil ->
      if ty = TInt_list then Ok ctxt else Error (exp_pat p.pos TInt_list ty)
  | PVar x ->
      if Env.mem x ctxt then Error (bound_several_times p.pos x)
      else Ok (Env.add x ty ctxt)
  | PCons (p1, p2) ->
      if ty <> TInt_list then Error (exp_pat p.pos TInt_list ty)
      else
        let* ctxt1 = type_pattern ctxt p1 TInt in
        type_pattern ctxt1 p2 TInt_list
  | PTuple ps ->
      begin match ty with
      | TTuple ts ->
          if List.length ps <> List.length ts then Error (exp_diff_tuple_pat p.pos ty)
          else
            List.fold_left2
              (fun acc p_i t_i ->
                 let* ctxt_acc = acc in
                 type_pattern ctxt_acc p_i t_i)
              (Ok ctxt)
              ps
              ts
      | _ -> Error (exp_tuple_pat p.pos ty)
      end

let rec type_of_expr (ctxt : ctxt) (e : expr) : (ty, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let rec type_list (es : expr list) : (ty list, Error_msg.t) result =
    match es with
    | [] -> Ok []
    | e1 :: rest ->
      let* t1 = type_of_expr ctxt e1 in
      let* ts = type_list rest in
      Ok (t1 :: ts)
  in
  match e.expr with
  | Unit -> Ok TUnit
  | Bool _ -> Ok TBool
  | Int _ -> Ok TInt
  | Var x ->
    begin match Env.find_opt x ctxt with
    | Some t -> Ok t
    | None -> Error (unknown_var e.pos x)
    end
  | Nil -> Ok TInt_list
  | Assert e1 ->
    let* t1 = type_of_expr ctxt e1 in
    if t1 = TBool then Ok TUnit else Error (exp_ty e1.pos t1 TBool)
  | Negate e1 ->
    let* t1 = type_of_expr ctxt e1 in
    if t1 = TInt then Ok TInt else Error (exp_ty e1.pos t1 TInt)
  | Tuple es ->
    let* ts = type_list es in
    Ok (TTuple ts)
  | Bop (bop, e1, e2) ->
    let* t1 = type_of_expr ctxt e1 in
    let* t2 = type_of_expr ctxt e2 in
    begin match bop with
    | Add | Sub | Mul | Div | Mod ->
        if t1 <> TInt then Error (exp_ty e1.pos t1 TInt)
        else if t2 <> TInt then Error (exp_ty e2.pos t2 TInt)
        else Ok TInt
    | Eq | Neq | Lt | Lte | Gt | Gte ->
        if t1 <> t2 then Error (exp_ty e2.pos t2 t1)
        else if comparable_ty t1 then Ok TBool
        else Error (exp_ty e1.pos t1 TUnit)
    | And | Or ->
        if t1 <> TBool then Error (exp_ty e1.pos t1 TBool)
        else if t2 <> TBool then Error (exp_ty e2.pos t2 TBool)
        else Ok TBool
    | Cons ->
        if t1 <> TInt then Error (exp_ty e1.pos t1 TInt)
        else if t2 <> TInt_list then Error (exp_ty e2.pos t2 TInt_list)
        else Ok TInt_list
    end
  | If (e1, e2, e3) ->
    let* t1 = type_of_expr ctxt e1 in
    let* t2 = type_of_expr ctxt e2 in
    let* t3 = type_of_expr ctxt e3 in
    if t1 <> TBool then Error (exp_ty e1.pos t1 TBool)
    else if t2 <> t3 then Error (exp_ty e3.pos t3 t2)
    else Ok t2
  | Fun (args, body) ->
    let body_ctxt = add_args_to_ctxt ctxt args in
    let* body_ty = type_of_expr body_ctxt body in
    Ok (mk_fun_ty args body_ty)
  | App (fn, args) ->
    let* fn_ty = type_of_expr ctxt fn in
    let rec go original_ty current_ty remaining =
      match remaining with
      | [] -> Ok current_ty
      | arg_expr :: rest ->
          let* arg_ty = type_of_expr ctxt arg_expr in
          begin match current_ty with
          | TFun (param_ty, out_ty) ->
              if arg_ty = param_ty
              then go original_ty out_ty rest
              else Error (exp_ty arg_expr.pos arg_ty param_ty)
          | _ ->
              Error (too_many_args fn.pos original_ty)
          end
    in
    begin match args with
    | [] -> Ok fn_ty
    | _ ->
        begin match fn_ty with
        | TFun _ -> go fn_ty fn_ty args
        | _ -> Error (not_func fn.pos fn_ty)
        end
    end
  | Let {is_rec; name; args; annot; binding; body} ->
    if is_rec then
      begin
        match args, annot with
        | [], _ -> Error (missing_rec_arg e.pos)
        | _, None -> Error (missing_rec_annot e.pos)
        | _, Some out_ty ->
            let fn_ty = mk_fun_ty args out_ty in
            let binding_ctxt =
              ctxt
              |> Env.add name fn_ty
              |> fun c -> add_args_to_ctxt c args
            in
            let* binding_ty = type_of_expr binding_ctxt binding in
            if binding_ty = out_ty
            then type_of_expr (Env.add name fn_ty ctxt) body
            else Error (exp_ty binding.pos binding_ty out_ty)
      end
    else
      begin
        match args with
        | [] ->
            let* binding_ty = type_of_expr ctxt binding in
            let declared_ty =
              match annot with
              | None -> Ok binding_ty
              | Some t ->
                  if t = binding_ty then Ok t
                  else Error (exp_ty binding.pos binding_ty t)
            in
            let* declared_ty = declared_ty in
            type_of_expr (Env.add name declared_ty ctxt) body
        | _ ->
            let binding_ctxt = add_args_to_ctxt ctxt args in
            let* inferred_out_ty = type_of_expr binding_ctxt binding in
            let out_ty =
              match annot with
              | None -> Ok inferred_out_ty
              | Some t ->
                  if t = inferred_out_ty then Ok t
                  else Error (exp_ty binding.pos inferred_out_ty t)
            in
            let* out_ty = out_ty in
            let fn_ty = mk_fun_ty args out_ty in
            type_of_expr (Env.add name fn_ty ctxt) body
      end

| Match (scrutinee, branches) ->
    let* scrut_ty = type_of_expr ctxt scrutinee in
    begin
      match branches with
      | [] -> assert false
      | (p1, e1) :: rest ->
          let* pat_ctxt1 = type_pattern Env.empty p1 scrut_ty in
          let ctxt1 = Env.union (fun _ _ new_t -> Some new_t) ctxt pat_ctxt1 in
          let* branch_ty = type_of_expr ctxt1 e1 in
          let rec check_rest bs =
            match bs with
            | [] -> Ok branch_ty
            | (p, branch_e) :: tl ->
                let* pat_ctxt = type_pattern Env.empty p scrut_ty in
                let branch_ctxt = Env.union (fun _ _ new_t -> Some new_t) ctxt pat_ctxt in
                let* this_ty = type_of_expr branch_ctxt branch_e in
                if this_ty = branch_ty then check_rest tl
                else Error (exp_ty branch_e.pos this_ty branch_ty)
          in
          check_rest rest
    end

let type_of (p : prog) : (ty, Error_msg.t) result =
  let rec go ctxt ty p =
    match p with
    | [] -> Ok (Option.value ~default:TUnit ty)
    | {pos; stmt=SLet {is_rec; name; args; annot; binding}} :: ps -> (
      let body = {pos=dummy_pos; expr=Var name} in
      let e = {pos; expr=Let {is_rec; name; args; annot; binding; body}} in
      match type_of_expr ctxt e with
      | Ok ty ->
        let ctxt = Env.add name ty ctxt in
        go ctxt (Some ty) ps
      | Error err -> Error err
    )
  in
  go Env.empty None p

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VTuple of value list
  | VClos of {
      env : value Env.t;
      name : string option;
      args : string list;
      body : expr;
    }
  | VInt_list of int list

type dyn_env = value Env.t

exception Div_by_zero of pos
exception Assert_fail of pos
exception Match_fail of pos

let rec value_equal (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | VUnit, VUnit -> true
  | VBool b1, VBool b2 -> b1 = b2
  | VInt n1, VInt n2 -> n1 = n2
  | VInt_list xs, VInt_list ys -> xs = ys
  | VTuple vs1, VTuple vs2 ->
    List.length vs1 = List.length vs2 &&
    List.for_all2 value_equal vs1 vs2
  | VClos _, VClos _ -> assert false
  | _ -> false

let rec value_compare (v1 : value) (v2 : value) : int =
  match v1, v2 with
  | VUnit, VUnit -> 0
  | VBool b1, VBool b2 -> Stdlib.compare b1 b2
  | VInt n1, VInt n2 -> Stdlib.compare n1 n2
  | VInt_list xs, VInt_list ys -> Stdlib.compare xs ys
  | VTuple vs1, VTuple vs2 ->
    let rec cmp_lists xs ys =
      match xs, ys with
      | [], [] -> 0
      | x :: xs', y :: ys' ->
        let c = value_compare x y in
        if c <> 0 then c else cmp_lists xs' ys'
      | _ -> assert false
    in
    cmp_lists vs1 vs2
  | VClos _, VClos _ -> assert false
  | _ -> assert false

and apply_closure (clos : value) (arg_val : value) : value =
  match clos with
  | VClos {env = clos_env; name; args; body} ->
    begin match args with
    | [] -> assert false
    | x :: rest ->
      let self = VClos {env = clos_env; name; args; body} in
      let env_with_name =
        match name with
        | None -> clos_env
        | Some f -> Env.add f self clos_env
      in
      let env' = Env.add x arg_val env_with_name in
      begin match rest with
      | [] -> eval_expr env' body
      | _ -> VClos {env = env'; name = None; args = rest; body}
      end
    end
  | _ -> assert false

and match_pattern (p : pattern) (v : value) : dyn_env option =
  match p.pattern, v with
  | PUnit, VUnit -> Some Env.empty
  | PBool b1, VBool b2 when b1 = b2 -> Some Env.empty
  | PInt n1, VInt n2 when n1 = n2 -> Some Env.empty
  | PNil, VInt_list [] -> Some Env.empty
  | PVar x, v -> Some (Env.add x v Env.empty)
  | PCons (p1, p2), VInt_list (x :: xs) ->
    begin match match_pattern p1 (VInt x), match_pattern p2 (VInt_list xs) with
    | Some env1, Some env2 ->
      Some (Env.union (fun _ v _ -> Some v) env1 env2)
    | _ -> None
    end
  | PTuple ps, VTuple vs when List.length ps = List.length vs ->
    let rec go ps vs acc =
      match ps, vs with
      | [], [] -> Some acc
      | p :: ps', v :: vs' ->
        begin match match_pattern p v with
        | None -> None
        | Some env_p ->
          let acc' = Env.union (fun _ v _ -> Some v) acc env_p in
          go ps' vs' acc'
        end
      | _ -> None
    in
    go ps vs Env.empty
  | _ -> None

and eval_expr (env : dyn_env) (e : expr) : value =
  match e.expr with
  | Unit -> VUnit
  | Bool b -> VBool b
  | Int n -> VInt n
  | Var x -> Env.find x env
  | Nil -> VInt_list []
  | Assert e1 ->
    begin match eval_expr env e1 with
    | VBool true -> VUnit
    | VBool false -> raise (Assert_fail e.pos)
    | _ -> assert false
    end
  | Negate e1 ->
    begin match eval_expr env e1 with
    | VInt n -> VInt (-n)
    | _ -> assert false
    end
  | Tuple es ->
    VTuple (List.map (eval_expr env) es)
  | Bop (bop, e1, e2) ->
    begin match bop with
    | Add ->
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n1, VInt n2 -> VInt (n1 + n2)
      | _ -> assert false
      end
    | Sub ->
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n1, VInt n2 -> VInt (n1 - n2)
      | _ -> assert false
      end
    | Mul ->
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n1, VInt n2 -> VInt (n1 * n2)
      | _ -> assert false
      end
    | Div ->
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt _, VInt 0 -> raise (Div_by_zero e.pos)
      | VInt n1, VInt n2 -> VInt (n1 / n2)
      | _ -> assert false
      end
    | Mod ->
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt _, VInt 0 -> raise (Div_by_zero e.pos)
      | VInt n1, VInt n2 -> VInt (n1 mod n2)
      | _ -> assert false
      end
    | Eq ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      VBool (value_equal v1 v2)
    | Neq ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      VBool (not (value_equal v1 v2))
    | Lt ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      VBool (value_compare v1 v2 < 0)
    | Lte ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      VBool (value_compare v1 v2 <= 0)
    | Gt ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      VBool (value_compare v1 v2 > 0)
    | Gte ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      VBool (value_compare v1 v2 >= 0)
    | And ->
      begin match eval_expr env e1 with
      | VBool false -> VBool false
      | VBool true ->
        begin match eval_expr env e2 with
        | VBool b -> VBool b
        | _ -> assert false
        end
      | _ -> assert false
      end
    | Or ->
      begin match eval_expr env e1 with
      | VBool true -> VBool true
      | VBool false ->
        begin match eval_expr env e2 with
        | VBool b -> VBool b
        | _ -> assert false
        end
      | _ -> assert false
      end
    | Cons ->
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n, VInt_list ns -> VInt_list (n :: ns)
      | _ -> assert false
      end
    end
  | If (e1, e2, e3) ->
    begin match eval_expr env e1 with
    | VBool true -> eval_expr env e2
    | VBool false -> eval_expr env e3
    | _ -> assert false
    end
  | Fun (args, body) ->
    VClos
      {
        env;
        name = None;
        args = List.map fst args;
        body;
      }
  | App (fn, args) ->
    let fn_val = eval_expr env fn in
    List.fold_left
      (fun acc arg_expr ->
        let arg_val = eval_expr env arg_expr in
        apply_closure acc arg_val)
      fn_val
      args
  | Let {is_rec; name; args; annot = _; binding; body} ->
    begin match is_rec, args with
    | false, [] ->
      let bound_val = eval_expr env binding in
      let env' = Env.add name bound_val env in
      eval_expr env' body
    | false, _ ->
      let clos =
        VClos
          {
            env;
            name = None;
            args = List.map fst args;
            body = binding;
          }
      in
      let env' = Env.add name clos env in
      eval_expr env' body
    | true, [] ->
      assert false
    | true, _ ->
      let clos =
        VClos
          {
            env;
            name = Some name;
            args = List.map fst args;
            body = binding;
          }
      in
      let env' = Env.add name clos env in
      eval_expr env' body
    end
  | Match (scrutinee, branches) ->
    let v = eval_expr env scrutinee in
    let rec try_branches bs =
      match bs with
      | [] -> raise (Match_fail e.pos)
      | (p, branch_e) :: rest ->
        begin match match_pattern p v with
        | None -> try_branches rest
        | Some pat_env ->
          let env' = Env.union (fun _ v _ -> Some v) pat_env env in
          eval_expr env' branch_e
        end
    in
    try_branches branches

let eval (p : prog) : value =
  let rec go env v p =
    match p with
    | [] -> Option.value ~default:VUnit v
    | {pos; stmt=SLet {is_rec; name; args; annot; binding}} :: ps ->
      let body = {pos=dummy_pos; expr=Var name} in
      let e = {pos; expr=Let {is_rec; name; args; annot; binding; body}} in
      let v = eval_expr env e in
      go (Env.add name v env) (Some v) ps
  in
  go Env.empty None p

let interp ~(filename : string) : (value * ty, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let* prog = Syntax.parse ~filename in
  let* prog = Ast.Interp2.prog_of_prog prog in
  let* ty = type_of prog in
  let* v =
    match eval prog with
    | v -> Ok v
    | exception Assert_fail pos -> Error (Error_msg.mk pos "(Exception) Assert_fail")
    | exception Div_by_zero pos -> Error (Error_msg.mk pos "(Exception) Div_by_zero")
    | exception Match_fail pos -> Error (Error_msg.mk pos "(Exception) Match_fail")
  in
  Ok (v, ty)

let parse_expr s =
  let s = "let _ = " ^ s in
  let p = Parser.prog Lexer.read (Lexing.from_string s) in
  match Ast.Interp2.prog_of_prog p with
  | Ok [{pos=_;stmt=SLet {binding=e;_}}] -> e
  | _ -> assert false

let parse_ty s =
  let s = "let _ : " ^ s ^ " = assert false" in
  let p = Parser.prog Lexer.read (Lexing.from_string s) in
  match Ast.Interp2.prog_of_prog p with
  | Ok [{pos=_;stmt=SLet {annot=Some ty;_}}] -> ty
  | _ -> assert false
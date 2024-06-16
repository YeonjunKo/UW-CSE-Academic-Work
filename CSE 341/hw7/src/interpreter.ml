open Ast
open Errors

let string_of_entry = show_env_entry
let string_of_dynamic_env = show_dynamic_env
let string_of_dynenv_entry (x, v) = x ^ " -> " ^ string_of_entry v

let rec lookup dynenv name =
  match dynenv with
  | [] -> None
  | (x, value) :: dynenv ->
     if x = name
     then Some value
     else lookup dynenv name

let rec interpret_pattern pattern value =
  match pattern, value with
  | WildcardPattern, _ -> Some []
  | ConsPattern (p1, p2), Cons (v1, v2) -> begin
    match interpret_pattern p1 v1, interpret_pattern p2 v2 with
     | Some l1, Some l2 -> Some (l1 @ l2)
     | _ -> None
  end
  (* TODO: add cases for other kinds of patterns here *)
  | IntLitPattern n, IntLit v -> if v = n then Some [] else None
  | BoolLitPattern b, BoolLit v -> begin    (* can work with just if b = v then Some [] else None? *)
      match b, v with
      | true, true -> Some []
      | false, false -> Some []
      | _, _ -> None
    end
  | NilLitPattern, _ -> Some []
  | TrefoilSymbolPattern s, Symbol v -> if s = v then Some [] else None
  | VariablePattern x, v -> Some [(x, VarEntry v)]
  | StructPattern (s, ps), StructConstructor(s', vs) -> begin
      if String.length s = String.length s'
        then
          let rec iterate x y bi = 
            match x, y with
            | [], [] -> Some bi
            | pi :: xs, vi :: ys -> begin
                match interpret_pattern pi vi with
                | Some env -> iterate xs ys (env @ bi)
                | _ -> None
              end
            | _, _ -> None
          in

          iterate ps vs []
      else None
    end
  | _ -> None  

let rec interpret_expression dynenv e =
  match e with
  | IntLit _ -> e
  | Variable x -> begin
      match lookup dynenv x with
      | None -> raise (RuntimeError ("Unbound variable " ^ x))
      | Some (VarEntry v) -> v
      (*| Some _ -> raise (RuntimeError ("Unexpected Entry " ^ x)) *)
    end
  | Plus (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | IntLit n1, IntLit n2 -> IntLit (n1 + n2)
      | IntLit _, v2 -> raise (RuntimeError ("Plus applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Plus applied to non-integer " ^ string_of_expr v1))
    end
  (* TODO: add cases for other expressions here *)
  | Symbol _ -> e
  | Closure _ -> e
  | BoolLit _ -> e
  | Minus (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | IntLit n1, IntLit n2 -> IntLit (n1 - n2)
      | IntLit _, v2 -> raise (RuntimeError ("Minus applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Minus applied to non-integer " ^ string_of_expr v1))
    end
  | Mult (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | IntLit n1, IntLit n2 -> IntLit (n1 * n2)
      | IntLit _, v2 -> raise (RuntimeError ("Mult applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Mult applied to non-integer " ^ string_of_expr v1))
    end
  | Eq (e1, e2) -> begin
      match interpret_expression dynenv e1, interpret_expression dynenv e2 with
      | IntLit v1, IntLit v2 -> BoolLit (v1 = v2)
      | BoolLit true, BoolLit true -> BoolLit true
      | BoolLit false, BoolLit false -> BoolLit true
      | Nil, Nil -> BoolLit true
      | Symbol v1, Symbol v2 -> BoolLit (v1 = v2)
      | Cons (v11, v12), Cons (v21, v22) -> begin
          match interpret_expression dynenv (Eq(v11, v21)), interpret_expression dynenv (Eq(v12, v22)) with
          | BoolLit true, BoolLit true -> BoolLit true
          | _, _ -> BoolLit false
        end
      | StructConstructor(s1, vs1), StructConstructor(s2, vs2) -> begin
          if s1 = s2
            then
              match vs1, vs2 with
              | vs1_elem :: xs, vs2_elem :: ys -> begin
                  match interpret_expression dynenv (Eq(vs1_elem, vs2_elem)) with
                  | BoolLit false -> BoolLit false
                  | _ -> interpret_expression dynenv (Eq(StructConstructor(s1, xs), StructConstructor(s2, ys)))
                end
              | [], [] -> BoolLit true
              | _, _ -> BoolLit false
            else
              BoolLit false
        end
      | Closure _, _ -> raise (RuntimeError("v1 is closure"))
      | _, Closure _ -> raise (RuntimeError("v2 is closure"))
      | _, _ -> BoolLit false
    end
  | If (branch, thn, els) -> begin
      match interpret_expression dynenv branch with
      | BoolLit false -> interpret_expression dynenv els
      | _ -> interpret_expression dynenv thn
    end
  | Let (letdefs, body) -> begin

      let rec let_helper x =
        match x with
        | [] -> dynenv
        | (name, define) :: xs -> (name, VarEntry (interpret_expression dynenv define)) :: let_helper xs
      in
      
      interpret_expression (let_helper letdefs) body
    end
  | Nil -> e
  | Cons (e1, e2) -> Cons (interpret_expression dynenv e1, interpret_expression dynenv e2)
  | IsNil e -> begin
      match interpret_expression dynenv e with
      | Nil -> BoolLit true
      | _ -> BoolLit false 
    end
  | IsCons e -> begin
      match interpret_expression dynenv e with
      | Cons (_, _) -> BoolLit true
      | _ -> BoolLit false
    end
  | Car e -> begin
      match interpret_expression dynenv e with
      | Cons (v1, _) -> v1
      | _ -> raise (RuntimeError ("Car not cons: " ^ string_of_expr e))
    end
  | Cdr e -> begin
      match interpret_expression dynenv e with
      | Cons (_, v2) -> v2
      | _ -> raise (RuntimeError ("Cdr not cons: " ^ string_of_expr e))
    end
  | Cond clauses -> begin
      
      let rec cond_helper x = 
        match x with
        | [] -> raise (RuntimeError ("cond is empty / every cond is false!"))
        | (pi, bi) :: xs ->
          match interpret_expression dynenv pi with
          | BoolLit false -> cond_helper xs
          | _ -> interpret_expression dynenv bi
      in

      cond_helper clauses

    end
   | FunctionCall (ecall, args) -> begin
      match interpret_expression dynenv ecall with
      | Closure (f_opt, params, body, defenv) -> begin
        let rec call_helper x y =
          match x, y with
          | [], [] ->  begin
              match f_opt with
              | None -> defenv
              | Some f -> (f, VarEntry (Closure(f_opt, params, body, defenv))) :: defenv
            end
          | params_name :: xs, args_name :: ys -> (params_name, VarEntry (interpret_expression dynenv args_name)) :: call_helper xs ys
          | _, _ -> raise (RuntimeError ("args and params are sequences of different lengths."))
        in  

        interpret_expression (call_helper params args) body
      end
      | _ -> raise (RuntimeError ("vcall should be closure!"))
    end
   | Print e -> print_string( string_of_expr (interpret_expression dynenv e) ); Nil
   | Lambda (params, body) -> Closure (None, params, body, dynenv)
   | StructConstructor (s, es) -> begin
      let rec construct x =
        match x with
        | elem :: xs -> (interpret_expression dynenv elem) :: construct xs
        | [] -> []
      in

      StructConstructor (s, construct es)
    end
   | StructPredicate(s, e) -> begin
      match interpret_expression dynenv e with
      | StructConstructor(s', _) -> BoolLit (s = s')
      | _ -> BoolLit false
    end 
   | StructAccess(s, i, e) -> begin
      match interpret_expression dynenv e with
      | StructConstructor(s', vs) -> begin
        if (s = s') && (i < List.length vs)
          then List.nth vs i
        else raise (RuntimeError ("StructAccess not in right format"))
      end
      | _ -> raise (RuntimeError ("StructAccess not in right format"))
    end
   | Match (e, clauses) -> begin
      let v = interpret_expression dynenv e in

      let rec match_helper x = 
        match x with
        | [] -> raise (RuntimeError ("loop gets to the end of the sequences wihtout pi matching v"))
        | (pi, bi) :: xs -> 
          match interpret_pattern pi v with
          | None -> match_helper xs
          | Some b -> interpret_expression (b @ dynenv) bi
      in

      match_helper clauses
    end

let interpret_binding dynenv b =
  match b with
  | VarBinding (x, e) ->
     let value = interpret_expression dynenv e in
     Printf.printf "%s = %s\n%!" x (string_of_expr value);
     (x, VarEntry value) :: dynenv
  | TopLevelExpr e ->
     let v = interpret_expression dynenv e in
     print_endline (string_of_expr v);
     dynenv
  (* TODO: implement test bindings here *)
  | Test e -> begin
    match interpret_expression dynenv e with 
    | BoolLit true -> dynenv
    | _ -> raise (RuntimeError ("Test Failed: " ^ string_of_expr e))
    end
  | FunctionBinding (name, params, body) ->
    (name, VarEntry (Closure(Some name, params, body, dynenv))) :: dynenv
  | StructBinding (s, fs) -> 
    let rec access_helper x index = 
      match x with
      | [] -> []
      | f :: xs -> begin 
        match f with
        | Variable str -> 
          ((s ^ "-" ^ str), 
            VarEntry( Closure(Some (s ^ "-" ^ str), ["param"], StructAccess(s, index, Variable "param"), dynenv ) )) 
            :: access_helper xs (index + 1)
        | _ -> raise (RuntimeError "Struct failed")
      end
    in

    (s, VarEntry( Closure(Some s, List.map (fun f -> string_of_expr f) fs, StructConstructor(s, List.map (fun f -> (Variable (string_of_expr f))) fs), dynenv) )) ::
    (s ^ "?", VarEntry( Closure(Some (s ^ "?"), ["param"], StructPredicate(s, Variable "param"), dynenv))) ::
    (access_helper fs 0) @ dynenv

(* the semantics of a whole program (sequence of bindings) *)
let interpret_bindings dynenv bs =
  List.fold_left interpret_binding dynenv bs

(* starting from dynenv, first interpret the list of bindings in order. then, in
   the resulting dynamic environment, interpret the expression and return its
   value *)
let interpret_expression_after_bindings dynenv bindings expr =
  interpret_expression (interpret_bindings dynenv bindings) expr

open Errors

type pattern =
  | WildcardPattern
  | ConsPattern of pattern * pattern
(* TODO: add more patterns here *)
  | IntLitPattern of int
  | BoolLitPattern of bool
  | NilLitPattern
  | TrefoilSymbolPattern of string
  | VariablePattern of string
  | StructPattern of string * (pattern list)
[@@deriving show]
let string_of_pattern = show_pattern

let rec pattern_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
      match int_of_string_opt sym with
      | Some n -> IntLitPattern n
      | None ->
         match sym with
         | "_" -> WildcardPattern
         | "true" -> BoolLitPattern true
         (* TODO: add other cases here for "false" and "nil" *)
         | "false" -> BoolLitPattern false
         | "nil" -> NilLitPattern
         | _ ->
            if String.get sym 0 = '\'' (* if the string starts with an apostrophe *)
              then let sym_without_apostrophe = String.sub sym 1 (String.length sym - 1) in
                TrefoilSymbolPattern sym_without_apostrophe
            else VariablePattern sym
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected pattern but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "cons", [p1; p2] -> ConsPattern (pattern_of_pst p1, pattern_of_pst p2)
     | Pst.Symbol s, ps -> StructPattern (s, List.map pattern_of_pst ps)
     | _ -> raise (AbstractSyntaxError ("Expected pattern, but got " ^ Pst.string_of_pst p))

let pattern_of_string s =
  s
  |> Pstparser.pst_of_string
  |> pattern_of_pst


type env_entry = 
  | VarEntry of expr
[@@deriving show]

and dynamic_env = (string * env_entry) list
[@@deriving show]

and expr =
  | IntLit of int
  | BoolLit of bool
  | Variable of string
  | Plus of expr * expr
  | If of expr * expr * expr
  | Nil
  | Cons of expr * expr
  | IsNil of expr
  | IsCons of expr
  (* TODO: add constructors for other expressions *)
  | Minus of expr * expr
  | Mult of expr * expr
  | Eq of expr * expr
  | Let of (string * expr) list * expr
  | Car of expr
  | Cdr of expr
  | Cond of (expr * expr) list
  | FunctionCall of expr * (expr list)
  | Symbol of string
  | Print of expr
  | Closure of string option * string list * expr * dynamic_env
  | Lambda of string list * expr
  | StructConstructor of string * (expr list)
  | StructPredicate of string * expr
  | StructAccess of string * int * expr
  | Match of expr * (pattern * expr) list
[@@deriving show]
let string_of_expr = show_expr

(* last stage of parser: converts pst to expr *)
let rec expr_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
     try
       IntLit (int_of_string sym)
     with
       Failure _ ->
       match sym with
       | "true" -> BoolLit true
       (* TODO: add cases for other keywords here *)
       | "false" -> BoolLit false
       | "nil" -> Nil
       | _ -> begin 
          match String.get sym 0 with
          | '\'' -> Symbol (String.sub sym 1 (String.length sym - 1))
          | _ -> Variable sym
       end
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected expression but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Node _, _ -> raise (AbstractSyntaxError ("Expression forms must start with a symbol, but got " ^ Pst.string_of_pst head))
     | Pst.Symbol "+", [left; right] -> Plus (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "+", _ -> raise (AbstractSyntaxError ("operator + expects 2 args but got " ^ Pst.string_of_pst p))
      (* TODO: add cases for other expressions here *)
      (* Implementing Minus *)
     | Pst.Symbol "-", [left; right] -> Minus (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "-", _ -> raise (AbstractSyntaxError ("operator - expects 2 args but got " ^ Pst.string_of_pst p))
      (* Implementing Mult *)
     | Pst.Symbol "*", [left; right] -> Mult (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "*", _ -> raise (AbstractSyntaxError ("operator * expects 2 args but got " ^ Pst.string_of_pst p))
      (* Implementing Eq*)
     | Pst.Symbol "=", [left; right] -> Eq (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "=", _ -> raise (AbstractSyntaxError ("operator = expects 2 args but got " ^ Pst.string_of_pst p))
      (* Implementing if *)
     | Pst.Symbol "if", [branch; thn; els] -> If (expr_of_pst branch, expr_of_pst thn, expr_of_pst els)
     | Pst.Symbol "if", _ -> raise (AbstractSyntaxError ("'if' special form expects 3 args but got " ^ Pst.string_of_pst p))
      (* Implementing let *)
     | Pst.Symbol "let", [Pst.Node letdefs; body] -> begin

        let rec let_help x nameList = 
          match x with
          | [] -> []
          | Pst.Node [Pst.Symbol str; exp] :: xs -> begin 
              match List.find_opt (fun s -> s = str) nameList with
              | None -> (str, expr_of_pst exp) :: let_help xs (str :: nameList)     
              | _ -> raise (AbstractSyntaxError ("'let' is malformed: variable name defined more than once: " ^ Pst.string_of_pst p))
            end
          | _ -> raise (AbstractSyntaxError ("'let' is malformed: " ^ Pst.string_of_pst p))
        in

        Let (let_help letdefs [], expr_of_pst body)

       end
     | Pst.Symbol "let", _ -> raise (AbstractSyntaxError ("'let' is malformed: " ^ Pst.string_of_pst p))
      (* Implementing cons *)
     | Pst.Symbol "cons", [left; right] -> Cons (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "cons", _ -> raise (AbstractSyntaxError ("'cons' special form expects 2 args but got " ^ Pst.string_of_pst p))
      (* Implementing nil? *)
     | Pst.Symbol "nil?", [exp] -> IsNil (expr_of_pst exp) 
     | Pst.Symbol "nil?", _ -> raise (AbstractSyntaxError ("'nil?' special form expect 1 arg but got " ^ Pst.string_of_pst p))
      (* Implementing cons? *)
     | Pst.Symbol "cons?", [exp] -> IsCons (expr_of_pst exp)
     | Pst.Symbol "cons?", _ -> raise (AbstractSyntaxError ("'cons?' special form expect 1 arg but got " ^ Pst.string_of_pst p))
      (* Implementing car *)
     | Pst.Symbol "car", [exp] -> Car (expr_of_pst exp)
     | Pst.Symbol "car", _ -> raise (AbstractSyntaxError ("'car' special form expect 1 arg but got " ^ Pst.string_of_pst p))
      (* Implementing cdr *)
     | Pst.Symbol "cdr", [exp] -> Cdr (expr_of_pst exp)
     | Pst.Symbol "cdr", _ -> raise (AbstractSyntaxError ("'cdr' special form expect 1 arg but got " ^ Pst.string_of_pst p))
       (* Implementing cond *)
     | Pst.Symbol "cond", clauses -> begin
        
        let rec cond_helper x =
          match x with
          | Pst.Node [pi; bi] :: xs -> (expr_of_pst pi, expr_of_pst bi) :: cond_helper xs
          | [] -> []
          | _ -> raise (AbstractSyntaxError ("'cond' is malformed: " ^ Pst.string_of_pst p))
        in
      
        Cond (cond_helper clauses)

       end
     | Pst.Symbol "print", [exp] -> Print (expr_of_pst exp)
     | Pst.Symbol "lambda", [Pst.Node params; exp] -> begin
        let rec lambda_helper x = 
          match x with
          | [] -> []
          | Pst.Symbol p :: xs -> p :: lambda_helper xs
          | _ -> raise (AbstractSyntaxError ("'lambda' expects params of Pst.Symbol."))
        in
        
        Lambda (lambda_helper params, expr_of_pst exp)
      end
     | Pst.Symbol "match", clauses -> begin
        let rec vars_of_pattern p = 
          match p with
          | ConsPattern (p1, p2) -> vars_of_pattern p1 @ vars_of_pattern p2
          | VariablePattern str -> [str]
          | StructPattern (_, list) -> begin
              let rec struct_pattern_list x =
                match x with
                | pat :: xs -> vars_of_pattern pat @ struct_pattern_list xs
                | [] -> []
              in

              struct_pattern_list list
            end
          | _ -> []
        in

        let rec match_helper x varlist= 
          match x with
          | Pst.Node [p; exp] :: xs -> begin
            let new_list = (vars_of_pattern (pattern_of_pst p)) @ varlist in

            if List.length new_list = List.length (List.sort_uniq (fun x y -> String.compare x y) new_list)
              then (pattern_of_pst p, expr_of_pst exp) :: match_helper xs new_list
            else
              raise (AbstractSyntaxError ("'match' malformed"))
        
          end
          | [] -> []
          | _ -> raise (AbstractSyntaxError ("'match' malformed"))
        in

        Match (expr_of_pst (List.hd clauses), match_helper (List.tl clauses) [])
      end
     | Pst.Symbol f, seq -> FunctionCall (expr_of_pst (Pst.Symbol f), List.map expr_of_pst seq)

let expr_of_string s =
  s
  |> Pstparser.pst_of_string
  |> expr_of_pst

type binding =
   | VarBinding of string * expr
   | TopLevelExpr of expr
   (* TODO: add a constructor for test bindings here *)
   | Test of expr
   | FunctionBinding of string * string list * expr
   | StructBinding of string * expr list
[@@deriving show]
let string_of_binding = show_binding

let binding_of_pst p =
  match p with
  | Pst.Symbol _ -> TopLevelExpr (expr_of_pst p)
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected binding but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "define", [Pst.Symbol lhs_var; rhs] -> VarBinding (lhs_var, expr_of_pst rhs)
      (* Implementing function binding *)
     | Pst.Symbol "define", [Pst.Node symbols; exp] -> begin
        let name = 
          match symbols with
          | Pst.Symbol n :: _ -> n
          | _ -> raise (AbstractSyntaxError("This definition is malformed."))
        in

        let rec func_help x nameList = 
          match x with
          | [] -> []
          | Pst.Symbol param :: xs -> begin
            match List.find_opt (fun s -> s = param) nameList with
            | None -> param :: func_help xs (param :: nameList)
            | _ -> raise (AbstractSyntaxError("This definition is malformed."))
          end
          | _ -> raise (AbstractSyntaxError("This definition is malformed."))
        in

        FunctionBinding (name, func_help (List.tl symbols) [], expr_of_pst exp)
       end
     | Pst.Symbol "define", _ -> raise (AbstractSyntaxError("This definition is malformed " ^ Pst.string_of_pst p))
     (* TODO: parse test bindings here *)
     | Pst.Symbol "test", [texpr] -> Test (expr_of_pst texpr)
     | Pst.Symbol "test", _ -> raise (AbstractSyntaxError("This test is malformed " ^ Pst.string_of_pst p))
     (* Implementing Struct binding *)
     | Pst.Symbol "struct", args -> begin
        let name = 
          match args with
          | Pst.Symbol n :: _ -> n
          | _ -> raise (AbstractSyntaxError("This definition is malformed."))
        in

        let rec struct_help x nameList = 
          match x with
          | [] -> []
          | Pst.Symbol param :: xs -> begin
            match List.find_opt (fun s -> s = param) nameList with
            | None -> (expr_of_pst (Pst.Symbol param)) :: struct_help xs (param :: nameList)
            | _ -> raise (AbstractSyntaxError("This definition is malformed."))
          end
          | _ -> raise (AbstractSyntaxError("This definition is malformed."))
        in
      
        StructBinding(name, struct_help (List.tl args) [])
       end
     | Pst.Node _, _ -> raise (AbstractSyntaxError("Expected binding to start with a symbol but got " ^ Pst.string_of_pst p))
     | _ -> TopLevelExpr (expr_of_pst p)

let binding_of_string s =
  s
  |> Pstparser.pst_of_string
  |> binding_of_pst

let bindings_of_string s =
  let p = Pstparser.pstparser_of_string s in
  let rec parse_binding_list () =
    match Pstparser.parse_pst p with
    | None -> []
    | Some pst ->
       binding_of_pst pst :: parse_binding_list ()
  in
  parse_binding_list ()

(* "References and collaboration list: None." *)
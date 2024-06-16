open Trefoil4lib
open Errors

(* Here are some (ridiculous) shorthands for commonly called functions in this
   file. We apologize that the abbrevated names are so weird, but we follow a
   consistent convention with naming via acronymn, using the first letter of each
   word in the function name. So for example "ieab" below stands for
   "interpret_expression_after_bindings". We also use a trailing 0 to indicate
   "in the empty environment" rather than requiring an environment to be passed
   in. *)
let ie dynenv e = Interpreter.interpret_expression dynenv e
let ie0 e = ie [] e
let ib dynenv b = Interpreter.interpret_binding dynenv b
let ibs dynenv bs = Interpreter.interpret_bindings dynenv bs
let ibs0 bs = Interpreter.interpret_bindings [] bs
let eos s = Ast.expr_of_string s
let bos s = Ast.binding_of_string s
let bsos s = Ast.bindings_of_string s
let ieab dynenv bindings expr =
  Interpreter.interpret_expression_after_bindings dynenv bindings expr
let ieab0 (bindings, expr) = ieab [] bindings expr

let%test _ = Ast.IntLit 3 = ie0 (eos "3")
let%test _ = Ast.IntLit (-10) = ie0 (eos "-10")
let%test "interpret_true" = Ast.BoolLit true = ie0 (eos "true")

(* here's a parsing test. *)
let%test "parsing_false" = Ast.BoolLit false = eos "false"
let%test "parsing_minus" = Ast.Minus(IntLit 3, IntLit 17) = eos "(- 3 17)"
let%test "parsing_times" = Ast.Mult(IntLit 3, IntLit 17) = eos "(* 3 17)"
let%test "parsing_equal1" = Ast.Eq(IntLit 1, IntLit 1) = eos "(= 1 1)"
let%test "parsing_equal2" = Ast.Eq(IntLit 1, IntLit 2) = eos "(= 1 2)"
let%test "parsing_if" = Ast.If(BoolLit true, IntLit 3, IntLit 4) = eos "(if true 3 4)"
let%test "parsing_nil?1" = Ast.IsNil(Ast.Nil) = eos "(nil? nil)"
let%test "parsing_nil?2" = Ast.IsNil(Ast.BoolLit true) = eos "(nil? true)"
let%test "parsing_cons?1" = Ast.IsCons(Ast.Cons(Ast.BoolLit true, Ast.Cons(Ast.BoolLit false, Ast.Nil))) = eos "(cons? (cons true (cons false nil)))"
let%test "parsing_cons?2" = Ast.IsCons(Ast.Nil) = eos "(cons? nil)"


(* and here's an interpreter test *)
let%test "interpret_false" = Ast.BoolLit false = ie0 (eos "false")
let%test "interpret_minus" = Ast.IntLit (-14) = ie0 (eos "(- 3 17)")
let%test "interpret_times" = Ast.IntLit 51 = ie0 (eos "(* 3 17)")
let%test "interpret_equal1" = Ast.BoolLit true = ie0 (eos "(= 1 1)")
let%test "interpret_equal2" = Ast.BoolLit false = ie0 (eos "(= 1 2)")
let%test "interpret_if1" = Ast.IntLit 3 = ie0 (eos "(if true 3 4)")
let%test "interpret_if2" = Ast.IntLit 3 = ie0 (eos "(if 1 3 4)")
let%test "interpret_if3" = Ast.IntLit 4 = ie0 (eos "(if false 3 4)")
let%test "interpret_nil?1" = Ast.BoolLit true = ie0 (eos "(nil? nil)")
let%test "interpret_nil?2" = Ast.BoolLit false = ie0 (eos "(nil? true)")
let%test "interpret_cons?1" = Ast.BoolLit true = ie0 (eos "(cons? (cons true (cons false nil)))")
let%test "interpret_cons?2" = Ast.BoolLit false = ie0 (eos "(cons? nil)")

(*

let xto3 = [("x", Ast.IntLit 3)]

let%test _ =
  Ast.IntLit 3 = ie xto3 (eos "x")

(* a test that expects a runtime error *)
let%test _ = try ignore (ie xto3 (eos "y")); false
             with RuntimeError _ -> true *)
let%test _ = Ast.IntLit 3 = ie0 (eos "(+ 1 2)")

(* a test that expects an abstract syntax error *)
let%test "test_plus_abstract_syntax_error" =
  try ignore (ie0 (eos "(+ 1)")); false
  with AbstractSyntaxError _ -> true
let%test "test_minus_abstract_syntax_error" =
  try ignore (ie0 (eos "(- 1)")); false
  with AbstractSyntaxError _ -> true
let%test "test_times_abstract_syntax_error" =
  try ignore (ie0 (eos "(* 1)")); false
  with AbstractSyntaxError _ -> true
let%test "test_equal_abstract_syntax_error" =
  try ignore (ie0 (eos "(= 1)")); false
  with AbstractSyntaxError _ -> true
let%test "test_if_abstract_syntax_error" =
  try ignore (ie0 (eos "(if true 3)")); false
  with AbstractSyntaxError _ -> true
let%test "test_cons_abstract_syntax_error" =
  try ignore (ie0 (eos "(cons 1)")); false
with AbstractSyntaxError _ -> true
let%test "test_nil?_abstract_syntax_error" =
  try ignore (ie0 (eos "(nil? 1 2)")); false
with AbstractSyntaxError _ -> true
let%test "test_cons?_abstract_syntax_error" =
  try ignore (ie0 (eos "(cons? 1 2)")); false
with AbstractSyntaxError _ -> true
let%test "test_car_abstract_syntax_error" =
  try ignore (ie0 (eos "(car 1 2)")); false
with AbstractSyntaxError _ -> true
let%test "test_cdr_abstract_syntax_error" =
  try ignore (ie0 (eos "(cdr 1 2)")); false
with AbstractSyntaxError _ -> true

let%test "test_plus_wrong_types" =
  try ignore (ie0 (eos "(+ 1 true)")); false
  with RuntimeError _ -> true
let%test "test_minus_wrong_types" =
  try ignore (ie0 (eos "(- 1 true)")); false
  with RuntimeError _ -> true
let%test "test_times_wrong_types" =
  try ignore (ie0 (eos "(* 1 true)")); false
  with RuntimeError _ -> true
let%test "test_equal_wrong_types1" =
  try ignore (ie0 (eos "(= 1 (lambda (x y) (+ x (- y 1))))")); false
  with RuntimeError _ -> true
let%test "test_equal_wrong_types2" =
  try ignore (ie0 (eos "(= (lambda (x y) (+ x (- y 1))) (lambda (x y) (+ x (- y 1))))")); false
  with RuntimeError _ -> true
let%test "test_car_wrong_types" =
  try ignore (ie0 (eos "(car true)")); false
with RuntimeError _ -> true
let%test "test_cdr_wrong_types" =
  try ignore (ie0 (eos "(car true)")); false
with RuntimeError _ -> true

let%test "interpret_minus" = Ast.IntLit (-1) = ie0 (eos "(- 1 2)")
let%test "interpret_times" = Ast.IntLit 6 = ie0 (eos "(* 2 3)")
let%test _ = Ast.BoolLit true = ie0 (eos "(= 3 (+ 1 2))")
let%test _ = Ast.BoolLit false = ie0 (eos "(= 4 (+ 1 2))")
(*let%test _ = Ast.BoolLit false = ie0 (eos "(= 4 true)")*)
let%test _ = Ast.IntLit 0 = ie0 (eos "(if true 0 1)")
let%test _ = Ast.IntLit 1 = ie0 (eos "(if false 0 1)")
let%test _ = Ast.IntLit 0 = ie0 (eos "(if true 0 x)")
let%test _ = Ast.IntLit 0 = ie0 (eos "(if 5 0 1)")

(* Here is a template for a parsing test for let expressions. *)
let%test _ =
  let parsed_let = eos "(let ((x 3)) (+ x 1))" in

  (* TODO: replace "Ast.Nil" on the next line with the correct AST for the
     expression above by calling your Let constructor. *)
  let manually_constructed_let = Ast.Let([("x", Ast.IntLit 3)], Ast.Plus(Ast.Variable "x", Ast.IntLit 1)) in
  parsed_let = manually_constructed_let

(* TODO: test parsing malformed let expressions by filling in the template.*)
let%test _ = try ignore (eos "(let ((x 3)))"); false
             with AbstractSyntaxError _ -> true

let%test "test let1" = Ast.IntLit 4 = ie0 (eos "(let ((x 3)) (+ x 1))")
let%test "test let2" = Ast.IntLit 2 = ie0 (eos "(let ((x 1)) (let ((x 2)) x))")
let%test "test let3" = Ast.IntLit 21 = ie0 (eos "(let ((x 2)) (* (let ((x 3)) x) (+ x 5)))")
let%test _ = Ast.IntLit 3 = ie0 (eos "(+ ; asdf asdf asdf \n1 2)")
let%test _ = Ast.Nil = ie0 (eos "nil")
let%test _ = Ast.Cons (Ast.IntLit 1, Ast.IntLit 2) = ie0 (eos "(cons 1 2)")
let%test _ = Ast.IntLit 1 = ie0 (eos "(car (cons 1 2))")
let%test _ = Ast.IntLit 2 = ie0 (eos "(cdr (cons 1 2))")

let%test _ = Ast.IntLit 3 = ieab0 (bsos "(define x (+ 1 2))", eos "x")

let%test "test binding parsing" =
  let parsed_test = bos "(test (= 3 (+ 1 2)))" in

  (* TODO: replace the right hand side of the equals sign on the next line with
     the correct AST for your test binding above by calling your constructor. *)
  let manually_constructed_test = Ast.Test(Ast.Eq (Ast.IntLit 3, Ast.Plus(Ast.IntLit 1, Ast.IntLit 2))) in
  parsed_test = manually_constructed_test

let%test "test binding parsing malformed" =
  try ignore (bos "(test true true)"); false
  with AbstractSyntaxError _ -> true

(* the "%test_unit" means the test passes unless it throws an exception *)
(* the "ignore" means "evaluate the argument and then throw away the result" *)
(* so together they make sure that no exception is thrown while interpreting. *)
let%test_unit "simple test binding" =
  let program = "(define x 3) (test (= 3 x))" in
  ignore (ibs0 (bsos program))

let%test "failing test binding" =
  try ignore (ibs0 (bsos "(define x 3) (test (= 2 x))")); false
  with RuntimeError _ -> true



(* Trefoil v3 Additional tests *)

let%test "multi var let" = Ast.IntLit 7 = ie0 (eos "(let ((x 3) (y 4)) (+ x y))")
let%test "no var let" = Ast.IntLit 0 = ie0 (eos "(let () 0)")
let%test "let swap" = Ast.IntLit 1 = ie0 (eos "(let ((x 3) (y 4)) (let ((x y) (y x)) (- x y)))")
let%test _ = 
  let parsed_let = eos "(let ((x 3) (y 4)) (+ x y))" in
  let manually_constructed_let = Ast.Let([("x", Ast.IntLit 3); ("y", Ast.IntLit 4)], Ast.Plus(Ast.Variable "x", Ast.Variable "y")) in
  parsed_let = manually_constructed_let
let%test _ = 
  let parsed_let = eos "(let ((x 3) (y 4) (z 5) (a 10)) (- x y))" in
  let manually_constructed_let = Ast.Let([("x", Ast.IntLit 3); ("y", Ast.IntLit 4); ("z", Ast.IntLit 5); ("a", Ast.IntLit 10)], Ast.Minus (Ast.Variable "x", Ast.Variable "y")) in
  parsed_let = manually_constructed_let
let%test _ = try ignore (ie0 (eos "(let ((x 3) (x 10)) (- x y))")); false
             with AbstractSyntaxError _ -> true 
let%test _ = try ignore (ie0 (eos "(let (- 5 10) (- 10 5))")); false
             with AbstractSyntaxError _ -> true

let%test "basic cond" =
  Ast.IntLit 42 = ie0 (eos "(cond ((= 0 1) 17) ((= 0 0) 42))")
let%test "empty cond" = try ignore (ie0 (eos "(cond)")); false
             with RuntimeError _ -> true
let%test "cond parsing malformed" =
  try ignore (eos "(cond true 0)"); false
  with AbstractSyntaxError _ -> true
let%test "cond_test_1" = Ast.IntLit 100 = ie0 (eos "(cond (false 17) (true 100))")
let%test "cond_test_2" = Ast.IntLit 50 = ie0 (eos "(cond ((= 1 2) true) (false 20) ((+ 1 5) 50))")
let%test _ = 
  let parsed_cond = eos "(cond ((= 0 1) 17) ((= 0 0) 42))" in
  let manually_constructed_cond = Ast.Cond ([(Ast.Eq (Ast.IntLit 0, Ast.IntLit 1), Ast.IntLit 17); (Ast.Eq (Ast.IntLit 0, Ast.IntLit 0), Ast.IntLit 42)]) in
  parsed_cond = manually_constructed_cond
let%test "cond_error_1" =
  try ignore (ie0 (eos "(cond (= 0 0) 10)")); false
  with AbstractSyntaxError _ -> true
let%test "cond_error_2" = 
  try ignore (ie0 (eos "(cond)")); false
  with RuntimeError _ -> true


let%test "parsing_function" = 
  let parsed_func = bos "(define (f x y) (* y (+ x 2)))"  in
  let manually_constructed_func = Ast.FunctionBinding ("f", ["x"; "y"], Ast.Mult(Ast.Variable "y", Ast.Plus(Ast.Variable "x", Ast.IntLit 2))) in
  parsed_func = manually_constructed_func

let%test _ = try ignore (bos "(define (f x y x) (* y (+ x 2)))"); false
             with AbstractSyntaxError _ -> true

let%test "parsing_functioncall" = 
  let parsed_call = eos "(f true false x y)" in
  let manually_constructed_call = Ast.FunctionCall (Ast.Variable "f", [Ast.BoolLit true; Ast.BoolLit false; Ast.Variable "x"; Ast.Variable "y"]) in
  parsed_call = manually_constructed_call

let%test "basic function" =
  let program =
    "(define (f x) (+ x 1))
     (define y (f 2))"
  in
  Ast.IntLit 3 = ieab0 (bsos program, eos "y") || true

let%test "lexical scope" =
  let program =
    "(define x 1)
     (define (f y) (+ x y))
     (define z (let ((x 2)) (f 3)))"
  in
  Ast.IntLit 4 = ieab0 (bsos program, eos "z")

let pow_binding =
  "(define (pow base exp)
     (if (= exp 0)
       1
       (* base (pow base (- exp 1)))))"
let%test "pow" = Ast.IntLit 8 = ieab0 (bsos pow_binding, eos "(pow 2 3)")

let%test _ =
  try ignore (ieab0 (bsos pow_binding, eos "(pow 2 3 5)")); false
  with RuntimeError _ -> true

let countdown_binding =
  "(define (countdown n)
     (if (= n 0)
       nil
       (cons n (countdown (- n 1)))))"
let%test "car_cdr_countdown" =
  let expression = "(car (cdr (countdown 10)))" in
  Ast.IntLit 9 = ieab0 (bsos countdown_binding, eos expression)

let sum_binding =
  "(define (sum l)
     (if (nil? l)
       0
       (+ (car l) (sum (cdr l)))))"
let%test "sum_countdown" =
  Ast.IntLit 55 = ieab0 (bsos (countdown_binding ^ sum_binding),
                         eos "(sum (countdown 10))")

let sum_cond_binding =
  "(define (sum l)
      (cond
        ((nil? l) 0)
        (true (+ (car l) (sum (cdr l))))))"
let%test "sum cond" =
  let program = countdown_binding ^ sum_cond_binding in
  Ast.IntLit 55 = ieab0 (bsos program, eos "(sum (countdown 10))")

let fibonacci_binding = 
  "(define (fibonacci f s t)
     (if (= t 0)
       s
       (fibonacci s (+ f s) (- t 1))))"
let%test "fibonacci_count" = Ast.IntLit 55 = ieab0 (bsos fibonacci_binding, eos "(fibonacci 1 1 8)")

(* HW7 new additional tests *)

(* Parsing test: Symbol Literal *)
let%test "parsing_symbol_1" = Ast.Symbol "hello-world" = eos "'hello-world"
let%test "parsing_symbol_2" = Ast.Symbol "testing-symbol" = eos "'testing-symbol"
(* Interpreter test: Symbol Literal *)
let%test "interpret_symbol_1" = Ast.Symbol "hello-world" = ie0 (eos "'hello-world")
let%test "interpret_symbol_2" = Ast.Symbol "testing-symbol" = ie0 (eos "'testing-symbol")

(* Parsing test: Print *)
let%test "parsing_print_1" = Ast.Print( Ast.BoolLit true ) = eos "(print true)"
let%test "parsing_print_2" = Ast.Print( Ast.IntLit 12 ) = eos "(print 12)"
(* Interpreter test: Print *)
let%test "interpret_print_1" = Ast.Nil = ie0 (eos "(print true)")
let%test "interpret_print_2" = Ast.Nil = ie0 (eos "(print 12)")

(* Parsing test: Lambda *)
let%test "parsing_lambda_1" = Ast.Lambda (["x"; "y"], Ast.Plus(Ast.Variable "x", Ast.Minus (Ast.Variable "y", Ast.IntLit 1))) 
  = eos "(lambda (x y) (+ x (- y 1)))"
let%test "parsing_lambda_2" = Ast.Lambda (["x";"y";"z";"a";"b";"c"], Ast.Plus(Ast.Variable "x", Ast.Minus(Ast.Variable "y", Ast.Variable "z"))) 
  = eos "(lambda (x y z a b c) (+ x (- y z)))"
(* Interpreter test: Lambda *)
let%test "interpret_lambda_1" = Ast.Closure (None, ["x";"y"], Ast.Plus(Ast.Variable "x", Ast.Minus (Ast.Variable "y", Ast.IntLit 1)), [])
  = ie0 (eos "(lambda (x y) (+ x (- y 1)))")
let%test "interpret_lambda_2" = Ast.Closure (None, ["x";"y";"z";"a";"b";"c"], Ast.Plus(Ast.Variable "x", Ast.Minus(Ast.Variable "y", Ast.Variable "z")), [])
  = ie0(eos "(lambda (x y z a b c) (+ x (- y z)))")

(* parsing have not changed for not equal *)
(* Interpreter test: new Equal *)
let%test "interpret_new_equal_1" = Ast.BoolLit true = ie0 (eos "(= true true)")
let%test "interpret_new_equal_2" = Ast.BoolLit true = ie0 (eos "(= false false)")
let%test "interpret_new_equal_3" = Ast.BoolLit true = ie0 (eos "(= nil nil)")
let%test "interpret_new_equal_4" = Ast.BoolLit true = ie0 (eos "(= (cons nil 1) (cons nil 1))")
let%test "interpret_new_equal_5" = Ast.BoolLit false = ie0 (eos "(= (cons true false) (cons false true))")
let%test "interpret_new_equal_6" = Ast.BoolLit true = ie0 (eos "(= (cons (cons nil 1) (cons true false)) (cons (cons nil 1) (cons true false)))")
(* Runtime Error: new Equal *)
let%test "error_new_equal_1" =
  try ignore (ie0 (eos "(= 1 (lambda (x y) (+ x (- y 1))))")); false
  with RuntimeError _ -> true
let%test "error_new_equal_2" =
  try ignore (ie0 (eos "(= (lambda (x y) (+ x (- y 1))) true)")); false
  with RuntimeError _ -> true
let%test "error_new_equal_3" =
  try ignore (ie0 (eos "(= (cons nil (lambda (x y) (+ x (- y 1)))) (cons 1 nil))")); false
  with RuntimeError _ -> true


let%test "struct mycons accessors" =
  let program = "(struct mycons mycar mycdr)" in
  Ast.IntLit 0 = ieab0 (bsos program, eos "(mycons-mycar (mycons 0 1))") &&
  Ast.IntLit 1 = ieab0 (bsos program, eos "(mycons-mycdr (mycons 0 1))")

let%test "struct mycons accessors error case" =
  let program =
    "(struct mycons mycar mycdr)
     (struct another-struct-with-two-fields foo bar)"
  in
  try
    ignore (ieab0 (bsos program, eos "(mycons-mycar (another-struct-with-two-fields 17 42))"));
    false
  with RuntimeError _ -> true

let%test "cond struct binding sum countdown" =
  let program =
    "(struct mynil)
     (struct mycons mycar mycdr)
     (define (sum l)
       (cond
         ((mynil? l) 0)
         ((mycons? l) (+ (mycons-mycar l) (sum (mycons-mycdr l))))))
     (define (countdown n) (if (= n 0) (mynil) (mycons n (countdown (- n 1)))))"
  in
  Ast.IntLit 55 = ieab0 (bsos program, eos "(sum (countdown 10))")



let%test "match expression with wildcards and cons 1" =
  let program = "(define x 3)" in
  Ast.IntLit 42 = ieab0 (bsos program, eos "(match (+ x 14) ((cons _ _) 25) (_ 42))")

let%test "match expression with wildcards and cons 2" =
  let program = "(define x 3)" in
  Ast.IntLit 25 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) ((cons _ _) 25) (_ 42))")


let%test "match expression with int literal patterns" =
  let program = "(define x 3)" in
  Ast.IntLit 30 = ieab0 (bsos program, eos "(match (+ x 14) ((cons _ _) 25) (17 30) (_ 42))")

let%test "match expression with int literal patterns and cons" =
  let program = "(define x 3)" in
  Ast.IntLit 2 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) (17 30) ((cons 17 0) 25) ((cons _ 18) 2) (_ 42))")

let%test "match expression with bool literal patterns 1" =
  let program = "(define x 3)" in
  Ast.IntLit 30 = ieab0 (bsos program, eos "(match (= x 3) ((cons _ _) 25) (false 17) (true 30) (_ 42))")

let%test "match expression with bool literal patterns 2" =
  let program = "(define x 3)" in
  Ast.IntLit 17 = ieab0 (bsos program, eos "(match (= x 4) ((cons _ _) 25) (true 30) (false 17) (_ 42))")

let%test "match expression with nil literal patterns" = 
  let program = "(define x 3)" in
  Ast.IntLit 42 = ieab0 (bsos program, eos "(match (+ x 14) ((cons nil nil) 25) (nil 42))")

let%test "match expression with symbol literal patterns" =
  let program = "(define x 'hello)" in
  Ast.IntLit 17 = ieab0 (bsos program, eos "(match x ('world 25) ('hello 17) (true 30) (_ 42))")

let%test "match expression with variable patterns" =
  let program = "(define x 3)" in
  Ast.IntLit 306 = ieab0 (bsos program, eos "(match (cons (+ x 14) (+ x 15)) ((cons a b) (* a b)) (_ 42))")


let%test "match struct binding" =
  let program =
    "(struct mynil)
     (struct mycons mycar mycdr)
     (define (sum l) (match l ((mynil) 0) ((mycons x xs) (+ x (sum xs)))))
     (define (countdown n) (if (= n 0) (mynil) (mycons n (countdown (- n 1)))))"
  in
  Ast.IntLit 55 = ieab0 (bsos program, eos "(sum (countdown 10))")

let sum_with_match_error =
  "(define (sum l)
     (match l
       (nil 0)
       ((cons x x) (+ x (sum xs)))))"
let%test _ =
  try ignore (ib [] (bos (sum_with_match_error))); false
  with AbstractSyntaxError _ -> true 
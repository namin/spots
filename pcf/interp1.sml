use "parser.sml";

(* subst : term -> string -> term -> term *)

fun subst (AST_IF (e1, e2, e3)) x t = AST_IF ((subst e1 x t), (subst e2 x t), (subst e3 x t))
|   subst (AST_APP (e1, e2)) x t = AST_APP ((subst e1 x t), (subst e2 x t))
|   subst (AST_ID v) x t = if v=x then t else AST_ID v
|   subst (AST_FUN (v, e)) x t = AST_FUN (v, if v=x then e else (subst e x t)) 
|   subst (AST_REC (v, e)) x t = AST_REC (v, if v=x then e else (subst e x t)) 
|   subst e _ _ = e

(*  
subst (AST_APP (AST_SUCC,AST_ID "x")) "x" (AST_NUM 1);
val it = AST_APP (AST_SUCC,AST_NUM 1) : term

subst (parsestr "(fn x => succ x) (pred x)") "x" (AST_NUM 3);
val it =
  AST_APP
    (AST_FUN ("x",AST_APP (AST_SUCC,AST_ID "x")),AST_APP (AST_PRED,AST_NUM 3))
  : term
*)

(*  interp : term -> term *)

fun interp (AST_ID _) = AST_ERROR "unbound identifier"
|   interp (AST_REC (x, e)) = interp (subst e x (AST_REC (x, e)))
|   interp (AST_IF (e1, e2, e3)) =
     (case (interp e1) of
          (AST_ERROR s)          => AST_ERROR s
        | (AST_BOOL true)        => (interp e2)
        | (AST_BOOL false)       => (interp e3)
        | (_)                    => AST_ERROR "if condition must be a bool")
|   interp (AST_APP (e1, e2)) =
     (case (interp e1, interp e2) of
          (AST_ERROR s, _)	  => AST_ERROR s
        | (_, AST_ERROR s)        => AST_ERROR s
        | (AST_SUCC, AST_NUM n)   => AST_NUM (n+1)
        | (AST_SUCC, _)           => AST_ERROR "succ needs int argument"
        | (AST_PRED, AST_NUM 0)   => AST_NUM 0
        | (AST_PRED, AST_NUM n)   => AST_NUM (n-1)
        | (AST_PRED, _)           => AST_ERROR "pred needs int argument"
        | (AST_ISZERO, AST_NUM 0) => AST_BOOL true
        | (AST_ISZERO, AST_NUM _) => AST_BOOL false
        | (AST_ISZERO, _)         => AST_ERROR "iszero needs int argument"
        | (AST_FUN (x, e), v)     => interp (subst e x v)
        | (_, _)                  => AST_ERROR "not a functional application")
|   interp e = e

(*  
interp (parsestr "succ (succ 7)");
val it = AST_NUM 9 : term

interp (parsestr "if iszero 0 then succ (succ 7) else 7");
val it = AST_NUM 9 : term

interp (parsestr "pred 1");
val it = AST_NUM 0 : term

interp (parsestr "((fn x => succ x) (succ 0))");
val it = AST_NUM 2 : term

interp (parsefile "twice.pcf");
val it = AST_NUM 65536 : term

interp (parsefile "minus.pcf");
val it = AST_NUM 46 : term

interp (parsefile "factorial.pcf");
val it = AST_NUM 720 : term

interp (parsefile "fibonacci.pcf");
val it = AST_NUM 6765 : term

interp (parsefile "lists.pcf");
val it = AST_BOOL true : term
*)

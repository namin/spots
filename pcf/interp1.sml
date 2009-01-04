use "parser.sml";

(* subst : term -> string -> term -> term *)

(*  interp : term -> term. *)

fun interp (AST_NUM n) = AST_NUM n
|   interp (AST_BOOL b) = AST_BOOL b
|   interp (AST_ERROR s) = AST_ERROR s
|   interp AST_SUCC = AST_SUCC
|   interp AST_PRED = AST_PRED
|   interp AST_ISZERO = AST_ISZERO
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
	| (_, _) => AST_ERROR "other function applications not implemented")
|   interp (AST_ID _) = AST_ERROR "variables not implemented"
|   interp (AST_FUN _) = AST_ERROR "custom functions not implemented"
|   interp (AST_REC _) = AST_ERROR "custom recursive functions not implemented"

(*  
interp (parsestr "succ (succ 7)");
val it = AST_NUM 9 : term

interp (parsestr "if iszero 0 then succ (succ 7) else 7");
val it = AST_NUM 9 : term

interp (parsestr "pred 1");
val it = AST_NUM 0 : term

interp (parsefile "factorial.pcf");

*)

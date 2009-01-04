(*  Here's a skeleton file to help you get started on Interpreter 1.  *)

use "parser.sml";

(*  Here you need to put in the definition of
      subst : term -> string -> term -> term
*)

(*  Here's a partial skeleton of interp : term -> term.
    Also, I've shown you how rule (7) can be implemented.
*)

fun interp (AST_IF (e1, e2, e3)) =
      AST_ERROR "rules (5) and (6) not implemented"
|   interp (AST_APP (e1, e2)) =
     (case (interp e1, interp e2) of
	  (AST_ERROR s, _)	  => AST_ERROR s
	| (_, AST_ERROR s)        => AST_ERROR s
        | (AST_SUCC, AST_NUM n)   => AST_NUM (n+1)
	| (AST_SUCC, _)           => AST_ERROR "succ needs int argument"
	| (_, _) => AST_ERROR "other function applications not implemented")

(*  Once you have defined interp, you can try out simple examples by
      interp (parsestr "succ (succ 7)");
    and you can try out larger examples by
      interp (parsefile "factorial.pcf");
*)


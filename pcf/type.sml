(*  PCF Type Inference *)

use "parser.sml";

datatype typ = VAR of string | INT | BOOL | ARROW of typ * typ

(*  Convert a typ to a string, using as few parentheses as possible.  *)

fun typ2str (VAR a)             = "'" ^ a
|   typ2str INT                 = "int"
|   typ2str BOOL                = "bool"
|   typ2str (ARROW(t1 as ARROW(_, _), t2)) =	(*  the tricky bit  *)
      "(" ^ typ2str t1 ^ ") -> " ^ typ2str t2
|   typ2str (ARROW(t1, t2))     = typ2str t1 ^ " -> " ^ typ2str t2

(*  A substitution is just a function typ -> typ.  *)

fun identity (t : typ) = t

(*  replace (a, t) is the substitution that just replaces a with t.  *)

fun replace (a, t) (VAR b) =
      if a = b then t else VAR b
|   replace (a, t) (ARROW (t1, t2)) =
      ARROW (replace (a, t) t1, replace (a, t) t2)
|   (* No effect on other types. *)
    replace (a, t) t1 = t1

(*  occurs : string * typ -> bool  *)

fun occurs (a, VAR b)         = (a = b)
|   occurs (a, ARROW(t1, t2)) = occurs (a, t1) orelse occurs (a, t2)
|   occurs (a, _)             = false

exception Circularity
exception Mismatch

(*  unify : typ * typ -> (typ -> typ)  *)

fun unify (VAR a, t) =
      if VAR a = t then identity
      else if occurs (a, t) then raise Circularity
      else replace (a, t)
|   unify (t, VAR a)   = unify (VAR a, t)
|   unify (INT, INT)   = identity
|   unify (BOOL, BOOL) = identity
|   unify (ARROW(t1, t2), ARROW(t3, t4)) =
      let val s1 = unify (t1, t3)
          val s2 = unify (s1 t2, s1 t4)
      in
          s2 o s1
      end
|   unify (_, _) = raise Mismatch

(*  An environment is a function string -> typ.
 *  I've included code for the empty environment
 *  and code that produces the updated environment E[x : t].
 *)

exception UnboundID

fun emptyenv (x : string) : typ = raise UnboundID

(*  update : (string -> typ) -> string -> typ -> string -> typ  *)

fun update E (x : string) (ty : typ) y = if x = y then ty else E y

(*  New type variable generator:  newtypevar () and reset ()  *)

local
  val letters = ["a","b","c","d","e","f","g","h","i","j","k","l","m",
                 "n","o","p","q","r","s","t","u","v","w","x","y","z"]
  val cnt = ref 0
  val typevars = ref letters
in
  fun newtypevar () = (
    if null (! typevars) then (
      cnt := ! cnt + 1;
      typevars := map (fn s => s ^ Int.toString (! cnt)) letters
    ) else ();
    VAR (hd (! typevars)) before (typevars := tl (! typevars))
  )

  fun reset () = (
    cnt := 0;
    typevars := letters
  )
end

(*  Milner's algorithm W : (string -> typ) * term -> (typ -> typ) * typ *)

fun W (E, AST_ID x)    = (identity, E x)
|   W (E, AST_NUM _)   = (identity, INT)
|   W (E, AST_BOOL _)  = (identity, BOOL)
|   W (E, AST_SUCC)    = (identity, ARROW (INT,INT))
|   W (E, AST_PRED)    = (identity, ARROW (INT,INT))
|   W (E, AST_ISZERO)  = (identity, ARROW (INT,BOOL))
|   W (E, AST_IF (e1, e2, e3)) =
      let val (s1, t1) = W (E, e1)
          val s2 = unify(t1, BOOL)
          val (s3, t2) = W (s2 o s1 o E, e2)
          val (s4, t3) = W (s3 o s2 o s1 o E, e3)
          val s5 = unify(s4 t2, t3)
      in
        (s5 o s4 o s3 o s2 o s1, s5 t3)
      end
|   W (E, AST_FUN (x, e)) =
      let val t1 = newtypevar()
          val (s, t2) = W (update E x t1, e)
      in
        (s, s (ARROW (t1, t2)))
      end
|   W (E, AST_APP (e1, e2)) =
      let val (s1, t1) = W (E, e2)
          val t2 = newtypevar()
          val (s2, t3) = W (s1 o E, e1)
          val s3 = unify((s2 o s1) t3, ARROW ((s2 o s1) t1, t2))
          val s = s3 o s2 o s1
      in
        (s, s t2)
      end
|   W (E, AST_REC (x, e)) =
      let val t = newtypevar()
          val (s1, te) = W (update E x t, e)
          val s2 = unify(s1 t, s1 te)
          val s = s1 o s2
      in
        (s, s t)
      end
|   W (E, AST_ERROR _) = raise Mismatch (* shouldn't happen *)

(*  Put a type in canonical form, such that the type variables start
 *  from scratch. 
 *)

fun canonical' (E, VAR v) = 
      ((E, (E v)) handle UnboundID => 
        (let val t = newtypevar() 
         in 
           ((update E v t), t) 
         end))
|   canonical' (E, ARROW (t1, t2)) =
      let val (E, t1') = canonical' (E, t1)
          val (E, t2') = canonical' (E, t2)
      in
        (E, ARROW (t1', t2'))
      end
|   canonical' (E, x) = (E, x)

fun canonical t = 
  (reset (); 
  let val (_, t) = canonical' (emptyenv, t)
  in
    t
  end)

(*  Here's a driver program that uses W to find the principal type of e
 *  and then displays it nicely.
 *)

fun infer e =
  let val (s, t) = (reset (); W (emptyenv, e))
  in
    print ("The principal type is\n  " ^ typ2str (canonical t) ^ "\n")
  end

(*
infer (parsestr "fn f => fn g => g (f true) (f 1)");
exception Mismatch

infer (parsestr "fn f => fn x => f (f x)");
('a -> 'a) -> 'a -> 'a

infer (parsestr "fn f => fn g => fn x => f (g x)");
('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

infer (parsestr "fn b => if b then 1 else 0");
bool -> int

infer (parsestr "rec f => fn b => if b then 1 else f true");
bool -> int

infer (parsestr "rec f => fn x => f x");
'a -> 'b

infer (parsestr "rec m => fn x => fn y => if iszero y then x else m (pred x) (pred y)");
int -> int -> int

infer (parsestr "rec even => fn n => if iszero n then true else if iszero (pred n) then false else even (pred (pred n))");
int -> bool

infer (parsefile "twice.pcf");
exception Circularity

infer (parsefile "minus.pcf");
int

infer (parsefile "factorial.pcf");
int

infer (parsefile "fibonacci.pcf");
int

infer (parsefile "lists.pcf");
exception Mismatch
*)

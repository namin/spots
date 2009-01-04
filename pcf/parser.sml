(*  This file defines a recursive-descent parser that builds an abstract
    syntax tree for a PCF expression.  Two versions are provided:
    parsefile f reads the expression from a file named f, while parsestr s
    reads the expression directly from the string s.

    Originally created by Jon Riecke.  Later modified by Jay Sachs,
    Kim Bruce, Barbara Lerner, and Geoffrey Smith.
*)

(*  Increase print depth so abstract syntax trees get displayed completely.  *)
Compiler.Control.Print.printDepth:= 100;

datatype term = AST_ID of string | AST_NUM of int | AST_BOOL of bool
  | AST_FUN of (string * term) | AST_APP of (term * term) 
  | AST_SUCC | AST_PRED | AST_ISZERO
  | AST_IF of (term * term * term) | AST_REC of (string * term)
  | AST_ERROR of string
    
datatype token = ID of string | NUM of int 
  | IFSYM | THENSYM | ELSESYM | TRUESYM | FALSESYM 
  | SUCCSYM | PREDSYM | ISZEROSYM | FNSYM | RECSYM 
  | EQUAL | LPAREN | RPAREN | FNARROW | LETSYM | INSYM | ENDSYM | EOF


signature PCFLEXER =
sig
    val lexfile : string -> token list
    val lexstr : string -> token list
end

structure PCFlexer: PCFLEXER =
struct
  open TextIO

  (*  nexttoken recognizes the various reserved words of PCF, along with
      identifiers, integer literals, and the symbols =, =>, (, and ).
      Also, # begins a comment that extends to the end of the current line.
  *)
  fun nexttoken strm =
    case input1 strm of
        NONE   => EOF
      | SOME c =>
	  if Char.isSpace c then
	    nexttoken strm 
	  else if Char.isAlpha c then
	    let
	      fun getid id =
		case lookahead strm of
		    NONE   => id
		  | SOME d =>
		      if Char.isAlpha d orelse Char.isDigit d then
			(input1 strm; getid (id ^ str d))
		      else
			id
	      val ident = getid (str c)
	    in case ident of
	           "if"     => IFSYM
		 | "then"   => THENSYM
		 | "else"   => ELSESYM
		 | "true"   => TRUESYM
		 | "false"  => FALSESYM
		 | "succ"   => SUCCSYM
		 | "pred"   => PREDSYM
		 | "iszero" => ISZEROSYM
		 | "fn"     => FNSYM
		 | "rec"    => RECSYM
		 | "let"    => LETSYM
		 | "in"     => INSYM
		 | "end"    => ENDSYM
		 | _        => ID ident
	    end
	  else if Char.isDigit c then
	    let
	      fun getnum num =
		case lookahead strm of
		    NONE   => num
		  | SOME d =>
		      if Char.isDigit d then
			(input1 strm; getnum (10*num + ord d - ord #"0"))
		      else
			num
	    in
	      NUM (getnum (ord c - ord #"0"))
	    end
	  else
	    case c of
	        #"=" => (case lookahead strm of
			     SOME #">" => (input1 strm; FNARROW)
			   | _         => EQUAL)
	      | #"(" => LPAREN
	      | #")" => RPAREN
	      | #"#" =>
		  (*  A # starts a comment, so we eat the current line.  *)
		  let fun eatline () =
			case input1 strm of
			    NONE       => EOF
			  | SOME #"\n" => nexttoken strm
			  | SOME _     => eatline ()
		  in
		    eatline ()
		  end
	      | _    => (print ("Skipping illegal character " ^ str c ^ ".\n");
			 nexttoken strm)

  fun gettokens strm =
    let
      fun gettokens_aux toks =
        let val tok = nexttoken strm
        in
	  if tok = EOF then
	    (closeIn strm; rev (EOF::toks))
          else
	    gettokens_aux (tok::toks)
        end
    in
      gettokens_aux []
    end

  fun lexstr str = gettokens (openString str)

  fun lexfile file = gettokens (openIn file)
  (*  Note that we could instead have called
        explode (input (openIn file))
      to get a list of all the characters in the file.
  *)

end


signature PCFPARSER =
sig
  val parse : token list -> term
end

structure PCFparser : PCFPARSER =
struct
  fun error msg = print (msg ^ "\n")

  (*  Exp ::= x | n | true | false | succ | pred | iszero |
	      if Exps then Exps else Exps | fn x => Exps |
	      rec x => Exps | ( Exps ) | let x = Exps in Exps end
  *)
  fun parseExp (ID v::tail)	 = (AST_ID v, tail)
    | parseExp (NUM n::tail)	 = (AST_NUM n, tail)
    | parseExp (TRUESYM::tail)	 = (AST_BOOL true, tail)
    | parseExp (FALSESYM::tail)	 = (AST_BOOL false, tail)
    | parseExp (SUCCSYM::tail)	 = (AST_SUCC, tail)
    | parseExp (PREDSYM::tail)	 = (AST_PRED, tail)
    | parseExp (ISZEROSYM::tail) = (AST_ISZERO, tail)
    | parseExp (IFSYM::tail)     =
	let val (ast1, rest1) = parseExps tail
	in
	  if hd rest1 = THENSYM then
	    let val (ast2, rest2) = parseExps (tl rest1)
	    in
	      if hd rest2 = ELSESYM then
		let val (ast3, rest3) = parseExps (tl rest2)
		in
		  (AST_IF(ast1, ast2, ast3), rest3)
		end
	      else
		(error "Missing else"; (AST_ERROR "Missing else", [EOF]))
	    end
	  else
	    (error "Missing then"; (AST_ERROR "Missing then", [EOF]))
	end
    | parseExp (FNSYM::ID v::FNARROW::tail) =
        let val (ast, rest) = parseExps tail
	in
	  (AST_FUN(v, ast), rest)
	end
    | parseExp (FNSYM::ID v::tail) =
	(error ("Missing => after fn " ^ v);
	 (AST_ERROR ("Missing => after fn " ^ v), [EOF]))
    | parseExp (FNSYM::tail) =
        (error "Missing identifier after fn"; 
	 (AST_ERROR "Missing identifier after fn", [EOF]))
    | parseExp (RECSYM::ID v::FNARROW::tail) =
        let val (ast, rest) = parseExps tail
        in
	  (AST_REC(v, ast), rest)
	end
    | parseExp (RECSYM::ID v::tail) =
	(error ("Missing => after rec " ^ v);
	 (AST_ERROR ("Missing => after rec " ^ v), [EOF]))
    | parseExp (RECSYM::tail) =
        (error "Missing identifier after rec"; 
	 (AST_ERROR "Missing identifier after rec", [EOF]))
    | parseExp (LPAREN::tail) =
        let val (ast, rest) = parseExps tail
	in
	  if hd rest = RPAREN then
	    (ast, tl rest)
	  else
	    (error "Missing )"; (AST_ERROR "Missing )", [EOF]))
        end
    | parseExp (LETSYM::ID v::EQUAL::tail) =
	let val (ast1, rest1) = parseExps tail
	in
	  if hd rest1 = INSYM then
	    let val (ast2, rest2) = parseExps (tl rest1)
	    in
	      if hd rest2 = ENDSYM then
	        (* Note that "let x = e1 in e2 end" is
		   syntactic sugar for "(fn x => e2) e1" *)
	        (AST_APP (AST_FUN(v, ast2), ast1), tl rest2)
	      else
		(error "Missing end"; (AST_ERROR "Missing end", [EOF]))
	    end
	  else
	    (error "Missing in"; (AST_ERROR "Missing in", [EOF]))
	end
    | parseExp (LETSYM::ID v::tail) =
	(error "Missing ="; (AST_ERROR "Missing =", [EOF]))
    | parseExp (LETSYM::tail) =
        (error "Missing identifier after let";
	 (AST_ERROR "Missing identifier after let", [EOF]))
    | parseExp [EOF] = 
        (error "Unexpected EOF"; (AST_ERROR "Unexpected EOF", [EOF]))
    | parseExp _ = 
        (error "Bad expression"; (AST_ERROR "Bad expression", [EOF]))

  (*  Exps ::= Exps Exp | Exp
      We resolve the ambiguity in the grammar by assuming that concatenation
      (i.e. function application) binds tighter than if, fn, and rec.
  *)
  and parseExps tokens =
    let
      val (ast, rest) = parseExp tokens

      (*  startsExp tok tests whether tok is in FIRST(Exp).  *)
      fun startsExp (ID s)  = true
	| startsExp (NUM n) = true
	| startsExp tok     =
	    tok = TRUESYM orelse tok = FALSESYM orelse tok = SUCCSYM orelse
	    tok = PREDSYM orelse tok = ISZEROSYM orelse tok = IFSYM orelse
	    tok = FNSYM orelse tok = RECSYM orelse tok = LPAREN orelse
	    tok = LETSYM

      fun parseExps_aux ast rest =
	if startsExp (hd rest) then
	  let 
	    val (ast', rest') = parseExp rest
	  in
	    parseExps_aux (AST_APP (ast, ast')) rest'
	  end
	else
	  (ast, rest)
    in
      parseExps_aux ast rest
    end

  fun parse tokens =
    let val (ast, rest) = parseExps tokens
    in
      if hd rest = EOF then
	ast
      else
	(error "EOF expected"; AST_ERROR "EOF expected")
    end
end

(*  The final definitions of parsefile and parsestr put the pieces together.  *)

fun parsefile file = PCFparser.parse (PCFlexer.lexfile file)

fun parsestr str = PCFparser.parse (PCFlexer.lexstr str)


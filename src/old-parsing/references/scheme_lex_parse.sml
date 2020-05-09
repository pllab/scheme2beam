
(* This file converts a file containing a PCF term into an abstract *)
(* syntax tree suitable for interpreting.  The function "parsefile" *)
(* takes a file name as input and returns an abstract syntax tree.  *)
(*                                                                  *)
(* Created by Jon Riecke for Programming Paradigms Short Course.    *)
(* 3/7/98: Modified by Jay Sachs for SML 110. Added parsestr        *)
(* Feb 9, 200: Modified by M. Arthur Munson.  Added let expressions.*)
(* March 28, 2001: Major modifications by Barbara Lerner.
      Changed lexing so that the entire file is read in at once
      into a string so that stream manipulation only occurs in lex
      rather than begin distributed throughout the lexer.

      Changing parsing to be a recursive descent parser so it is
      easier to understand and update.

      Added better error messages and some error recovery to the
      parser.

      Added comments!                                               *)

(* Allows more print depth.                                         *)
Control.Print.printDepth:= 100;
Control.Print.printLength:= 200;

datatype term = AST_ID of string | AST_NUM of int | AST_BOOL of bool
  | AST_FUN of (string * term) | AST_APP of (term * term) 
  | AST_SUCC | AST_PRED | AST_ISZERO
  | AST_IF of (term * term * term) | AST_REC of (string * term)
  | AST_ERROR of string
    
datatype token = ID of string | NUM of int 
  | IFSYM | THENSYM | ELSESYM | TRUESYM | FALSESYM 
  | SUCCSYM | PREDSYM | ISZEROSYM | FNSYM | RECSYM 
  | EQUAL | LPAREN | RPAREN | FNARROW | LETSYM | INSYM | ENDSYM
  | EOF



signature PCFLEXER =
sig
    val lex : string -> token list
    val lexstr : string -> token list
end


structure PCFlexer: PCFLEXER =
struct
    open TextIO;

    (* Return true if c is a letter or digit *)
    fun alphanum c = (Char.isAlpha c) orelse (Char.isDigit c)

    (* Extracts consecutive alphanumeric characters from the input to
       build up an identifier.  Returns a tuple containing the next
       identifier in the input and the input left over after removing
       the identifier.
       Precondition:  The initial character of the identifier has
         already been found and is passed in in the second parameter.
       Parameter 1:  Input to extract the identifier from
       Parameter 2:  The characters found so far in the identifier.
    *)
    fun getid nil id = (id, nil)
      | getid (s as c::rest) id = 
                if (alphanum c) then getid rest (id ^ (str(c)))
                else (id, s)

    (* Extracts consecutive digits from the input to
       build up an integer.  Returns a tuple containing the next
       integer in the input and the input left over after removing
       the integer.
       Precondition:  The initial digit of the integer has
         already been found and is passed in in the second parameter.
       Parameter 1:  Input to extract the integer from
       Parameter 2:  The digits found so far in the integer.
    *)
    fun getnum nil num = (num, nil)
      | getnum (s as c::rest) num =
                if (Char.isDigit c) then 
                    getnum rest (num*10 + ((ord c)-ord #"0"))
                else (num, s)
                    
    (* Return the list of tokens found in the input.
       Parameter:  A character list to tokenize
    *)
    fun gettokens nil = [EOF]
      | gettokens (#"="::(#">"::rest)) = FNARROW::gettokens rest
      | gettokens (#"="::rest) = EQUAL::gettokens rest
      | gettokens (#"("::rest) = LPAREN::gettokens rest
      | gettokens (#")"::rest) = RPAREN::gettokens rest
      | gettokens (c::rest) =
          if Char.isSpace c then
	    (* Recurse to skip white space *)
	    gettokens rest

	  else if Char.isAlpha c then
	    (* Return keyword or identifier *)
	      let val (id, remainder) = (getid rest (str c))
	      fun idSymbol "fn" = FNSYM
                | idSymbol "if" = IFSYM
		| idSymbol "then" = THENSYM
		| idSymbol "else" = ELSESYM
		| idSymbol "true" = TRUESYM
		| idSymbol "false" = FALSESYM
		| idSymbol "succ" = SUCCSYM
		| idSymbol "pred" = PREDSYM
		| idSymbol "iszero" = ISZEROSYM
		| idSymbol "rec" = RECSYM
		| idSymbol "let" = LETSYM
		| idSymbol "in" = INSYM
		| idSymbol "end" = ENDSYM

		  (* Not a keyword.  This must be an identifier. *)
		| idSymbol id = ID id
	    in
	      (idSymbol id)::gettokens remainder
            end

          else if (Char.isDigit c) then
	    (* Return number *)
            let
	      val (num, remainder) = getnum rest ((ord c) - (ord #"0"))
	    in
	      (NUM (num))::gettokens remainder
	    end

          else
            (print ("Skipping illegal character "^(str c) ^"."); 
             gettokens rest)

    (* Returns the list of tokens found in a string. *)
    fun lexstr s = gettokens (explode s)
                
    (* Returns the list of tokens found in a file.
       Parameter:  filename *)
    fun lex file = 
        let
	    val strm = openIn file
            val filecontents = explode (input strm)
        in 
            (closeIn strm; gettokens filecontents)
        end
end;



(*  Now define the parsing part of the program, which takes a list of  *)
(*  tokens and returns an abstract syntax tree.                        *)

signature PCFPARSER =
sig
    val parse : token list -> term
end

structure PCFparser : PCFPARSER =
struct
    (* Output an error message *)
    fun error (msg:string) = print msg

    (* Parses an expression.  If an expression is found, returns a
       tuple containing the ast for the expression and the input
       following the expression.  If an expression is not found, it
       returns an error and consumes all remaining input.
    *)
       (* First convert the terminals into ASTs *)
    fun parseExpression ((ID v)::tl) = ((AST_ID v), tl)
      | parseExpression ((NUM n)::tl)  = ((AST_NUM n), tl)
      | parseExpression (TRUESYM::tl)  = ((AST_BOOL true), tl)
      | parseExpression (FALSESYM::tl) = ((AST_BOOL false), tl)
      | parseExpression (SUCCSYM::tl)  = (AST_SUCC, tl)
      | parseExpression (PREDSYM::tl)  = (AST_PRED, tl)
      | parseExpression (ISZEROSYM::tl)= (AST_ISZERO, tl)

        (* Parse nonterminals. *)
        (* fn ID => <expressions> *)
      | parseExpression (FNSYM::(ID v)::FNARROW::tail) =
          let
	    val (body, rest) = parseExpressions (tail)
	  in
	    (AST_FUN (v,body), rest)
	  end

      | parseExpression (FNSYM::(ID v)::tl) =
          let
	    val (body, rest) = parseExpressions (tl)
	  in
	    (error ("=> expected after fn " ^ v ^ ".\n");
	     (AST_FUN (v, body), rest))
	  end 

      | parseExpression (FNSYM::tl) =
          (error "Identifier expected after fn.\n";
	   (AST_ERROR "Identifier expected after fn.", nil))

	(* rec ID => <expressions> *)
      | parseExpression (RECSYM::(ID v)::FNARROW::tl) =
          let
	    val (func, rest) = parseExpressions (tl)
          in
	    (AST_REC (v, func), rest)
	  end

      | parseExpression (RECSYM::(ID v)::tl) =
          let
	    val (func, rest) = parseExpressions (tl)
	  in
	    (error ("=> expected after rec " ^ v ^ ".\n");
	     (AST_REC (v, func), rest))
	  end

      | parseExpression (RECSYM::tl) =
          (error "Identifier expected after rec.\n";
	   (AST_ERROR "Identifier expected after rec.", nil))

	(* ( <expressions> ) *)
      | parseExpression (LPAREN::tail)   =
          let
	    val (body, rest) = parseExpressions tail
	    fun parseRParen body (RPAREN::rest) = (body, rest)
              | parseRParen body rest =
	          (error "Missing right paren\n";
		   (body, rest))
	  in
	    parseRParen body rest
	  end

	(* if <expressions> then <expressions> else <expressions> *)
      | parseExpression (IFSYM::tail) =
          let
	    val (expr, rest) = parseExpressions tail
	    fun parseThen expr (THENSYM::rest) = 
	          let
	            val (trueexp, rest) = parseExpressions (rest)
		    fun parseElse expr thenpart (ELSESYM::rest) =
		          let
		            val (elseexp, rest) = parseExpressions rest
                          in
		            (AST_IF (expr, thenpart, elseexp), rest)
                          end
		      | parseElse expr thenpart _ =
		          (error "Missing ELSE \n";
		           (AST_ERROR "Missing else", nil))
                  in
		    parseElse expr trueexp rest
                  end
	      | parseThen expr rest =
	          (error "Missing THEN\n";
	           (AST_ERROR "Missing then", nil))
	  in
	    parseThen expr rest
	  end

	(* let ID = <expressions> in <expressions end *)
      | parseExpression (LETSYM::(ID var)::EQUAL::tail) =
          parseLetBody var tail ""

      | parseExpression (LETSYM::(ID var)::tail) =
          parseLetBody var tail ("Missing = after let " ^ var ^ ".\n")

      | parseExpression (LETSYM::tail) =
          (error "Identifier expected after let.\n";
	   (AST_ERROR "Identifier expected after let.", nil))

      | parseExpression [EOF] = 
          (error "Unexpected end of input\n";
           (AST_ERROR "Unexpected end of input", nil))

      | parseExpression _ = 
          (error "Fatal error\n";
           (AST_ERROR "Fatal error", nil))


       (* Return a tuple containing an AST_APP for a let expression
          and the input left over after the let expression.  Returns
          an AST_ERROR in the tuple if it cannot parse the let
          expression.  Assumes that "let ID =" has already been
          consumed from the input.
	  Parameter 1:  the variable being bound by the let expression
	  Parameter 2:  the tokens following the = in the let
		        expression
	  Parameter 3:  error strings describing errors found thus far
		        in the let expression.
       *)
  and parseLetBody var (INSYM::rest) err =
	(error (err ^ "Missing expression after let " ^ var ^ " = \n");
	 (AST_ERROR ("Missing expression after let " ^ var ^ " = \n"), nil))
    | parseLetBody var tokens err =
	  let
	    val (value, rest) = parseExpressions tokens
	    fun parseIn var value (INSYM::ENDSYM::rest) err =
	          (error (err ^ "Missing expression after let "^ var ^ " = \n");
	           (AST_ERROR ("Missing expression after let "^ var ^ " = \n"), nil))
	      | parseIn var value (INSYM::rest) err =
	          let
	            val (body, rest) = parseExpressions rest
		    fun parseEnd var value body (ENDSYM::rest) err =
		          (error err;
		           (AST_APP (AST_FUN (var, body), value),
			    rest))
                      | parseEnd var value body rest err =
		          (error (err ^ "Missing END \n");
		           (AST_APP (AST_FUN (var, body), value), nil))
	          in
		    parseEnd var value body rest err
	          end
	      | parseIn var value rest err =
	          (error (err ^ "Missing IN\n");
	           (AST_ERROR "Missing IN", nil))
	  in
	    parseIn var value rest err
	  end

        (* Return a tuple containing an ast for a single expression,
           or if a pair of expressions are found, return an AST_APP
           using those expressions as subexpressions.  The second
           element in the tuple is the input left over after parsing
           the expressions.
	   Parameter:  list of tokens to parse
	*)
  and parseExpressions tokens =
      let
        val (ast1, rest) = parseExpression tokens
      in
        (* Reached end of input.  Return expression found *)
        if rest = nil orelse rest = [EOF] then
	  (ast1, nil)
	else
	  let
	    (* Builds up nested AST_APPs left recursively.  Returns a
                 tuple containing the parsed ast and the tokens
		 following the parsed ast
	       Parameter 1:  the tokens remaining to parse
	       Parameter 2:  the previous expression parsed
	    *)
	    fun parseExpression2 nil ast1 = (ast1, nil)
	      | parseExpression2 [EOF] ast1 = (ast1, nil)
	      | parseExpression2 tokens ast1 =
	      let
        	val next = hd tokens
	      in
	        (* Found a token that cannot begin an expression.
                   Return the expression found.  Do not consume the
		   token. *) 
                if next = RPAREN orelse next = FNARROW orelse
		   next = THENSYM orelse next = ELSESYM orelse
		   next = INSYM orelse next = ENDSYM then
	          (ast1, tokens)
		else
		  let
		    val (ast2, rest) = parseExpression tokens
		  in
		    parseExpression2 rest (AST_APP (ast1, ast2))
		  end
	      end
	      
	  in
	    parseExpression2 rest ast1
	  end 
      end 

    (* Return an AST for the list of tokens passed in. *)
  fun parse tokens =
      let 
        val (ast1, rest) = parseExpressions tokens
      in
        if rest = [EOF] orelse rest = nil then
          ast1
        else
          (error "Fatal error -- more input than expected.\n";
           AST_ERROR "More input than expected.")
      end
end;


(*  #CheckMatch Debug := true; -- put back in if run on Mac *)

(* The final definition of the function puts the pieces together.   *)

fun parsefile str = PCFparser.parse (PCFlexer.lex str);

fun parsestr str = PCFparser.parse (PCFlexer.lexstr str)




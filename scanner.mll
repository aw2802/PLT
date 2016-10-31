{ open Parser }

let whitespace = [' ' '\t' '\r' '\n']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let int = digit+ 
let float = (digit+) '.' (digit+)
(* @TODO: if all we have are numbers (floats) what to do with ints? *)

let id = alpha (alpha | digit | '_')*
let char = ''' ( ascii) '''
let string = '"' ( (ascii)* as s) '"'


rule token = parse 
	whitespace { token lexbuf }
	| "/*"	{ comment lexbuf } 
	| "//"    { singleComment lexbuf }

	(* Operators and Separators *)
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | '['      { LBRACKET }
  | ']'      { RBRACKET }	
  | '.'			 { DOT 			}
  | ';'      { SEMI }
  | ','      { COMMA }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }
  | '='      { ASSIGN }
  | "=="     { EQ }
  | "!="     { NEQ }
  | '<'      { LT }
  | "<="     { LEQ }
  | ">"      { GT }
  | ">="     { GEQ }
  | "and"    { AND }
  | "or"     { OR }
  | "!"      { NOT }

	(* Branching Control *)
  | "if"     		{ IF }
  | "else"   		{ ELSE }
  | "elseif"		{ ELSEIF }
  | "for"    		{ FOR }
  | "while"  		{ WHILE }
  | "return" 	 	{ RETURN }
  | "break"  	 	{ BREAK }
  | "continue" 	{ CONTINUE }

  (* Data and Return Types *)
  | "number"    { FLOAT }
  | "char"      { CHAR }
  | "void"      { VOID }
  | "boolean"   { BOOLEAN }
  | "true"      { TRUE }
  | "false"     { FALSE }

  (* Classes *)
  | "class"     { CLASS 	}
  | "new" 	   	{ NEW 		}
  | "public"		{ PUBLIC 	}
  | "private"		{ PRIVATE }
  
  | int as lxm          { INT_LITERAL(int_of_string lxm) }
  | float as lxm        { FLOAT_LITERAL(float_of_string lxm) }
  | char as lxm         { CHAR_LITERAL(String.get lxm 1) }
  | string       				{ STRING_LITERAL(unescape s) }
  | id as lxm           { ID(lxm) }
  | eof                 { EOF }
  
  | _ as illegal  { raise (Failure("illegal character " ^ Char.escaped illegal )) }

and comment = parse
	"*/" { token lexbuf }
| _		{ comment lexbuf }

and singleComment = parse
  '\n' { token lexbuf }
| _    { singleComment lexbuf }
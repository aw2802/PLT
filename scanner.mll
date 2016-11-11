{ open Parser }

let whitespace = [' ' '\t' '\r' '\n']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let int = digit+ 
let float = (digit+) '.' (digit+)

let id = (alpha | '_') (alpha | digit | '_')*
let char = ''' (ascii) '''

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
  | '.'	     { DOT }
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
  | "if"     	{ IF }
  | "else"   	{ ELSE }
  | "elseif"	{ ELSEIF }
  | "for"    	{ FOR }
  | "while"  	{ WHILE }
  | "return" 	{ RETURN }
  | "break"  	{ BREAK }
  | "continue" 	{ CONTINUE }

  (* Primitive Data Types and Return Types *)
  | "char"      { JCHAR }
  | "int"       { JINT }
  | "float"     { JFLOAT }
  | "void"      { JVOID }
  | "boolean"   { JBOOLEAN }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "null"      { NULL }

  (* Classes *)
  | "class"   { CLASS }
  | "new" 	  { NEW 	}
  | "public"	{ PUBLIC }
  | "private"	{ PRIVATE }
  
  | int as lxm    { INT_LITERAL(int_of_string lxm) }
  | float as lxm  { FLOAT_LITERAL(float_of_string lxm) }
  | char as lxm   { CHAR_LITERAL(String.get lxm 1) }
  | '"'( ('\\'('/'|'\\'| 'b' | 'f' | 'n' | 'r' | 't'))|([^'"']) )*'"' as lxm { STRING_LITERAL(lxm) }
  | id as lxm      { ID(lxm) }
  | eof           { EOF }
  | _ as illegal  { raise (Failure("illegal character " ^ Char.escaped illegal )) }

and comment = parse
	"*/" 	{ token lexbuf }
| _		{ comment lexbuf }

and singleComment = parse
  '\n' { token lexbuf }
| _    { singleComment lexbuf }

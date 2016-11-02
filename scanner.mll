{ open Parser }

let whitespace = [' ' '\t' '\r' '\n']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let inty = digit+ 
let floaty = (digit+) '.' (digit+)
(* @TODO: if all we have are numbers (floats) what to do with ints? *)

let id = (alpha | '_') (alpha | digit | '_')*
let chary = ''' (ascii) '''

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
  | "char"      { CHAR }
  | "number"    { FLOAT }
  | "void"      { VOID }
  | "boolean"   { BOOLEAN }
  | "true"      { TRUE }
  | "false"     { FALSE }

  (* Classes *)
  | "class"     { CLASS }
  | "new" 	{ NEW 	}
  | "public"	{ PUBLIC }
  | "private"	{ PRIVATE }
  
  | inty as lxm    { INT_LITERAL(int_of_string lxm) }
  | floaty as lxm  { FLOAT_LITERAL(float_of_string lxm) }
  | chary as lxm   { CHAR_LITERAL(String.get lxm 1) }
  | '"'            { read_string (Buffer.create 17) lexbuf }  
  | id as lxm     { ID(lxm) }
  | eof           { EOF }
  | _ as illegal  { raise (Failure("illegal character " ^ Char.escaped illegal )) }

and comment = parse
	"*/" 	{ token lexbuf }
| _		{ comment lexbuf }

and singleComment = parse
  '\n' { token lexbuf }
| _    { singleComment lexbuf }

(* from https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html, to be modified *)
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

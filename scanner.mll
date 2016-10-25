{ open Parser }

rule token = parse
	[' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"		{ comment lexbuf }
| '('		{ LPAREN }	| '='		{ ASSIGN }	| "if"		{ IF }
| ')'		{ RPAREN }	| "=="		{ EQ }		| "else"	{ ELSE }
| '{'		{ LBRACE }	| "!="		{ NEQ }		| "elseif"	{ ELSEIF }
| '}'		{ RBRACE }	| '<'		{ LT }		| "while"	{ WHILE }
| ';'		{ SEMI }	| "<="		{ LEQ }		| "return"	{ RETURN }
| ','		{ COMMA }	| '>'		{ GT }		| "number"	{ NUMBER }
| '+'		{ PLUS }	| ">="		{ GEQ }		| "boolean"	{ BOOLEAN }
| '-'		{ MINUS }	| "and"		{ AND }		| "true"	{ TRUE }	
| '*'		{ TIMES }	| "or"		{ OR }		| "false"	{ FALSE }
| '/'		{ DIVIDE }	| '!'		{ NOT }		| "char"	{ CHAR }
| "break"	{ BREAK }	| "continue"{ CONTINUE }| "for"		{ FOR }
| "public"	{ PUBLIC }	| "private" { PRIVATE }	| "new"		{ NEW }	
| "void"	{ VOID }
| ['0'-'9']+ as lxm { LITERAL(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
	"*/" { token lexbuf }
| _		{ comment lexbuf }
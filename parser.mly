%{ open Ast %}

%token CLASS PUBLIC PRIVATE
%token NUMBER JBOOLEAN JCHAR JVOID TRUE FALSE
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA DOT
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT AND OR 
%token EQ NEQ LT LEQ GT GEQ 
%token RETURN IF ELSEIF ELSE FOR WHILE NEW BREAK CONTINUE

%token <int> INT_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <char> CHAR_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSEIF
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT 
%right RBRACKET
%left LBRACKET
%right DOT 

%start program
%type <Ast.program> program

%%

program:
	cdecls EOF { $1 } 

cdecls: cdecl_list { List.rev $1 } /* REVERSE ???! */

cdecl_list: 
	  cdecl 	{ [$1] } /* BRACES WTF  */
	| cdecl_list cdecl { $2::$1 } /* WE DON'T GET LISTS */

cdecl:
		PUBLIC CLASS ID LBRACE cbody RBRACE { {
			cname = $3;
			cbody = $5
		} }
	| 	PRIVATE CLASS ID LBRACE cbody RBRACE { {
			cname = $3;
			cbody = $5
		} }

cbody: /* make sure defined in ast, rename to variables */
	/* nothing */ { { 
		variables = [];
		constructors = [];
		methods = [];
	} }
	| 	cbody variable { { 
			variables = $2 :: $1.variables;
			constructors = $1.constructors;
			methods = $1.methods;
		} }
 	| 	cbody constructor { { 
			variables = $1.variables;
			constructors = $2 :: $1.constructors;
			methods = $1.methods;
		} }
 	| 	cbody fdecl { { 
			variables = $1.variables;
			constructors = $1.constructors;
			methods = $2 :: $1.methods;
		} }

/* variables */
variable:
	typ ID { { 
		vtype = $1;
		vname = $2;
	} }

/* constructors */

/* methods */





fdecl:
	typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
		{ { freturn = $1; 
			fname = $2; 
			fformals = $4;
			fbody = List.rev $8 } }

formals_opt: /* nothing */ { [] }
	| formal_list { List.rev $1 }

formal_list: 
	  typ ID { 
	  	[ { vtype = $1;
	  		vname = $2; } ] }
	| formal_list COMMA typ ID 
		{ { vtype = $3;
			vname = $4;
			} 
			:: $1 
		}

typ:  JCHAR { JChar }
	| JBOOLEAN { JBoolean }
	| JVOID { JVoid }

vdecl_list: /* nothing */ { [] }
	| vdecl_list vdecl { $2 :: $1 }

vdecl: typ ID SEMI { VarDecl({ 	
		vtype = $1;
		vname = $2; }) }


stmt_list:
	/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
	  expr SEMI { Expr $1 }
	/*| vdecl { VarDecl($1) } */
	| RETURN SEMI { Return Noexpr }
	| RETURN expr SEMI { Return $2 }
	| LBRACE stmt_list RBRACE { Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
	| WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr:
	  INT_LITERAL { Literal($1) }
	| TRUE { BoolLit(true) }
	| FALSE { BoolLit(false) }
	| ID { Id($1) }
	| expr PLUS expr { Binop($1, Add, $3) }
	| expr MINUS expr { Binop($1, Sub, $3) }
	| expr TIMES expr { Binop($1, Mult, $3) }
	| expr DIVIDE expr { Binop($1, Div, $3) }
	| expr EQ expr { Binop($1, Equal, $3) }
	| expr NEQ expr { Binop($1, Neq, $3) }
	| expr LT expr { Binop($1, Less, $3) }
	| expr LEQ expr { Binop($1, Leq, $3) }
	| expr GT expr { Binop($1, Greater, $3) }
	| expr GEQ expr { Binop($1, Geq, $3) }
	| expr AND expr { Binop($1, And, $3) }
	| expr OR expr { Binop($1, Or, $3) }
	| NOT expr { Unop(Not, $2) }
	| ID ASSIGN expr { Assign($1, $3) }
	| LPAREN expr RPAREN { $2 }
	| ID LPAREN actuals_opt RPAREN { Call($1, $3) }	


expr_opt:
	/* nothing */ { Noexpr }
	| expr { $1 }

actuals_opt:
	/* nothing */ { [] }
	| actuals_list { List.rev $1 }

actuals_list:
	expr { [$1] }
	| actuals_list COMMA expr { $3 :: $1 }
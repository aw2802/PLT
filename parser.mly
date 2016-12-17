%{ open Ast %}

%token CLASS PUBLIC PRIVATE
%token JBOOLEAN JCHAR JINT JFLOAT JVOID JSTRING TRUE FALSE NULL TUPLE
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
	cdecls EOF { Program($1) } 

cdecls: cdecl_list { List.rev $1 } 

cdecl_list: 
	  cdecl 	{ [$1] } 
	| cdecl_list cdecl { $2::$1 } 

scope:
	  PRIVATE { Private }
	| PUBLIC  { Public }

cdecl:
		scope CLASS ID LBRACE cbody RBRACE { {
			cscope = $1;
			cname = $3;
			cbody = $5
		} }
		

cbody: /* make sure defined in ast, rename to variables */
	/* nothing */ { { 
		variables = [];
		constructors = [];
		methods = [];
	} }
	| 	cbody vdecl { { 
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

vdecl:
	scope datatype ID SEMI{{ 
		vscope = $1;
		vtype = $2;
		vname = $3;
		vexpr = Noexpr;
	}}
	| scope datatype ID ASSIGN expr SEMI {{
		vscope = $1; 
		vtype = $2; 
		vname = $3; 
		vexpr = $5
	}}



/* constructors */
constructor:    
	ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE {
		{
			fscope = Public;
			fname = $1;
			freturn = JVoid; 
			fformals = $3;
			fbody = List.rev $6;
		        
		}
	}


/* methods */
fdecl:
	scope datatype ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
		{ { fscope = $1;
			freturn = $2; 
			fname = $3; 
			fformals = $5;
			fbody = List.rev $8 } }

tdatatype_args:
                datatype                        {[$1]}
        |       tdatatype_args COMMA datatype   {$3 :: $1}

tuple_type :
        TUPLE LT tdatatype_args GT { Tuple($3) }

/* datatypes + formal & actual params */
primitive:  
	  JCHAR 					{ JChar }
	| JINT						{ JInt }
	| JFLOAT				 	{ JFloat } 
	| JBOOLEAN 					{ JBoolean }
	| JVOID 					{ JVoid }
	| ID						{ Object($1) }
		
array_type:
	primitive LBRACKET brackets RBRACKET { Arraytype($1, $3) }

datatype:
	  primitive   { $1 }
	| array_type { $1 }
	| tuple_type { $1 }

brackets:
	  /* nothing */	{ 1 }
	| brackets RBRACKET LBRACKET { $1 + 1 }

formal:
	datatype ID
	 {{
		fvtype = $1;
	  	fvname = $2; 
	  	}}

formals_opt: /* nothing */ { [] }
	| formal_list { List.rev $1 }

formal_list: 
	  formal { [$1] }
	| formal_list COMMA formal { $3 :: $1 }

actuals_opt:
		/* nothing */ { [] }
	| 	actuals_list  { List.rev $1 }

actuals_list:
		expr                    { [$1] }
	| 	actuals_list COMMA expr { $3 :: $1 }

/* statements */

stmt_list:
	/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
	  expr SEMI { Expr ($1) }
	| vdecl { VarDecl ($1) }
	| datatype ID SEMI {LocalVarDecl($1, $2, Noexpr)}
	| datatype ID ASSIGN expr SEMI {LocalVarDecl($1, $2, $4)}
	| RETURN SEMI { Return Noexpr }
	| RETURN expr SEMI { Return $2 }
	| LBRACE stmt_list RBRACE { Block(List.rev $2) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
	| IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9) }
	| WHILE LPAREN expr RPAREN stmt { While($3, $5) }

/* expressions */

expr:
	  literals	{ $1 }
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
	| MINUS expr { Unop(Sub, $2) }
	| ID ASSIGN expr { Assign($1, $3) }
	| LPAREN expr RPAREN { $2 }
	| ID LPAREN actuals_opt RPAREN { FuncCall($1, $3) }	
	| NEW ID LPAREN actuals_opt RPAREN { CreateObject($2, $4)} 	  
	| expr DOT expr { ObjAccess($1, $3)}
	| NEW TUPLE LT tdatatype_args GT LPAREN actuals_opt RPAREN  { TupleCreate($4, $7) } 
	| expr LBRACKET expr RBRACKET {TupleAccess($1, $3)}
	| NEW primitive brackets_args RBRACKET { ArrayCreate($2, List.rev $3) }
	| expr brackets_args RBRACKET { ArrayAccess($1, List.rev $2) }

brackets_args:
	  LBRACKET expr { [$2] }
	| brackets_args RBRACKET LBRACKET expr { $4 :: $1 }

expr_opt:
	/* nothing */ { Noexpr }
	| expr { $1 }

literals:
	  INT_LITERAL      		{ Int_Lit($1) }
	| FLOAT_LITERAL    		{ Float_Lit($1) }
	| TRUE			   	{ Bool_Lit(true) }
	| FALSE			   	{ Bool_Lit(false) }
	| STRING_LITERAL   		{ String_Lit($1) }  
	| CHAR_LITERAL			{ Char_Lit($1) }
	| ID 			   	{ Id($1) }	
	| NULL				{ Null }

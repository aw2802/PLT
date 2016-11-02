%{ open Ast %}

%token CLASS PUBLIC PRIVATE
%token NUMBER BOOLEAN CHAR VOID TRUE FALSE
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
  decls EOF { $1 }
  
decls:
   /* nothing */ { [], [] }

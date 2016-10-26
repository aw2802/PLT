%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE ELSEIF FOR WHILE NUMBER BOOLEAN CHAR
%token VOID BREAK PUBLIC PRIVATE NEW CONTINUE

%token <int> LITERAL
%token <string> ID
%token EOF

%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT 

%start program
%type <Ast.program> program

%%


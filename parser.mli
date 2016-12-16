type token =
  | CLASS
  | PUBLIC
  | PRIVATE
  | JBOOLEAN
  | JCHAR
  | JINT
  | JFLOAT
  | JVOID
  | JSTRING
  | TRUE
  | FALSE
  | NULL
  | JTUPLE
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | DOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | AND
  | OR
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RETURN
  | IF
  | ELSEIF
  | ELSE
  | FOR
  | WHILE
  | NEW
  | BREAK
  | CONTINUE
  | INT_LITERAL of (int)
  | FLOAT_LITERAL of (float)
  | STRING_LITERAL of (string)
  | CHAR_LITERAL of (char)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program

type token =
  | CLASS
  | PUBLIC
  | PRIVATE
  | NUMBER
  | JBOOLEAN
  | JCHAR
  | JVOID
  | TRUE
  | FALSE
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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 55 "parser.ml"
let yytransl_const = [|
  257 (* CLASS *);
  258 (* PUBLIC *);
  259 (* PRIVATE *);
  260 (* NUMBER *);
  261 (* JBOOLEAN *);
  262 (* JCHAR *);
  263 (* JVOID *);
  264 (* TRUE *);
  265 (* FALSE *);
  266 (* SEMI *);
  267 (* LPAREN *);
  268 (* RPAREN *);
  269 (* LBRACE *);
  270 (* RBRACE *);
  271 (* LBRACKET *);
  272 (* RBRACKET *);
  273 (* COMMA *);
  274 (* DOT *);
  275 (* PLUS *);
  276 (* MINUS *);
  277 (* TIMES *);
  278 (* DIVIDE *);
  279 (* ASSIGN *);
  280 (* NOT *);
  281 (* AND *);
  282 (* OR *);
  283 (* EQ *);
  284 (* NEQ *);
  285 (* LT *);
  286 (* LEQ *);
  287 (* GT *);
  288 (* GEQ *);
  289 (* RETURN *);
  290 (* IF *);
  291 (* ELSEIF *);
  292 (* ELSE *);
  293 (* FOR *);
  294 (* WHILE *);
  295 (* NEW *);
  296 (* BREAK *);
  297 (* CONTINUE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  298 (* INT_LITERAL *);
  299 (* FLOAT_LITERAL *);
  300 (* STRING_LITERAL *);
  301 (* CHAR_LITERAL *);
  302 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\004\000\004\000\006\000\006\000\
\006\000\006\000\005\000\005\000\007\000\008\000\009\000\014\000\
\014\000\014\000\011\000\015\000\015\000\010\000\016\000\012\000\
\012\000\017\000\017\000\018\000\018\000\019\000\019\000\013\000\
\013\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\021\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\022\000\022\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\002\000\006\000\006\000\000\000\002\000\
\002\000\002\000\001\000\001\000\003\000\007\000\009\000\001\000\
\001\000\001\000\002\000\001\000\001\000\001\000\002\000\000\000\
\001\000\001\000\003\000\000\000\001\000\001\000\003\000\000\000\
\002\000\002\000\002\000\002\000\003\000\003\000\005\000\007\000\
\009\000\005\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000\003\000\003\000\004\000\
\000\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\012\000\011\000\067\000\000\000\000\000\003\000\
\000\000\001\000\004\000\000\000\000\000\007\000\000\000\000\000\
\005\000\000\000\008\000\009\000\010\000\000\000\019\000\017\000\
\016\000\018\000\000\000\021\000\020\000\022\000\000\000\000\000\
\000\000\000\000\026\000\000\000\000\000\023\000\000\000\000\000\
\000\000\032\000\027\000\000\000\000\000\032\000\046\000\047\000\
\000\000\032\000\014\000\000\000\000\000\000\000\000\000\000\000\
\043\000\044\000\045\000\000\000\000\000\000\000\000\000\033\000\
\000\000\000\000\000\000\061\000\036\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\034\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\063\000\038\000\037\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\013\000\000\000\
\000\000\051\000\052\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\064\000\000\000\
\000\000\000\000\042\000\000\000\000\000\000\000\040\000\000\000\
\000\000\041\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\061\000\015\000\062\000\020\000\
\021\000\033\000\028\000\034\000\045\000\029\000\030\000\035\000\
\036\000\099\000\100\000\063\000\064\000\097\000"

let yysindex = "\005\000\
\057\255\000\000\000\000\000\000\000\000\013\000\057\255\000\000\
\027\255\000\000\000\000\005\255\054\255\000\000\055\255\039\255\
\000\000\137\255\000\000\000\000\000\000\062\255\000\000\000\000\
\000\000\000\000\044\255\000\000\000\000\000\000\137\255\090\255\
\067\255\106\255\000\000\104\255\137\255\000\000\109\255\137\255\
\113\255\000\000\000\000\117\255\041\255\000\000\000\000\000\000\
\209\255\000\000\000\000\209\255\204\255\115\255\122\255\123\255\
\000\000\000\000\000\000\059\255\137\255\125\255\004\000\000\000\
\086\255\245\254\103\255\000\000\000\000\018\000\209\255\209\255\
\209\255\209\255\209\255\093\255\000\000\000\000\209\255\209\255\
\209\255\209\255\209\255\209\255\209\255\209\255\209\255\209\255\
\209\255\209\255\000\000\000\000\000\000\000\000\168\000\222\000\
\150\255\189\000\135\255\141\255\222\000\222\000\000\000\087\255\
\087\255\000\000\000\000\250\000\236\000\254\000\254\000\133\255\
\133\255\133\255\133\255\165\255\209\255\165\255\000\000\209\255\
\127\255\032\000\000\000\222\000\165\255\209\255\000\000\153\255\
\165\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\169\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\158\255\101\255\
\000\000\000\000\000\000\171\255\158\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\246\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\177\255\
\000\000\180\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\255\
\000\000\000\000\000\000\183\255\030\255\251\254\000\000\212\255\
\055\000\000\000\000\000\154\255\081\255\036\255\212\000\078\000\
\101\000\124\000\147\000\000\000\000\000\000\000\000\000\000\000\
\148\255\000\000\000\000\064\255\000\000\184\255\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\190\000\065\000\000\000\185\000\000\000\
\000\000\240\255\186\000\167\000\046\000\000\000\000\000\165\000\
\000\000\000\000\000\000\207\255\059\000\080\000"

let yytablesize = 542
let yytable = "\066\000\
\092\000\027\000\068\000\070\000\062\000\001\000\062\000\079\000\
\080\000\081\000\082\000\062\000\010\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\095\000\096\000\098\000\
\101\000\102\000\066\000\012\000\066\000\104\000\105\000\106\000\
\107\000\108\000\109\000\110\000\111\000\112\000\113\000\114\000\
\115\000\030\000\003\000\004\000\076\000\053\000\030\000\053\000\
\047\000\048\000\013\000\049\000\053\000\050\000\051\000\016\000\
\003\000\004\000\003\000\004\000\053\000\053\000\053\000\053\000\
\052\000\009\000\014\000\122\000\017\000\074\000\124\000\009\000\
\031\000\053\000\054\000\031\000\096\000\055\000\056\000\018\000\
\031\000\075\000\057\000\058\000\023\000\059\000\060\000\003\000\
\004\000\032\000\060\000\065\000\060\000\047\000\048\000\067\000\
\049\000\060\000\050\000\091\000\037\000\013\000\013\000\013\000\
\003\000\004\000\060\000\081\000\082\000\052\000\047\000\048\000\
\038\000\049\000\013\000\050\000\093\000\039\000\053\000\054\000\
\040\000\042\000\055\000\056\000\044\000\071\000\052\000\057\000\
\058\000\046\000\059\000\060\000\072\000\073\000\077\000\053\000\
\054\000\016\000\103\000\055\000\056\000\024\000\025\000\026\000\
\057\000\058\000\119\000\059\000\060\000\039\000\039\000\079\000\
\080\000\081\000\082\000\039\000\039\000\120\000\039\000\117\000\
\039\000\039\000\125\000\059\000\129\000\059\000\003\000\004\000\
\002\000\024\000\059\000\039\000\047\000\048\000\121\000\049\000\
\123\000\050\000\059\000\059\000\039\000\039\000\025\000\127\000\
\039\000\039\000\065\000\130\000\052\000\039\000\039\000\028\000\
\039\000\039\000\029\000\065\000\011\000\053\000\054\000\019\000\
\022\000\055\000\056\000\041\000\043\000\128\000\057\000\058\000\
\000\000\059\000\060\000\047\000\048\000\069\000\049\000\000\000\
\047\000\048\000\000\000\049\000\000\000\049\000\000\000\049\000\
\000\000\000\000\000\000\052\000\049\000\000\000\049\000\049\000\
\052\000\000\000\000\000\000\000\049\000\049\000\049\000\049\000\
\049\000\049\000\049\000\049\000\000\000\057\000\058\000\000\000\
\059\000\060\000\057\000\058\000\000\000\059\000\060\000\048\000\
\000\000\048\000\000\000\000\000\000\000\000\000\048\000\000\000\
\048\000\048\000\048\000\048\000\000\000\078\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\079\000\080\000\
\081\000\082\000\000\000\094\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\079\000\080\000\081\000\082\000\
\000\000\126\000\083\000\084\000\085\000\086\000\087\000\088\000\
\089\000\090\000\079\000\080\000\081\000\082\000\000\000\000\000\
\083\000\084\000\085\000\086\000\087\000\088\000\089\000\090\000\
\050\000\000\000\050\000\000\000\000\000\000\000\000\000\050\000\
\000\000\050\000\050\000\000\000\000\000\000\000\000\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\055\000\
\000\000\055\000\000\000\000\000\000\000\000\000\055\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\055\000\055\000\
\055\000\055\000\055\000\055\000\055\000\055\000\056\000\000\000\
\056\000\000\000\000\000\000\000\000\000\056\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\056\000\056\000\056\000\
\056\000\056\000\056\000\056\000\056\000\057\000\000\000\057\000\
\000\000\000\000\000\000\000\000\057\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\057\000\057\000\057\000\057\000\
\057\000\057\000\057\000\057\000\058\000\000\000\058\000\000\000\
\000\000\000\000\000\000\058\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\058\000\058\000\058\000\058\000\058\000\
\058\000\058\000\058\000\116\000\000\000\000\000\000\000\000\000\
\000\000\000\000\079\000\080\000\081\000\082\000\000\000\000\000\
\083\000\084\000\085\000\086\000\087\000\088\000\089\000\090\000\
\118\000\000\000\000\000\000\000\000\000\000\000\000\000\079\000\
\080\000\081\000\082\000\000\000\000\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\054\000\000\000\054\000\
\000\000\000\000\000\000\000\000\054\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\054\000\054\000\054\000\054\000\
\079\000\080\000\081\000\082\000\000\000\000\000\083\000\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\079\000\080\000\
\081\000\082\000\000\000\000\000\083\000\000\000\085\000\086\000\
\087\000\088\000\089\000\090\000\079\000\080\000\081\000\082\000\
\079\000\080\000\081\000\082\000\085\000\086\000\087\000\088\000\
\089\000\090\000\087\000\088\000\089\000\090\000"

let yycheck = "\049\000\
\012\001\018\000\052\000\053\000\010\001\001\000\012\001\019\001\
\020\001\021\001\022\001\017\001\000\000\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\071\000\072\000\073\000\
\074\000\075\000\010\001\001\001\012\001\079\000\080\000\081\000\
\082\000\083\000\084\000\085\000\086\000\087\000\088\000\089\000\
\090\000\012\001\002\001\003\001\061\000\010\001\017\001\012\001\
\008\001\009\001\046\001\011\001\017\001\013\001\014\001\001\001\
\002\001\003\001\002\001\003\001\025\001\026\001\027\001\028\001\
\024\001\001\000\013\001\117\000\014\001\011\001\120\000\007\000\
\011\001\033\001\034\001\012\001\126\000\037\001\038\001\015\000\
\017\001\023\001\042\001\043\001\046\001\045\001\046\001\002\001\
\003\001\046\001\010\001\046\000\012\001\008\001\009\001\050\000\
\011\001\017\001\013\001\014\001\011\001\001\001\002\001\003\001\
\002\001\003\001\026\001\021\001\022\001\024\001\008\001\009\001\
\046\001\011\001\014\001\013\001\014\001\012\001\033\001\034\001\
\017\001\013\001\037\001\038\001\012\001\011\001\024\001\042\001\
\043\001\013\001\045\001\046\001\011\001\011\001\010\001\033\001\
\034\001\001\001\046\001\037\001\038\001\005\001\006\001\007\001\
\042\001\043\001\012\001\045\001\046\001\002\001\003\001\019\001\
\020\001\021\001\022\001\008\001\009\001\017\001\011\001\010\001\
\013\001\014\001\036\001\010\001\012\001\012\001\002\001\003\001\
\000\000\012\001\017\001\024\001\008\001\009\001\116\000\011\001\
\118\000\013\001\025\001\026\001\033\001\034\001\012\001\125\000\
\037\001\038\001\010\001\129\000\024\001\042\001\043\001\012\001\
\045\001\046\001\012\001\012\001\007\000\033\001\034\001\015\000\
\015\000\037\001\038\001\037\000\040\000\126\000\042\001\043\001\
\255\255\045\001\046\001\008\001\009\001\010\001\011\001\255\255\
\008\001\009\001\255\255\011\001\255\255\010\001\255\255\012\001\
\255\255\255\255\255\255\024\001\017\001\255\255\019\001\020\001\
\024\001\255\255\255\255\255\255\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\255\255\042\001\043\001\255\255\
\045\001\046\001\042\001\043\001\255\255\045\001\046\001\010\001\
\255\255\012\001\255\255\255\255\255\255\255\255\017\001\255\255\
\019\001\020\001\021\001\022\001\255\255\010\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\019\001\020\001\
\021\001\022\001\255\255\010\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\019\001\020\001\021\001\022\001\
\255\255\010\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\019\001\020\001\021\001\022\001\255\255\255\255\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\010\001\255\255\012\001\255\255\255\255\255\255\255\255\017\001\
\255\255\019\001\020\001\255\255\255\255\255\255\255\255\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\010\001\
\255\255\012\001\255\255\255\255\255\255\255\255\017\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\010\001\255\255\
\012\001\255\255\255\255\255\255\255\255\017\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\010\001\255\255\012\001\
\255\255\255\255\255\255\255\255\017\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\010\001\255\255\012\001\255\255\
\255\255\255\255\255\255\017\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\012\001\255\255\255\255\255\255\255\255\
\255\255\255\255\019\001\020\001\021\001\022\001\255\255\255\255\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\012\001\255\255\255\255\255\255\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001\255\255\255\255\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\010\001\255\255\012\001\
\255\255\255\255\255\255\255\255\017\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\025\001\026\001\027\001\028\001\
\019\001\020\001\021\001\022\001\255\255\255\255\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\019\001\020\001\
\021\001\022\001\255\255\255\255\025\001\255\255\027\001\028\001\
\029\001\030\001\031\001\032\001\019\001\020\001\021\001\022\001\
\019\001\020\001\021\001\022\001\027\001\028\001\029\001\030\001\
\031\001\032\001\029\001\030\001\031\001\032\001"

let yynames_const = "\
  CLASS\000\
  PUBLIC\000\
  PRIVATE\000\
  NUMBER\000\
  JBOOLEAN\000\
  JCHAR\000\
  JVOID\000\
  TRUE\000\
  FALSE\000\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  COMMA\000\
  DOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  AND\000\
  OR\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  RETURN\000\
  IF\000\
  ELSEIF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  NEW\000\
  BREAK\000\
  CONTINUE\000\
  EOF\000\
  "

let yynames_block = "\
  INT_LITERAL\000\
  FLOAT_LITERAL\000\
  STRING_LITERAL\000\
  CHAR_LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cdecls) in
    Obj.repr(
# 38 "parser.mly"
            ( Program(_1) )
# 399 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cdecl_list) in
    Obj.repr(
# 40 "parser.mly"
                   ( List.rev _1 )
# 406 "parser.ml"
               : 'cdecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cdecl) in
    Obj.repr(
# 43 "parser.mly"
          ( [_1] )
# 413 "parser.ml"
               : 'cdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cdecl) in
    Obj.repr(
# 44 "parser.mly"
                    ( _2::_1 )
# 421 "parser.ml"
               : 'cdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'scope) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'cbody) in
    Obj.repr(
# 47 "parser.mly"
                                     ( {
			cscope = _1;
			cname = _3;
			cbody = _5
		} )
# 434 "parser.ml"
               : 'cdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'scope) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'cbody) in
    Obj.repr(
# 52 "parser.mly"
                                       ( {
			cscope = _1;
			cname = _3;
			cbody = _5
		} )
# 447 "parser.ml"
               : 'cdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
               ( { 
		variables = [];
		constructors = [];
		methods = [];
	} )
# 457 "parser.ml"
               : 'cbody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cbody) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 64 "parser.mly"
                ( { 
			variables = _2 :: _1.variables;
			constructors = _1.constructors;
			methods = _1.methods;
		} )
# 469 "parser.ml"
               : 'cbody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cbody) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constructor) in
    Obj.repr(
# 69 "parser.mly"
                       ( { 
			variables = _1.variables;
			constructors = _2 :: _1.constructors;
			methods = _1.methods;
		} )
# 481 "parser.ml"
               : 'cbody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cbody) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 74 "parser.mly"
                 ( { 
			variables = _1.variables;
			constructors = _1.constructors;
			methods = _2 :: _1.methods;
		} )
# 493 "parser.ml"
               : 'cbody))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
           ( Private )
# 499 "parser.ml"
               : 'scope))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
           ( Public )
# 505 "parser.ml"
               : 'scope))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'scope) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'datatype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
                   ({ 
		vscope = _1;
		vtype = _2;
		vname = _3;
	})
# 518 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 95 "parser.mly"
                                                                             (
		{
			fscope = Public;
			fname = _1;
			freturn = JVoid; (* WTFFFFF some sort of object retunr type *)
			fformals = _3;
			fbody = List.rev _6;
		        
		}
	)
# 536 "parser.ml"
               : 'constructor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'scope) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'datatype) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 110 "parser.mly"
  ( { fscope = _1;
			freturn = _2; 
			fname = _3; 
			fformals = _5;
			fbody = List.rev _8 } )
# 551 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
                    ( JChar )
# 557 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                ( JBoolean )
# 563 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
              ( JVoid )
# 569 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "parser.mly"
          ( _2 )
# 576 "parser.ml"
               : 'name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primitive) in
    Obj.repr(
# 126 "parser.mly"
             ( _1 )
# 583 "parser.ml"
               : 'type_tag))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 127 "parser.mly"
          ( Object(_1) )
# 590 "parser.ml"
               : 'type_tag))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_tag) in
    Obj.repr(
# 130 "parser.mly"
            ( _1 )
# 597 "parser.ml"
               : 'datatype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'datatype) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 134 "parser.mly"
  ({
		vtype = _1;
	  	vname = _2; 
	  	})
# 608 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "parser.mly"
                           ( [] )
# 614 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 140 "parser.mly"
               ( List.rev _1 )
# 621 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 143 "parser.mly"
          ( [_1] )
# 628 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 144 "parser.mly"
                            ( _3 :: _1 )
# 636 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
                ( [] )
# 642 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 148 "parser.mly"
                  ( List.rev _1 )
# 649 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 151 "parser.mly"
                          ( [_1] )
# 656 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 152 "parser.mly"
                            ( _3 :: _1 )
# 664 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "parser.mly"
               ( [] )
# 670 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 159 "parser.mly"
                  ( _2 :: _1 )
# 678 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 162 "parser.mly"
             ( Expr _1 )
# 685 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 163 "parser.mly"
              ( VarDecl(_1) )
# 692 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 164 "parser.mly"
               ( Return Noexpr )
# 698 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 165 "parser.mly"
                    ( Return _2 )
# 705 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 166 "parser.mly"
                           ( Block(List.rev _2) )
# 712 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 167 "parser.mly"
                                           ( If(_3, _5, Block([])) )
# 720 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 168 "parser.mly"
                                        ( If(_3, _5, _7) )
# 729 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 169 "parser.mly"
                                                           ( For(_3, _5, _7, _9) )
# 739 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 170 "parser.mly"
                                 ( While(_3, _5) )
# 747 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 175 "parser.mly"
               ( Int_Lit(_1) )
# 754 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 176 "parser.mly"
                 ( Float_Lit(_1))
# 761 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 177 "parser.mly"
                ( Char_Lit(_1))
# 768 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 178 "parser.mly"
        ( Bool_Lit(true) )
# 774 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 179 "parser.mly"
         ( Bool_Lit(false) )
# 780 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 180 "parser.mly"
      ( Id(_1) )
# 787 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 181 "parser.mly"
                  ( Binop(_1, Add, _3) )
# 795 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 182 "parser.mly"
                   ( Binop(_1, Sub, _3) )
# 803 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 183 "parser.mly"
                   ( Binop(_1, Mult, _3) )
# 811 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 184 "parser.mly"
                    ( Binop(_1, Div, _3) )
# 819 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 185 "parser.mly"
                ( Binop(_1, Equal, _3) )
# 827 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 186 "parser.mly"
                 ( Binop(_1, Neq, _3) )
# 835 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 187 "parser.mly"
                ( Binop(_1, Less, _3) )
# 843 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 188 "parser.mly"
                 ( Binop(_1, Leq, _3) )
# 851 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 189 "parser.mly"
                ( Binop(_1, Greater, _3) )
# 859 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 190 "parser.mly"
                 ( Binop(_1, Geq, _3) )
# 867 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "parser.mly"
                 ( Binop(_1, And, _3) )
# 875 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 192 "parser.mly"
                ( Binop(_1, Or, _3) )
# 883 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 193 "parser.mly"
            ( Unop(Not, _2) )
# 890 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 194 "parser.mly"
                  ( Assign(_1, _3) )
# 898 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 195 "parser.mly"
                      ( _2 )
# 905 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 196 "parser.mly"
                                ( Call(_1, _3) )
# 913 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 200 "parser.mly"
               ( Noexpr )
# 919 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 201 "parser.mly"
        ( _1 )
# 926 "parser.ml"
               : 'expr_opt))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)

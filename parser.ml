type token =
  | CLASS
  | PUBLIC
  | PRIVATE
  | NUMBER
  | JBOOLEAN
  | JCHAR
  | JINT
  | JFLOAT
  | JVOID
  | TRUE
  | FALSE
  | NULL
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
# 58 "parser.ml"
let yytransl_const = [|
  257 (* CLASS *);
  258 (* PUBLIC *);
  259 (* PRIVATE *);
  260 (* NUMBER *);
  261 (* JBOOLEAN *);
  262 (* JCHAR *);
  263 (* JINT *);
  264 (* JFLOAT *);
  265 (* JVOID *);
  266 (* TRUE *);
  267 (* FALSE *);
  268 (* NULL *);
  269 (* SEMI *);
  270 (* LPAREN *);
  271 (* RPAREN *);
  272 (* LBRACE *);
  273 (* RBRACE *);
  274 (* LBRACKET *);
  275 (* RBRACKET *);
  276 (* COMMA *);
  277 (* DOT *);
  278 (* PLUS *);
  279 (* MINUS *);
  280 (* TIMES *);
  281 (* DIVIDE *);
  282 (* ASSIGN *);
  283 (* NOT *);
  284 (* AND *);
  285 (* OR *);
  286 (* EQ *);
  287 (* NEQ *);
  288 (* LT *);
  289 (* LEQ *);
  290 (* GT *);
  291 (* GEQ *);
  292 (* RETURN *);
  293 (* IF *);
  294 (* ELSEIF *);
  295 (* ELSE *);
  296 (* FOR *);
  297 (* WHILE *);
  298 (* NEW *);
  299 (* BREAK *);
  300 (* CONTINUE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  301 (* INT_LITERAL *);
  302 (* FLOAT_LITERAL *);
  303 (* STRING_LITERAL *);
  304 (* CHAR_LITERAL *);
  305 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\005\000\005\000\004\000\006\000\
\006\000\006\000\006\000\007\000\008\000\009\000\014\000\014\000\
\014\000\014\000\014\000\011\000\015\000\015\000\010\000\016\000\
\012\000\012\000\017\000\017\000\018\000\018\000\019\000\019\000\
\013\000\013\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\021\000\021\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\022\000\022\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\002\000\001\000\001\000\006\000\000\000\
\002\000\002\000\002\000\003\000\007\000\009\000\001\000\001\000\
\001\000\001\000\001\000\002\000\001\000\001\000\001\000\002\000\
\000\000\001\000\001\000\003\000\000\000\001\000\001\000\003\000\
\000\000\002\000\002\000\002\000\002\000\003\000\003\000\005\000\
\007\000\009\000\005\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\003\000\003\000\004\000\000\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\006\000\005\000\071\000\000\000\000\000\003\000\
\000\000\001\000\004\000\000\000\000\000\008\000\000\000\000\000\
\007\000\000\000\009\000\010\000\011\000\000\000\020\000\018\000\
\015\000\016\000\017\000\019\000\000\000\022\000\021\000\023\000\
\000\000\000\000\000\000\000\000\027\000\000\000\000\000\024\000\
\000\000\000\000\000\000\033\000\028\000\000\000\000\000\033\000\
\065\000\066\000\070\000\000\000\033\000\013\000\000\000\000\000\
\000\000\000\000\000\000\063\000\064\000\067\000\068\000\000\000\
\000\000\000\000\000\000\034\000\044\000\000\000\000\000\000\000\
\057\000\037\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\036\000\035\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\059\000\039\000\038\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\000\000\000\000\047\000\048\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\060\000\000\000\000\000\000\000\043\000\
\000\000\000\000\000\000\041\000\000\000\000\000\042\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\065\000\015\000\066\000\020\000\
\021\000\035\000\030\000\036\000\047\000\031\000\032\000\037\000\
\038\000\104\000\105\000\067\000\068\000\102\000\069\000"

let yysindex = "\001\000\
\027\255\000\000\000\000\000\000\000\000\010\000\027\255\000\000\
\018\255\000\000\000\000\251\254\043\255\000\000\065\255\048\255\
\000\000\228\255\000\000\000\000\000\000\085\255\000\000\000\000\
\000\000\000\000\000\000\000\000\067\255\000\000\000\000\000\000\
\228\255\100\255\070\255\102\255\000\000\108\255\228\255\000\000\
\121\255\228\255\127\255\000\000\000\000\123\255\044\255\000\000\
\000\000\000\000\000\000\212\255\000\000\000\000\212\255\004\255\
\129\255\134\255\135\255\000\000\000\000\000\000\000\000\031\255\
\228\255\145\255\007\000\000\000\000\000\084\255\194\000\124\255\
\000\000\000\000\021\000\212\255\212\255\212\255\212\255\212\255\
\110\255\000\000\000\000\212\255\212\255\212\255\212\255\212\255\
\212\255\212\255\212\255\212\255\212\255\212\255\212\255\000\000\
\000\000\000\000\000\000\215\000\013\001\149\255\236\000\148\255\
\157\255\013\001\013\001\000\000\050\255\050\255\000\000\000\000\
\162\255\122\255\027\001\027\001\243\255\243\255\243\255\243\255\
\205\255\212\255\205\255\000\000\212\255\140\255\035\000\000\000\
\013\001\205\255\212\255\000\000\153\255\205\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\182\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\168\255\101\255\000\000\000\000\000\000\173\255\168\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\249\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\176\255\000\000\175\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\252\254\000\000\000\000\000\000\
\183\255\068\255\249\254\000\000\058\000\081\000\000\000\000\000\
\049\255\093\255\248\254\003\001\104\000\127\000\150\000\173\000\
\000\000\000\000\000\000\000\000\000\000\164\255\000\000\000\000\
\090\255\000\000\184\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\195\000\108\000\000\000\188\000\000\000\
\000\000\239\255\191\000\175\000\059\000\000\000\000\000\176\000\
\000\000\000\000\000\000\204\255\198\255\089\000\000\000"

let yytablesize = 574
let yytable = "\071\000\
\029\000\001\000\073\000\075\000\049\000\058\000\049\000\058\000\
\062\000\010\000\062\000\049\000\058\000\049\000\050\000\051\000\
\074\000\052\000\012\000\049\000\049\000\049\000\049\000\100\000\
\101\000\103\000\106\000\107\000\003\000\004\000\055\000\109\000\
\110\000\111\000\112\000\113\000\114\000\115\000\116\000\117\000\
\118\000\119\000\120\000\013\000\079\000\003\000\004\000\081\000\
\060\000\061\000\062\000\063\000\064\000\049\000\050\000\051\000\
\080\000\052\000\014\000\053\000\054\000\055\000\126\000\055\000\
\128\000\016\000\003\000\004\000\055\000\127\000\055\000\132\000\
\129\000\086\000\087\000\135\000\055\000\055\000\101\000\056\000\
\057\000\017\000\031\000\058\000\059\000\003\000\004\000\031\000\
\060\000\061\000\062\000\063\000\064\000\049\000\050\000\051\000\
\023\000\052\000\033\000\053\000\096\000\012\000\012\000\012\000\
\032\000\056\000\070\000\056\000\009\000\032\000\055\000\072\000\
\056\000\039\000\009\000\034\000\041\000\012\000\040\000\056\000\
\057\000\056\000\018\000\058\000\059\000\003\000\004\000\042\000\
\060\000\061\000\062\000\063\000\064\000\049\000\050\000\051\000\
\044\000\052\000\048\000\053\000\098\000\046\000\076\000\084\000\
\085\000\086\000\087\000\077\000\078\000\088\000\055\000\090\000\
\091\000\092\000\093\000\094\000\095\000\082\000\108\000\056\000\
\057\000\122\000\124\000\058\000\059\000\040\000\040\000\134\000\
\060\000\061\000\062\000\063\000\064\000\040\000\040\000\040\000\
\125\000\040\000\130\000\040\000\040\000\002\000\025\000\084\000\
\085\000\086\000\087\000\026\000\061\000\029\000\040\000\090\000\
\091\000\092\000\093\000\094\000\095\000\030\000\061\000\040\000\
\040\000\011\000\019\000\040\000\040\000\022\000\003\000\004\000\
\040\000\040\000\040\000\040\000\040\000\043\000\049\000\050\000\
\051\000\045\000\052\000\133\000\053\000\049\000\050\000\051\000\
\000\000\052\000\000\000\000\000\016\000\000\000\000\000\055\000\
\024\000\025\000\026\000\027\000\028\000\000\000\055\000\000\000\
\056\000\057\000\000\000\000\000\058\000\059\000\000\000\000\000\
\000\000\060\000\061\000\062\000\063\000\064\000\000\000\000\000\
\060\000\061\000\062\000\063\000\064\000\069\000\000\000\069\000\
\084\000\085\000\086\000\087\000\069\000\000\000\069\000\069\000\
\069\000\069\000\000\000\083\000\069\000\069\000\069\000\069\000\
\069\000\069\000\069\000\069\000\084\000\085\000\086\000\087\000\
\000\000\099\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\084\000\085\000\086\000\087\000\000\000\131\000\
\088\000\089\000\090\000\091\000\092\000\093\000\094\000\095\000\
\084\000\085\000\086\000\087\000\000\000\000\000\088\000\089\000\
\090\000\091\000\092\000\093\000\094\000\095\000\045\000\000\000\
\045\000\000\000\000\000\000\000\000\000\045\000\000\000\045\000\
\045\000\000\000\000\000\000\000\000\000\045\000\045\000\045\000\
\045\000\045\000\045\000\045\000\045\000\046\000\000\000\046\000\
\000\000\000\000\000\000\000\000\046\000\000\000\046\000\046\000\
\000\000\000\000\000\000\000\000\046\000\046\000\046\000\046\000\
\046\000\046\000\046\000\046\000\051\000\000\000\051\000\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\051\000\051\000\051\000\051\000\051\000\
\051\000\051\000\051\000\052\000\000\000\052\000\000\000\000\000\
\000\000\000\000\052\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\052\000\052\000\052\000\052\000\052\000\052\000\
\052\000\052\000\053\000\000\000\053\000\000\000\000\000\000\000\
\000\000\053\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
\053\000\054\000\000\000\054\000\000\000\000\000\000\000\000\000\
\054\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
\097\000\000\000\000\000\000\000\000\000\000\000\000\000\084\000\
\085\000\086\000\087\000\000\000\000\000\088\000\089\000\090\000\
\091\000\092\000\093\000\094\000\095\000\121\000\000\000\000\000\
\000\000\000\000\000\000\000\000\084\000\085\000\086\000\087\000\
\000\000\000\000\088\000\089\000\090\000\091\000\092\000\093\000\
\094\000\095\000\123\000\000\000\000\000\000\000\000\000\000\000\
\000\000\084\000\085\000\086\000\087\000\000\000\000\000\088\000\
\089\000\090\000\091\000\092\000\093\000\094\000\095\000\050\000\
\000\000\050\000\000\000\000\000\000\000\000\000\050\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\050\000\050\000\
\050\000\050\000\084\000\085\000\086\000\087\000\000\000\000\000\
\088\000\089\000\090\000\091\000\092\000\093\000\094\000\095\000\
\084\000\085\000\086\000\087\000\000\000\000\000\000\000\000\000\
\000\000\000\000\092\000\093\000\094\000\095\000"

let yycheck = "\052\000\
\018\000\001\000\055\000\056\000\013\001\013\001\015\001\015\001\
\013\001\000\000\015\001\020\001\020\001\010\001\011\001\012\001\
\013\001\014\001\001\001\028\001\029\001\030\001\031\001\076\000\
\077\000\078\000\079\000\080\000\002\001\003\001\027\001\084\000\
\085\000\086\000\087\000\088\000\089\000\090\000\091\000\092\000\
\093\000\094\000\095\000\049\001\014\001\002\001\003\001\065\000\
\045\001\046\001\047\001\048\001\049\001\010\001\011\001\012\001\
\026\001\014\001\016\001\016\001\017\001\013\001\121\000\015\001\
\123\000\001\001\002\001\003\001\020\001\122\000\027\001\130\000\
\125\000\024\001\025\001\134\000\028\001\029\001\131\000\036\001\
\037\001\017\001\015\001\040\001\041\001\002\001\003\001\020\001\
\045\001\046\001\047\001\048\001\049\001\010\001\011\001\012\001\
\049\001\014\001\014\001\016\001\017\001\001\001\002\001\003\001\
\015\001\013\001\048\000\015\001\001\000\020\001\027\001\053\000\
\020\001\014\001\007\000\049\001\015\001\017\001\049\001\036\001\
\037\001\029\001\015\000\040\001\041\001\002\001\003\001\020\001\
\045\001\046\001\047\001\048\001\049\001\010\001\011\001\012\001\
\016\001\014\001\016\001\016\001\017\001\015\001\014\001\022\001\
\023\001\024\001\025\001\014\001\014\001\028\001\027\001\030\001\
\031\001\032\001\033\001\034\001\035\001\013\001\049\001\036\001\
\037\001\013\001\015\001\040\001\041\001\002\001\003\001\015\001\
\045\001\046\001\047\001\048\001\049\001\010\001\011\001\012\001\
\020\001\014\001\039\001\016\001\017\001\000\000\015\001\022\001\
\023\001\024\001\025\001\015\001\013\001\015\001\027\001\030\001\
\031\001\032\001\033\001\034\001\035\001\015\001\015\001\036\001\
\037\001\007\000\015\000\040\001\041\001\015\000\002\001\003\001\
\045\001\046\001\047\001\048\001\049\001\039\000\010\001\011\001\
\012\001\042\000\014\001\131\000\016\001\010\001\011\001\012\001\
\255\255\014\001\255\255\255\255\001\001\255\255\255\255\027\001\
\005\001\006\001\007\001\008\001\009\001\255\255\027\001\255\255\
\036\001\037\001\255\255\255\255\040\001\041\001\255\255\255\255\
\255\255\045\001\046\001\047\001\048\001\049\001\255\255\255\255\
\045\001\046\001\047\001\048\001\049\001\013\001\255\255\015\001\
\022\001\023\001\024\001\025\001\020\001\255\255\022\001\023\001\
\024\001\025\001\255\255\013\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\022\001\023\001\024\001\025\001\
\255\255\013\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\022\001\023\001\024\001\025\001\255\255\013\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\022\001\023\001\024\001\025\001\255\255\255\255\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\013\001\255\255\
\015\001\255\255\255\255\255\255\255\255\020\001\255\255\022\001\
\023\001\255\255\255\255\255\255\255\255\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\013\001\255\255\015\001\
\255\255\255\255\255\255\255\255\020\001\255\255\022\001\023\001\
\255\255\255\255\255\255\255\255\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\013\001\255\255\015\001\255\255\
\255\255\255\255\255\255\020\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\013\001\255\255\015\001\255\255\255\255\
\255\255\255\255\020\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\013\001\255\255\015\001\255\255\255\255\255\255\
\255\255\020\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\013\001\255\255\015\001\255\255\255\255\255\255\255\255\
\020\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\015\001\255\255\255\255\255\255\255\255\255\255\255\255\022\001\
\023\001\024\001\025\001\255\255\255\255\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\015\001\255\255\255\255\
\255\255\255\255\255\255\255\255\022\001\023\001\024\001\025\001\
\255\255\255\255\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\015\001\255\255\255\255\255\255\255\255\255\255\
\255\255\022\001\023\001\024\001\025\001\255\255\255\255\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\013\001\
\255\255\015\001\255\255\255\255\255\255\255\255\020\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\028\001\029\001\
\030\001\031\001\022\001\023\001\024\001\025\001\255\255\255\255\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\022\001\023\001\024\001\025\001\255\255\255\255\255\255\255\255\
\255\255\255\255\032\001\033\001\034\001\035\001"

let yynames_const = "\
  CLASS\000\
  PUBLIC\000\
  PRIVATE\000\
  NUMBER\000\
  JBOOLEAN\000\
  JCHAR\000\
  JINT\000\
  JFLOAT\000\
  JVOID\000\
  TRUE\000\
  FALSE\000\
  NULL\000\
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
# 416 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cdecl_list) in
    Obj.repr(
# 40 "parser.mly"
                   ( List.rev _1 )
# 423 "parser.ml"
               : 'cdecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cdecl) in
    Obj.repr(
# 43 "parser.mly"
          ( [_1] )
# 430 "parser.ml"
               : 'cdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'cdecl) in
    Obj.repr(
# 44 "parser.mly"
                    ( _2::_1 )
# 438 "parser.ml"
               : 'cdecl_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 47 "parser.mly"
           ( Private )
# 444 "parser.ml"
               : 'scope))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
           ( Public )
# 450 "parser.ml"
               : 'scope))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'scope) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'cbody) in
    Obj.repr(
# 51 "parser.mly"
                                     ( {
			cscope = _1;
			cname = _3;
			cbody = _5
		} )
# 463 "parser.ml"
               : 'cdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
               ( { 
		variables = [];
		constructors = [];
		methods = [];
	} )
# 473 "parser.ml"
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
# 485 "parser.ml"
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
# 497 "parser.ml"
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
# 509 "parser.ml"
               : 'cbody))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'scope) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'datatype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
                   ({ 
		vscope = _1;
		vtype = _2;
		vname = _3;
	})
# 522 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'name) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 90 "parser.mly"
                                                                             (
		{
			fscope = Public;
			fname = _1;
			freturn = JVoid; (* WTFFFFF some sort of object retunr type *)
			fformals = _3;
			fbody = List.rev _6;
		        
		}
	)
# 540 "parser.ml"
               : 'constructor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'scope) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'datatype) in
    let _3 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'formals_opt) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 105 "parser.mly"
  ( { fscope = _1;
			freturn = _2; 
			fname = _3; 
			fformals = _5;
			fbody = List.rev _8 } )
# 555 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                    ( JChar )
# 561 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
            ( JInt )
# 567 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
               ( JFloat )
# 573 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                ( JBoolean )
# 579 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
              ( JVoid )
# 585 "parser.ml"
               : 'primitive))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 119 "parser.mly"
          ( _2 )
# 592 "parser.ml"
               : 'name))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primitive) in
    Obj.repr(
# 122 "parser.mly"
             ( _1 )
# 599 "parser.ml"
               : 'type_tag))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'name) in
    Obj.repr(
# 123 "parser.mly"
          ( Object(_1) )
# 606 "parser.ml"
               : 'type_tag))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'type_tag) in
    Obj.repr(
# 126 "parser.mly"
            ( _1 )
# 613 "parser.ml"
               : 'datatype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'datatype) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "parser.mly"
  ({
		vtype = _1;
	  	vname = _2; 
	  	})
# 624 "parser.ml"
               : 'formal))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
                           ( [] )
# 630 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 136 "parser.mly"
               ( List.rev _1 )
# 637 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 139 "parser.mly"
          ( [_1] )
# 644 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal) in
    Obj.repr(
# 140 "parser.mly"
                            ( _3 :: _1 )
# 652 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
                ( [] )
# 658 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 144 "parser.mly"
                  ( List.rev _1 )
# 665 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 147 "parser.mly"
                          ( [_1] )
# 672 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 148 "parser.mly"
                            ( _3 :: _1 )
# 680 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 154 "parser.mly"
               ( [] )
# 686 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 155 "parser.mly"
                  ( _2 :: _1 )
# 694 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 158 "parser.mly"
             ( Expr _1 )
# 701 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    Obj.repr(
# 159 "parser.mly"
              ( VarDecl(_1) )
# 708 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "parser.mly"
               ( Return Noexpr )
# 714 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 161 "parser.mly"
                    ( Return _2 )
# 721 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 162 "parser.mly"
                           ( Block(List.rev _2) )
# 728 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 163 "parser.mly"
                                           ( If(_3, _5, Block([])) )
# 736 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 164 "parser.mly"
                                        ( If(_3, _5, _7) )
# 745 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 165 "parser.mly"
                                                           ( For(_3, _5, _7, _9) )
# 755 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 166 "parser.mly"
                                 ( While(_3, _5) )
# 763 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'literals) in
    Obj.repr(
# 171 "parser.mly"
            ( _1 )
# 770 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 172 "parser.mly"
                  ( Binop(_1, Add, _3) )
# 778 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 173 "parser.mly"
                   ( Binop(_1, Sub, _3) )
# 786 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 174 "parser.mly"
                   ( Binop(_1, Mult, _3) )
# 794 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 175 "parser.mly"
                    ( Binop(_1, Div, _3) )
# 802 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 176 "parser.mly"
                ( Binop(_1, Equal, _3) )
# 810 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 177 "parser.mly"
                 ( Binop(_1, Neq, _3) )
# 818 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 178 "parser.mly"
                ( Binop(_1, Less, _3) )
# 826 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 179 "parser.mly"
                 ( Binop(_1, Leq, _3) )
# 834 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 180 "parser.mly"
                ( Binop(_1, Greater, _3) )
# 842 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 181 "parser.mly"
                 ( Binop(_1, Geq, _3) )
# 850 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 182 "parser.mly"
                 ( Binop(_1, And, _3) )
# 858 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 183 "parser.mly"
                ( Binop(_1, Or, _3) )
# 866 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 184 "parser.mly"
            ( Unop(Not, _2) )
# 873 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 185 "parser.mly"
                  ( Assign(_1, _3) )
# 881 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 186 "parser.mly"
                      ( _2 )
# 888 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 187 "parser.mly"
                                ( FuncCall(_1, _3) )
# 896 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 190 "parser.mly"
               ( Noexpr )
# 902 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 191 "parser.mly"
        ( _1 )
# 909 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 194 "parser.mly"
                      ( Int_Lit(_1) )
# 916 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 195 "parser.mly"
                      ( Float_Lit(_1) )
# 923 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    Obj.repr(
# 196 "parser.mly"
               ( Bool_Lit(true) )
# 929 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    Obj.repr(
# 197 "parser.mly"
                ( Bool_Lit(false) )
# 935 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 198 "parser.mly"
                      ( String_Lit(_1) )
# 942 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 199 "parser.mly"
                  ( Char_Lit(_1) )
# 949 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 200 "parser.mly"
              ( Id(_1) )
# 956 "parser.ml"
               : 'literals))
; (fun __caml_parser_env ->
    Obj.repr(
# 201 "parser.mly"
               ( Null )
# 962 "parser.ml"
               : 'literals))
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

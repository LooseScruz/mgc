type token =
  | VOID
  | CHAR
  | INT
  | STRING
  | LONG
  | FLOAT
  | BOOL
  | IF
  | ELSE
  | WHILE
  | RETURN
  | STRUCT
  | FOR
  | NULL
  | NEW
  | ARRAY
  | EQ
  | NE
  | LT
  | LE
  | GT
  | GE
  | CHARCON of (char)
  | INTCON of (int)
  | FLOATCON of (string)
  | IDENTIFIER of (string)
  | STRINGCON of (string)
  | BOOLCON of (bool)
  | EOF
  | NEWLINE
  | CALL
  | IFELSE
  | INITDECL
  | POS
  | NEG
  | NEWARRAY
  | TYPEID
  | FIELD
  | ORD
  | CHR
  | ROOT
  | NEWSTRING
  | RETURNVOID
  | INDEX
  | VARDECL
  | FUNCTION
  | PARAMLIST
  | PROTOTYPE
  | DECLID
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | MUL
  | ADD
  | SUB
  | MOD
  | DIV
  | ASSIGN
  | NOT
  | INDENT
  | DEDENT

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 71 "parser.ml"
let yytransl_const = [|
  257 (* VOID *);
  258 (* CHAR *);
  259 (* INT *);
  260 (* STRING *);
  261 (* LONG *);
  262 (* FLOAT *);
  263 (* BOOL *);
  264 (* IF *);
  265 (* ELSE *);
  266 (* WHILE *);
  267 (* RETURN *);
  268 (* STRUCT *);
  269 (* FOR *);
  270 (* NULL *);
  271 (* NEW *);
  272 (* ARRAY *);
  273 (* EQ *);
  274 (* NE *);
  275 (* LT *);
  276 (* LE *);
  277 (* GT *);
  278 (* GE *);
    0 (* EOF *);
  285 (* NEWLINE *);
  286 (* CALL *);
  287 (* IFELSE *);
  288 (* INITDECL *);
  289 (* POS *);
  290 (* NEG *);
  291 (* NEWARRAY *);
  292 (* TYPEID *);
  293 (* FIELD *);
  294 (* ORD *);
  295 (* CHR *);
  296 (* ROOT *);
  297 (* NEWSTRING *);
  298 (* RETURNVOID *);
  299 (* INDEX *);
  300 (* VARDECL *);
  301 (* FUNCTION *);
  302 (* PARAMLIST *);
  303 (* PROTOTYPE *);
  304 (* DECLID *);
  305 (* LPAREN *);
  306 (* RPAREN *);
  307 (* LBRACE *);
  308 (* RBRACE *);
  309 (* COMMA *);
  310 (* MUL *);
  311 (* ADD *);
  312 (* SUB *);
  313 (* MOD *);
  314 (* DIV *);
  315 (* ASSIGN *);
  316 (* NOT *);
  317 (* INDENT *);
  318 (* DEDENT *);
    0|]

let yytransl_block = [|
  279 (* CHARCON *);
  280 (* INTCON *);
  281 (* FLOATCON *);
  282 (* IDENTIFIER *);
  283 (* STRINGCON *);
  284 (* BOOLCON *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\002\000\004\000\
\006\000\006\000\009\000\009\000\005\000\005\000\005\000\005\000\
\007\000\007\000\003\000\008\000\008\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\012\000\012\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\013\000\013\000\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\000\000\002\000\002\000\009\000\
\000\000\001\000\002\000\004\000\001\000\001\000\001\000\001\000\
\000\000\002\000\003\000\000\000\002\000\002\000\003\000\003\000\
\005\000\007\000\009\000\005\000\000\000\001\000\001\000\001\000\
\001\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\003\000\004\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\056\000\000\000\016\000\013\000\015\000\014\000\
\001\000\003\000\004\000\000\000\000\000\019\000\000\000\000\000\
\000\000\000\000\011\000\000\000\000\000\017\000\000\000\000\000\
\012\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\031\000\032\000\000\000\033\000\000\000\020\000\008\000\
\000\000\000\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\048\000\047\000\000\000\
\000\000\000\000\022\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\023\000\000\000\
\000\000\000\000\000\000\000\000\051\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\037\000\038\000\000\000\000\000\000\000\050\000\000\000\000\000\
\028\000\000\000\000\000\000\000\000\000\026\000\000\000\000\000\
\027\000"

let yydgoto = "\002\000\
\003\000\004\000\010\000\011\000\012\000\017\000\024\000\028\000\
\018\000\043\000\044\000\048\000\074\000\075\000"

let yysindex = "\006\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\233\254\016\255\000\000\094\255\251\254\
\244\254\253\254\000\000\013\255\094\255\000\000\041\255\094\255\
\000\000\000\000\056\255\029\255\054\255\038\255\044\255\203\000\
\047\255\000\000\000\000\209\254\000\000\203\000\000\000\000\000\
\203\000\203\000\000\000\152\255\203\000\203\000\249\255\069\255\
\203\000\203\000\203\000\163\255\060\255\000\000\000\000\203\000\
\203\000\203\000\000\000\203\000\203\000\203\000\203\000\203\000\
\203\000\203\000\203\000\203\000\219\255\228\255\000\000\073\255\
\249\255\053\255\051\255\249\255\000\000\000\000\076\000\124\255\
\124\255\002\000\058\000\076\000\124\255\124\255\200\254\200\254\
\000\000\000\000\127\255\127\255\203\000\000\000\203\000\104\255\
\000\000\238\255\249\255\127\255\203\000\000\000\064\255\127\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\074\255\000\000\
\000\000\077\255\000\000\000\000\000\000\000\000\000\000\066\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\087\255\
\000\000\000\000\000\000\139\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\247\254\000\000\
\087\255\078\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\211\254\000\000\079\255\249\254\000\000\000\000\250\254\143\000\
\148\000\254\254\135\255\049\000\168\000\173\000\067\000\089\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\097\255\
\000\000\000\000\255\254\000\000\081\255\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\082\000\000\000\051\000\000\000\000\000\080\000\
\000\000\041\000\224\255\211\255\000\000\000\000"

let yytablesize = 529
let yytable = "\047\000\
\009\000\050\000\013\000\072\000\054\000\052\000\001\000\054\000\
\054\000\055\000\039\000\051\000\069\000\070\000\067\000\068\000\
\047\000\073\000\076\000\030\000\019\000\049\000\039\000\079\000\
\080\000\081\000\046\000\082\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\030\000\020\000\031\000\032\000\
\030\000\033\000\049\000\039\000\014\000\049\000\039\000\046\000\
\055\000\021\000\046\000\055\000\034\000\035\000\036\000\103\000\
\037\000\039\000\039\000\039\000\098\000\046\000\099\000\022\000\
\015\000\016\000\025\000\030\000\047\000\031\000\032\000\023\000\
\033\000\020\000\027\000\020\000\020\000\038\000\020\000\039\000\
\040\000\029\000\014\000\034\000\035\000\036\000\045\000\037\000\
\041\000\020\000\020\000\020\000\046\000\020\000\005\000\049\000\
\006\000\071\000\042\000\007\000\008\000\093\000\094\000\095\000\
\025\000\026\000\025\000\025\000\038\000\025\000\039\000\078\000\
\100\000\104\000\020\000\029\000\020\000\020\000\053\000\041\000\
\025\000\025\000\025\000\009\000\025\000\020\000\010\000\052\000\
\053\000\042\000\029\000\096\000\097\000\000\000\030\000\020\000\
\031\000\032\000\000\000\033\000\102\000\000\000\000\000\000\000\
\105\000\025\000\000\000\025\000\025\000\000\000\034\000\035\000\
\036\000\000\000\037\000\034\000\025\000\034\000\000\000\034\000\
\000\000\000\000\000\000\045\000\000\000\000\000\025\000\034\000\
\056\000\000\000\057\000\000\000\058\000\000\000\000\000\038\000\
\000\000\039\000\000\000\056\000\059\000\057\000\000\000\058\000\
\045\000\000\000\041\000\045\000\034\000\000\000\000\000\034\000\
\065\000\066\000\067\000\068\000\042\000\000\000\045\000\045\000\
\000\000\000\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\034\000\000\000\077\000\000\000\000\000\060\000\
\061\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\000\000\000\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\056\000\000\000\057\000\000\000\058\000\
\000\000\000\000\000\000\000\000\056\000\000\000\057\000\000\000\
\058\000\000\000\000\000\000\000\000\000\000\000\056\000\000\000\
\057\000\005\000\058\000\006\000\000\000\000\000\007\000\008\000\
\000\000\056\000\101\000\057\000\091\000\058\000\000\000\000\000\
\000\000\000\000\056\000\000\000\057\000\092\000\058\000\000\000\
\000\000\000\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\000\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\000\000\000\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\040\000\061\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\056\000\000\000\057\000\040\000\058\000\000\000\
\000\000\000\000\000\000\035\000\000\000\035\000\000\000\035\000\
\000\000\000\000\000\000\000\000\000\000\000\000\057\000\035\000\
\058\000\000\000\040\000\000\000\000\000\040\000\000\000\000\000\
\000\000\036\000\000\000\036\000\000\000\036\000\000\000\000\000\
\040\000\040\000\040\000\000\000\035\000\036\000\000\000\035\000\
\000\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\000\000\036\000\000\000\000\000\036\000\063\000\064\000\
\065\000\066\000\067\000\068\000\000\000\000\000\000\000\000\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\041\000\
\000\000\041\000\000\000\041\000\043\000\000\000\043\000\000\000\
\043\000\000\000\000\000\041\000\000\000\000\000\000\000\000\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\042\000\000\000\042\000\000\000\042\000\044\000\000\000\044\000\
\041\000\044\000\000\000\041\000\042\000\043\000\000\000\000\000\
\043\000\044\000\000\000\000\000\000\000\000\000\041\000\041\000\
\041\000\041\000\041\000\043\000\043\000\043\000\043\000\043\000\
\000\000\042\000\000\000\000\000\042\000\000\000\044\000\000\000\
\000\000\044\000\034\000\035\000\036\000\000\000\037\000\042\000\
\042\000\042\000\042\000\042\000\044\000\044\000\044\000\044\000\
\044\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\038\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\041\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\042\000"

let yycheck = "\032\000\
\000\000\049\001\026\001\049\000\050\001\038\000\001\000\053\001\
\041\000\042\000\017\001\059\001\045\000\046\000\071\001\072\001\
\049\000\050\000\051\000\029\001\026\001\029\001\029\001\056\000\
\057\000\058\000\029\001\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\008\001\050\001\010\001\011\001\
\050\001\013\001\050\001\050\001\029\001\053\001\053\001\050\001\
\050\001\053\001\053\001\053\001\024\001\025\001\026\001\101\000\
\028\001\064\001\065\001\066\001\093\000\064\001\095\000\051\001\
\049\001\015\000\026\001\008\001\101\000\010\001\011\001\021\000\
\013\001\008\001\024\000\010\001\011\001\049\001\013\001\051\001\
\052\001\026\001\029\001\024\001\025\001\026\001\049\001\028\001\
\060\001\024\001\025\001\026\001\049\001\028\001\001\001\049\001\
\003\001\029\001\070\001\006\001\007\001\029\001\050\001\053\001\
\008\001\024\000\010\001\011\001\049\001\013\001\051\001\052\001\
\009\001\050\001\049\001\029\001\051\001\052\001\039\000\060\001\
\024\001\025\001\026\001\050\001\028\001\060\001\050\001\050\001\
\050\001\070\001\050\001\091\000\092\000\255\255\008\001\070\001\
\010\001\011\001\255\255\013\001\100\000\255\255\255\255\255\255\
\104\000\049\001\255\255\051\001\052\001\255\255\024\001\025\001\
\026\001\255\255\028\001\017\001\060\001\019\001\255\255\021\001\
\255\255\255\255\255\255\029\001\255\255\255\255\070\001\029\001\
\017\001\255\255\019\001\255\255\021\001\255\255\255\255\049\001\
\255\255\051\001\255\255\017\001\029\001\019\001\255\255\021\001\
\050\001\255\255\060\001\053\001\050\001\255\255\255\255\053\001\
\069\001\070\001\071\001\072\001\070\001\255\255\064\001\065\001\
\255\255\255\255\064\001\065\001\066\001\067\001\068\001\069\001\
\070\001\071\001\072\001\255\255\050\001\255\255\255\255\064\001\
\065\001\066\001\067\001\068\001\069\001\070\001\071\001\072\001\
\255\255\255\255\064\001\065\001\066\001\067\001\068\001\069\001\
\070\001\071\001\072\001\017\001\255\255\019\001\255\255\021\001\
\255\255\255\255\255\255\255\255\017\001\255\255\019\001\255\255\
\021\001\255\255\255\255\255\255\255\255\255\255\017\001\255\255\
\019\001\001\001\021\001\003\001\255\255\255\255\006\001\007\001\
\255\255\017\001\029\001\019\001\050\001\021\001\255\255\255\255\
\255\255\255\255\017\001\255\255\019\001\050\001\021\001\255\255\
\255\255\255\255\064\001\065\001\066\001\067\001\068\001\069\001\
\070\001\071\001\072\001\064\001\065\001\066\001\067\001\068\001\
\069\001\070\001\071\001\072\001\255\255\064\001\065\001\066\001\
\067\001\068\001\069\001\070\001\071\001\072\001\255\255\255\255\
\064\001\065\001\066\001\067\001\068\001\069\001\070\001\071\001\
\072\001\017\001\065\001\066\001\067\001\068\001\069\001\070\001\
\071\001\072\001\017\001\255\255\019\001\029\001\021\001\255\255\
\255\255\255\255\255\255\017\001\255\255\019\001\255\255\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\019\001\029\001\
\021\001\255\255\050\001\255\255\255\255\053\001\255\255\255\255\
\255\255\017\001\255\255\019\001\255\255\021\001\255\255\255\255\
\064\001\065\001\066\001\255\255\050\001\029\001\255\255\053\001\
\255\255\255\255\255\255\066\001\067\001\068\001\069\001\070\001\
\071\001\072\001\064\001\065\001\066\001\067\001\068\001\069\001\
\070\001\255\255\050\001\255\255\255\255\053\001\067\001\068\001\
\069\001\070\001\071\001\072\001\255\255\255\255\255\255\255\255\
\064\001\065\001\066\001\067\001\068\001\069\001\070\001\017\001\
\255\255\019\001\255\255\021\001\017\001\255\255\019\001\255\255\
\021\001\255\255\255\255\029\001\255\255\255\255\255\255\255\255\
\029\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\017\001\255\255\019\001\255\255\021\001\017\001\255\255\019\001\
\050\001\021\001\255\255\053\001\029\001\050\001\255\255\255\255\
\053\001\029\001\255\255\255\255\255\255\255\255\064\001\065\001\
\066\001\067\001\068\001\064\001\065\001\066\001\067\001\068\001\
\255\255\050\001\255\255\255\255\053\001\255\255\050\001\255\255\
\255\255\053\001\024\001\025\001\026\001\255\255\028\001\064\001\
\065\001\066\001\067\001\068\001\064\001\065\001\066\001\067\001\
\068\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\049\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\060\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\070\001"

let yynames_const = "\
  VOID\000\
  CHAR\000\
  INT\000\
  STRING\000\
  LONG\000\
  FLOAT\000\
  BOOL\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  RETURN\000\
  STRUCT\000\
  FOR\000\
  NULL\000\
  NEW\000\
  ARRAY\000\
  EQ\000\
  NE\000\
  LT\000\
  LE\000\
  GT\000\
  GE\000\
  EOF\000\
  NEWLINE\000\
  CALL\000\
  IFELSE\000\
  INITDECL\000\
  POS\000\
  NEG\000\
  NEWARRAY\000\
  TYPEID\000\
  FIELD\000\
  ORD\000\
  CHR\000\
  ROOT\000\
  NEWSTRING\000\
  RETURNVOID\000\
  INDEX\000\
  VARDECL\000\
  FUNCTION\000\
  PARAMLIST\000\
  PROTOTYPE\000\
  DECLID\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  MUL\000\
  ADD\000\
  SUB\000\
  MOD\000\
  DIV\000\
  ASSIGN\000\
  NOT\000\
  INDENT\000\
  DEDENT\000\
  "

let yynames_block = "\
  CHARCON\000\
  INTCON\000\
  FLOATCON\000\
  IDENTIFIER\000\
  STRINGCON\000\
  BOOLCON\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 47 "parser.mly"
            ( _1 )
# 430 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                 ( ([], [])               )
# 436 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 51 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 444 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 52 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 452 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                 ( ([], [])               )
# 458 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 56 "parser.mly"
               ( ((_2 :: fst _1), snd _1) )
# 466 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 57 "parser.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 474 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 61 "parser.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 489 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                  ( [] )
# 495 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 69 "parser.mly"
                  ( _1 )
# 502 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "parser.mly"
                                     ( [(_1,_2)]     )
# 510 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
                                     ( (_3,_4) :: _1 )
# 519 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
          ( Int   )
# 525 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
          ( Bool  )
# 531 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
          ( Float )
# 537 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
          ( Void  )
# 543 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                     ( [] )
# 549 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 83 "parser.mly"
                     ( _2 :: _1 )
# 557 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 86 "parser.mly"
                          ( (_1, _2) )
# 565 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
                   ( [] )
# 571 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 90 "parser.mly"
                   ( _2 :: _1 )
# 579 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                                               ( Expr _1               )
# 586 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 94 "parser.mly"
                                               ( Return _2             )
# 593 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 95 "parser.mly"
                                            ( Block(List.rev _2)    )
# 600 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 96 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 608 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 97 "parser.mly"
                                            ( If(_3, _5, _7)        )
# 617 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 99 "parser.mly"
                                            ( For(_3, _5, _7, _9)   )
# 627 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 100 "parser.mly"
                                            ( While(_3, _5)         )
# 635 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                  ( Noexpr )
# 641 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                  ( _1 )
# 648 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 107 "parser.mly"
              ( Literal(_1)            )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 108 "parser.mly"
                   ( Fliteral(_1)           )
# 662 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 109 "parser.mly"
                        ( BoolLit(_1)            )
# 669 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 110 "parser.mly"
                        ( Id(_1)                 )
# 676 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                      ( Binop(_1, Add,   _3)   )
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                      ( Binop(_1, Sub,   _3)   )
# 692 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                      ( Binop(_1, Mult,  _3)   )
# 700 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                      ( Binop(_1, Div,   _3)   )
# 708 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                      ( Binop(_1, Equal, _3)   )
# 716 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                      ( Binop(_1, Neq,   _3)   )
# 724 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                      ( Binop(_1, Less,  _3)   )
# 732 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                      ( Binop(_1, Leq,   _3)   )
# 740 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                      ( Binop(_1, Greater, _3) )
# 748 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                      ( Binop(_1, Geq,   _3)   )
# 756 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                      ( Binop(_1, And,   _3)   )
# 764 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                      ( Binop(_1, Or,    _3)   )
# 772 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                             ( Unop(Neg, _2)      )
# 779 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                          ( Unop(Not, _2)          )
# 786 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                                ( Assign(_1, _3)         )
# 794 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 126 "parser.mly"
                                       ( Call(_1, _3)  )
# 802 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                        ( _2                   )
# 809 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "parser.mly"
                  ( [] )
# 815 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 131 "parser.mly"
               ( List.rev _1 )
# 822 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "parser.mly"
                            ( [_1] )
# 829 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "parser.mly"
                         ( _3 :: _1 )
# 837 "parser.ml"
               : 'args_list))
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
;;

type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | DOT
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | VOID
  | STRUCT
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | FLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 4 "src/microcparse.mly"
open Ast
# 44 "src/microcparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* NOT *);
  269 (* EQ *);
  270 (* NEQ *);
  271 (* LT *);
  272 (* LEQ *);
  273 (* GT *);
  274 (* GEQ *);
  275 (* AND *);
  276 (* OR *);
  277 (* DOT *);
  278 (* RETURN *);
  279 (* IF *);
  280 (* ELSE *);
  281 (* FOR *);
  282 (* WHILE *);
  283 (* INT *);
  284 (* BOOL *);
  285 (* FLOAT *);
  286 (* VOID *);
  287 (* STRUCT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  288 (* LITERAL *);
  289 (* BLIT *);
  290 (* ID *);
  291 (* FLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\007\000\007\000\
\003\000\010\000\008\000\008\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\013\000\013\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\014\000\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\002\000\000\000\002\000\
\003\000\005\000\000\000\002\000\002\000\003\000\003\000\005\000\
\007\000\009\000\005\000\000\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\004\000\003\000\000\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\055\000\000\000\010\000\011\000\012\000\013\000\
\000\000\001\000\003\000\004\000\000\000\014\000\000\000\017\000\
\000\000\000\000\000\000\000\000\008\000\000\000\000\000\015\000\
\000\000\000\000\009\000\016\000\000\000\000\000\000\000\000\000\
\019\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\032\000\000\000\031\000\020\000\000\000\000\000\000\000\
\046\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\050\000\023\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\049\000\
\000\000\000\000\000\000\027\000\000\000\000\000\000\000\025\000\
\000\000\000\000\026\000"

let yydgoto = "\002\000\
\003\000\004\000\011\000\012\000\013\000\019\000\026\000\030\000\
\020\000\000\000\045\000\046\000\052\000\078\000\079\000"

let yysindex = "\005\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\240\254\000\000\000\000\000\000\242\254\000\000\046\255\000\000\
\149\255\248\254\055\255\038\255\000\000\057\255\149\255\000\000\
\036\255\149\255\000\000\000\000\044\255\041\255\071\255\007\255\
\000\000\000\000\007\255\007\255\007\255\079\255\084\255\087\255\
\000\000\000\000\006\255\000\000\000\000\174\255\098\000\075\255\
\000\000\000\000\148\000\098\255\007\255\007\255\007\255\007\255\
\007\255\000\000\007\255\007\255\007\255\007\255\007\255\007\255\
\007\255\007\255\007\255\007\255\007\255\007\255\000\000\000\000\
\000\000\116\000\103\255\134\000\148\000\108\255\111\255\148\000\
\081\255\081\255\000\000\000\000\187\000\187\000\086\255\086\255\
\086\255\086\255\175\000\162\000\119\255\007\255\119\255\000\000\
\007\255\096\255\194\255\000\000\148\000\119\255\007\255\000\000\
\121\255\119\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\122\255\000\000\000\000\125\255\000\000\000\000\000\000\000\000\
\000\000\080\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\128\255\000\000\000\000\000\000\
\000\000\000\000\154\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\255\000\000\000\000\128\255\000\000\127\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\008\255\000\000\129\255\053\255\
\214\255\234\255\000\000\000\000\080\000\084\000\254\255\020\000\
\040\000\060\000\049\255\255\255\000\000\000\000\000\000\000\000\
\000\000\114\255\000\000\000\000\054\255\000\000\130\255\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\108\000\000\000\246\255\000\000\000\000\102\000\
\000\000\000\000\245\000\224\255\204\255\000\000\000\000"

let yytablesize = 461
let yytable = "\047\000\
\010\000\075\000\049\000\050\000\051\000\001\000\018\000\056\000\
\032\000\029\000\053\000\029\000\025\000\053\000\035\000\029\000\
\057\000\014\000\036\000\015\000\074\000\051\000\076\000\077\000\
\080\000\021\000\081\000\082\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\091\000\092\000\041\000\042\000\
\043\000\044\000\032\000\023\000\033\000\034\000\016\000\017\000\
\035\000\044\000\105\000\044\000\036\000\048\000\044\000\048\000\
\054\000\022\000\048\000\054\000\024\000\099\000\037\000\038\000\
\101\000\039\000\040\000\044\000\044\000\027\000\051\000\016\000\
\041\000\042\000\043\000\044\000\032\000\031\000\033\000\072\000\
\053\000\019\000\035\000\019\000\019\000\054\000\036\000\019\000\
\055\000\061\000\062\000\019\000\059\000\060\000\061\000\062\000\
\037\000\038\000\073\000\039\000\040\000\019\000\019\000\094\000\
\019\000\019\000\041\000\042\000\043\000\044\000\096\000\019\000\
\019\000\019\000\019\000\024\000\097\000\024\000\024\000\102\000\
\032\000\024\000\033\000\106\000\006\000\024\000\035\000\007\000\
\028\000\051\000\036\000\052\000\028\000\028\000\048\000\024\000\
\024\000\000\000\024\000\024\000\037\000\038\000\000\000\039\000\
\040\000\024\000\024\000\024\000\024\000\000\000\041\000\042\000\
\043\000\044\000\033\000\000\000\033\000\000\000\000\000\033\000\
\033\000\033\000\033\000\033\000\000\000\000\000\033\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\058\000\005\000\
\006\000\007\000\008\000\009\000\059\000\060\000\061\000\062\000\
\000\000\000\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\103\000\000\000\000\000\000\000\000\000\000\000\
\059\000\060\000\061\000\062\000\000\000\000\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\034\000\000\000\
\034\000\000\000\000\000\034\000\034\000\034\000\000\000\000\000\
\000\000\000\000\034\000\034\000\034\000\034\000\034\000\034\000\
\034\000\034\000\035\000\000\000\035\000\000\000\000\000\035\000\
\035\000\035\000\000\000\000\000\000\000\000\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\040\000\045\000\
\040\000\045\000\000\000\040\000\045\000\000\000\000\000\000\000\
\000\000\000\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\045\000\000\000\041\000\000\000\041\000\000\000\
\000\000\041\000\000\000\005\000\006\000\007\000\008\000\009\000\
\041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\042\000\000\000\042\000\000\000\000\000\042\000\000\000\000\000\
\000\000\000\000\000\000\000\000\042\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\043\000\000\000\043\000\000\000\
\000\000\043\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
\038\000\098\000\038\000\100\000\039\000\038\000\039\000\000\000\
\000\000\039\000\104\000\000\000\038\000\038\000\107\000\000\000\
\039\000\039\000\038\000\038\000\071\000\000\000\039\000\039\000\
\059\000\060\000\061\000\062\000\000\000\000\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\093\000\000\000\
\000\000\000\000\059\000\060\000\061\000\062\000\000\000\000\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\095\000\000\000\000\000\000\000\059\000\060\000\061\000\062\000\
\000\000\000\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\059\000\060\000\061\000\062\000\000\000\000\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\070\000\
\059\000\060\000\061\000\062\000\000\000\000\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\059\000\060\000\061\000\
\062\000\000\000\000\000\063\000\064\000\065\000\066\000\067\000\
\068\000\059\000\060\000\061\000\062\000\000\000\000\000\000\000\
\000\000\065\000\066\000\067\000\068\000"

let yycheck = "\032\000\
\000\000\054\000\035\000\036\000\037\000\001\000\017\000\002\001\
\002\001\001\001\003\001\003\001\023\000\006\001\008\001\026\000\
\011\001\034\001\012\001\034\001\053\000\054\000\055\000\056\000\
\057\000\034\001\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\032\001\033\001\
\034\001\035\001\002\001\006\001\004\001\005\001\001\001\002\001\
\008\001\001\001\103\000\003\001\012\001\001\001\006\001\003\001\
\003\001\003\001\006\001\006\001\004\001\094\000\022\001\023\001\
\097\000\025\001\026\001\019\001\020\001\034\001\103\000\001\001\
\032\001\033\001\034\001\035\001\002\001\034\001\004\001\005\001\
\002\001\002\001\008\001\004\001\005\001\002\001\012\001\008\001\
\002\001\009\001\010\001\012\001\007\001\008\001\009\001\010\001\
\022\001\023\001\001\001\025\001\026\001\022\001\023\001\001\001\
\025\001\026\001\032\001\033\001\034\001\035\001\003\001\032\001\
\033\001\034\001\035\001\002\001\006\001\004\001\005\001\024\001\
\002\001\008\001\004\001\003\001\003\001\012\001\008\001\003\001\
\001\001\003\001\012\001\003\001\003\001\026\000\033\000\022\001\
\023\001\255\255\025\001\026\001\022\001\023\001\255\255\025\001\
\026\001\032\001\033\001\034\001\035\001\255\255\032\001\033\001\
\034\001\035\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\027\001\
\028\001\029\001\030\001\031\001\007\001\008\001\009\001\010\001\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\001\001\255\255\255\255\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\255\255\
\003\001\255\255\255\255\006\001\007\001\008\001\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\001\001\001\001\
\003\001\003\001\255\255\006\001\006\001\255\255\255\255\255\255\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\020\001\255\255\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\027\001\028\001\029\001\030\001\031\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\001\001\255\255\003\001\255\255\255\255\006\001\255\255\255\255\
\255\255\255\255\255\255\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\001\001\093\000\003\001\095\000\001\001\006\001\003\001\255\255\
\255\255\006\001\102\000\255\255\013\001\014\001\106\000\255\255\
\013\001\014\001\019\001\020\001\003\001\255\255\019\001\020\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\003\001\255\255\
\255\255\255\255\007\001\008\001\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\003\001\255\255\255\255\255\255\007\001\008\001\009\001\010\001\
\255\255\255\255\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\007\001\008\001\009\001\010\001\255\255\255\255\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\007\001\008\001\009\001\010\001\255\255\255\255\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\007\001\008\001\009\001\010\001\255\255\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  DOT\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  VOID\000\
  STRUCT\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  ID\000\
  FLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 32 "src/microcparse.mly"
            ( _1 )
# 331 "src/microcparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "src/microcparse.mly"
                 ( ([], [])               )
# 337 "src/microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 36 "src/microcparse.mly"
               ( ((_2 :: fst _1), snd _1) )
# 345 "src/microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 37 "src/microcparse.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 353 "src/microcparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 41 "src/microcparse.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 368 "src/microcparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "src/microcparse.mly"
                  ( [] )
# 374 "src/microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 49 "src/microcparse.mly"
                  ( _1 )
# 381 "src/microcparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "src/microcparse.mly"
                             ( [(_1,_2)]     )
# 389 "src/microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "src/microcparse.mly"
                             ( (_3,_4) :: _1 )
# 398 "src/microcparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "src/microcparse.mly"
          ( Int   )
# 404 "src/microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 57 "src/microcparse.mly"
          ( Bool  )
# 410 "src/microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "src/microcparse.mly"
          ( Float )
# 416 "src/microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "src/microcparse.mly"
          ( Void  )
# 422 "src/microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "src/microcparse.mly"
              ( Struct(_2) )
# 429 "src/microcparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "src/microcparse.mly"
                     ( [] )
# 435 "src/microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 64 "src/microcparse.mly"
                     ( _2 :: _1 )
# 443 "src/microcparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 67 "src/microcparse.mly"
               ( (_1, _2) )
# 451 "src/microcparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    Obj.repr(
# 71 "src/microcparse.mly"
  ( { sname = _2;
	 body = List.rev _4 } )
# 460 "src/microcparse.ml"
               : 'struct_defn))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "src/microcparse.mly"
                   ( [] )
# 466 "src/microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "src/microcparse.mly"
                   ( _2 :: _1 )
# 474 "src/microcparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "src/microcparse.mly"
                                            ( Expr _1               )
# 481 "src/microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 81 "src/microcparse.mly"
                                            ( Return _2             )
# 488 "src/microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 82 "src/microcparse.mly"
                                            ( Block(List.rev _2)    )
# 495 "src/microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "src/microcparse.mly"
                                            ( If(_3, _5, Block([])) )
# 503 "src/microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 84 "src/microcparse.mly"
                                            ( If(_3, _5, _7)        )
# 512 "src/microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 86 "src/microcparse.mly"
                                            ( For(_3, _5, _7, _9)   )
# 522 "src/microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 87 "src/microcparse.mly"
                                            ( While(_3, _5)         )
# 530 "src/microcparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "src/microcparse.mly"
                  ( Noexpr )
# 536 "src/microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "src/microcparse.mly"
                  ( _1 )
# 543 "src/microcparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 94 "src/microcparse.mly"
                     ( Literal(_1)            )
# 550 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 95 "src/microcparse.mly"
                    ( Fliteral(_1)           )
# 557 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 96 "src/microcparse.mly"
                     ( BoolLit(_1)            )
# 564 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 97 "src/microcparse.mly"
                     ( Id(_1)                 )
# 571 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "src/microcparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 579 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "src/microcparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 587 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "src/microcparse.mly"
                     ( Binop(_1, Mult,  _3)   )
# 595 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "src/microcparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 603 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "src/microcparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 611 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "src/microcparse.mly"
                     ( Binop(_1, Neq,   _3)   )
# 619 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "src/microcparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 627 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "src/microcparse.mly"
                     ( Binop(_1, Leq,   _3)   )
# 635 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "src/microcparse.mly"
                     ( Binop(_1, Greater, _3) )
# 643 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "src/microcparse.mly"
                     ( Binop(_1, Geq,   _3)   )
# 651 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "src/microcparse.mly"
                     ( Binop(_1, And,   _3)   )
# 659 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "src/microcparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 667 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "src/microcparse.mly"
                         ( Unop(Neg, _2)      )
# 674 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "src/microcparse.mly"
                     ( Unop(Not, _2)          )
# 681 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "src/microcparse.mly"
                     ( Assign(_1, _3)         )
# 689 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 113 "src/microcparse.mly"
                              ( Call(_1, _3)  )
# 697 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 114 "src/microcparse.mly"
                       ( _2                   )
# 704 "src/microcparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "src/microcparse.mly"
                  ( [] )
# 710 "src/microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 119 "src/microcparse.mly"
               ( List.rev _1 )
# 717 "src/microcparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "src/microcparse.mly"
                            ( [_1] )
# 724 "src/microcparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "src/microcparse.mly"
                         ( _3 :: _1 )
# 732 "src/microcparse.ml"
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

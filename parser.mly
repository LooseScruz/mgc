%{ open Ast %}


%token VOID CHAR INT STRING LONG FLOAT BOOL
%token IF ELSE WHILE RETURN STRUCT FOR
%token NULL NEW ARRAY
%token EQ NE LT LE GT GE
%token <char> CHARCON
%token <int> INTCON
%token <string> FLOATCON
%token <string> IDENTIFIER STRINGCON
%token <bool> BOOLCON
%token EOF

%token NEWLINE

%token CALL IFELSE INITDECL
%token POS NEG NEWARRAY TYPEID FIELD
%token ORD CHR ROOT

%token NEWSTRING RETURNVOID INDEX VARDECL FUNCTION
%token PARAMLIST PROTOTYPE DECLID
%token LPAREN RPAREN COMMA COLON

%token MUL ADD SUB MOD DIV
%token ASSIGN NOT

%token INDENT DEDENT

%token CONST

%nonassoc NOELSE
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

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }
 
decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
   typ IDENTIFIER LPAREN formals_opt RPAREN COLON INDENT vdecl_list stmt_list DEDENT
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 locals = List.rev $8;
	 body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ IDENTIFIER                   { [($1,$2)]     }
  | formal_list COMMA typ IDENTIFIER { ($3,$4) :: $1 }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | VOID  { Void  }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ IDENTIFIER { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr                                          { Expr $1               }
  | RETURN expr_opt                               { Return $2             }
  | INDENT stmt_list DEDENT                       { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN COLON stmt %prec NOELSE { If($3, $6, Block([])) }
  | IF LPAREN expr RPAREN COLON stmt ELSE COLON stmt    { If($3, $6, $9)        }
  | FOR LPAREN expr_opt expr expr_opt RPAREN COLON stmt
                                                  { For($3, $4, $5, $8)   }
  | WHILE LPAREN expr RPAREN COLON stmt           { While($3, $6)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    INTCON				    { Literal($1)            }
  | FLOATCON	     	  { Fliteral($1)           }
  | BOOLCON           { BoolLit($1)            }
  | IDENTIFIER        { Id($1)                 }
  | expr PLUS   expr 	{ Binop($1, Add,   $3)   }
  | expr MINUS  expr 	{ Binop($1, Sub,   $3)   }
  | expr TIMES  expr 	{ Binop($1, Mult,  $3)   }
  | expr DIVIDE expr 	{ Binop($1, Div,   $3)   }
  | expr EQ     expr 	{ Binop($1, Equal, $3)   }
  | expr NEQ    expr 	{ Binop($1, Neq,   $3)   }
  | expr LT     expr 	{ Binop($1, Less,  $3)   }
  | expr LEQ    expr 	{ Binop($1, Leq,   $3)   }
  | expr GT     expr 	{ Binop($1, Greater, $3) }
  | expr GEQ    expr 	{ Binop($1, Geq,   $3)   }
  | expr AND    expr 	{ Binop($1, And,   $3)   }
  | expr OR     expr 	{ Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT 				        { Unop(Neg, $2)      }
  | NOT expr         					          { Unop(Not, $2)          }
  | IDENTIFIER ASSIGN expr   			      { Assign($1, $3)         }
  | IDENTIFIER LPAREN args_opt RPAREN 	{ Call($1, $3)  }
  | LPAREN expr RPAREN 	{ $2                   }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }


%%

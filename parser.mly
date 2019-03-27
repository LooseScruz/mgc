%{ open Ast %}


%token VOID CHAR INT STRING LONG FLOAT
%token IF ELSE WHILE RETURN STRUCT
%token NULL NEW ARRAY
%token EQ NE LT LE GT GE
%token <char> CHARCON
%token <int> INTCON
%token <string> IDENT STRINGCON
%token <bool> BOOLCON
%token EOF

%token CALL IFELSE INITDECL
%token POS NEG NEWARRAY TYPEID FIELD
%token ORD CHR ROOT

%token NEWSTRING RETURNVOID INDEX VARDECL FUNCTION
%token PARAMLIST PROTOTYPE DECLID
%token LPAREN RPAREN LBRACE RBRACE COMMA

%token MUL ADD SUB MOD DIV
%token ASSIGN NOT

%right IFELSE IF ELSE
%right ASSIGN
%left EQ NE LT LE GT GE
%left ADD SUB
%left MUL DIV MOD
%right POS NEG NOT NEW

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }
 
%%

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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program

{ open Parser }

rule token = parse
  ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| "void"			{ TOK_VOID }
| "char"			{ TOK_CHAR }
| "int"				{ TOK_INT }
| "string"			{ TOK_STRING }
| "long"			{ TOK_LONG }
| "float"			{ TOK_FLOAT }
| "if"				{ TOK_IF }
| "else"			{ TOK_ELSE }
| "while"			{ TOK_WHILE }
| "return"			{ TOK_RETURN }
| "struct"			{ TOK_STRUCT }
| "null"			{ TOK_NULL }
| "new"				{ TOK_NEW }
(* Need array *)
| "=="				{ TOK_EQ }
| "!="				{ TOK_NEQ }
| '<'				{ TOK_LT }
| "<="				{ TOK_LE }
| '>'				{ TOK_GT }
| ">="				{ TOK_GE }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* { TOK_IDENT(lxm) }
| _					{ TOK_CHARCON }

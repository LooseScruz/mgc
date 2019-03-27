{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| "void"			{ VOID }
| "char"			{ CHAR }
| "int"				{ INT }
| "string"			{ STRING }
| "long"			{ LONG }
| "float"			{ FLOAT }
| "if"				{ IF }
| "else"			{ ELSE }
| "while"			{ WHILE }
| "return"			{ RETURN }
| "struct"			{ STRUCT }
| "null"			{ NULL }
| "new"				{ NEW }
| "[]"				{ ARRAY }
(* Need array *)
| "=="				{ EQ }
| "!="				{ NEQ }
| '<'				{ LT }
| "<="				{ LE }
| '>'				{ GT }
| ">="				{ GE }
| '='				{ ASSIGN }
| '+'				{ ADD }
| '-'				{ SUB }
| '/'				{ DIV }
| '*'				{ MUL }
| '%'				{ MOD }
| '('      			{ LPAREN }
| ')'      			{ RPAREN }
| '{'      			{ LBRACE }
| '}'      			{ RBRACE }
| ';'      			{ SEMI }
| ','      			{ COMMA }
| "true"   			{ BOOLCON(true)  }
| "false"  			{ BOOLCON(false) }
| digits as lxm 	{ LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* { IDENT(lxm) }
(*| _					{ CHARCON }*)
| _ as char 		{ raise (Failure("illegal character " ^ Char.escaped char)) }

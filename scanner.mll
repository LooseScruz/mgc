{
	open Parser
}

let digit = ['0' - '9']
let digits = digit+

let epsilon = ""


rule token tab_cnt = parse
  [' ' '\r'] { token tab_cnt lexbuf } (* Whitespace *)
| '\t'				{ token (Scanner_TC.incr_current_tab_count tab_cnt) lexbuf }
| '\n'				{ token tab_cnt lexbuf } (*{ NEWLINE }*)
| "void"			{ VOID }
| "char"			{ CHAR }
| "int"				{ INT }
| "string"			{ STRING }
| "long"			{ LONG }
| "bool"			{ BOOL }
| "float"			{ FLOAT }
| "if"				{ IF }
| "else"			{ ELSE }
| "while"			{ WHILE }
| "return"			{ RETURN }
| "struct"			{ STRUCT }
| "null"			{ NULL }
| "new"				{ NEW }
| "const"			{ CONST }
| "[]"				{ ARRAY }
| "=="				{ EQ }
| "!="				{ NE }
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
| '{'      			{ INDENT }
| '}'      			{ DEDENT }
| ','      			{ COMMA }
| ':'				{ COLON }
| "true"   			{ BOOLCON(true)  }
| "false"  			{ BOOLCON(false) }
| digits as lxm 	{ INTCON(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLOATCON(lxm) }
| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { IDENTIFIER(lxm) }
| eof { EOF }
| _ as char 		{ raise (Failure("illegal character " ^ Char.escaped char)) }

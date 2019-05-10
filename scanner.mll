{
	open Parser
	open Scanner_TC
}

let digit = ['0' - '9']
let digits = digit+

let epsilon = ""


rule token tab_cnt = parse
  [' ' '\r'] { token tab_cnt lexbuf } (* Whitespace *)
| '\t'				{ Scanner_TC.incr_current_tab_count tab_cnt; token tab_cnt lexbuf }
| '\n'				{ Scanner_TC.adv_tab_count tab_cnt; token tab_cnt lexbuf }
| epsilon 			{ let prev_tc = tab_cnt.last_tab_count in
					  let curr_tc = tab_cnt.current_tab_count in
					  if curr_tc < prev_tc
					  then (Scanner_TC.decr_prev_tab_count tab_cnt; DEDENT)
					  else if curr_tc > prev_tc
					  then (Scanner_TC.incr_prev_tab_count tab_cnt; INDENT)
					  else _token lexbuf }

and _token = parse
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
| "do"				{ DO }
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
| '+'				{ PLUS }
| '-'				{ MINUS }
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

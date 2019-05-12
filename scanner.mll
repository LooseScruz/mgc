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
(* | eof 				{  _endtokens tab_cnt lexbuf } *)
| epsilon 			{ let prev_tc = tab_cnt.last_tab_count in
					  let curr_tc = tab_cnt.current_tab_count in
					  if curr_tc < prev_tc
					  then (Scanner_TC.decr_prev_tab_count tab_cnt; DEDENT)
					  else if curr_tc > prev_tc
					  then (Scanner_TC.incr_prev_tab_count tab_cnt; INDENT)
					  else _token lexbuf }

and _endtokens tab_cnt = parse
| epsilon			{ let prev_tc = tab_cnt.last_tab_count in
						if prev_tc > 0
						then (Scanner_TC.decr_prev_tab_count tab_cnt); _endtokens tab_cnt lexbuf; DEDENT }

and _token = parse
| "void"			{ VOID }
| "char"			{ CHAR }
| "int"				{ INT }
| "string"			{ STRING }
| "long"			{ LONG }
| "bool"			{ BOOL }
| "float"			{ FLOAT }
| "double"			{ DOUBLE }
| "if"				{ IF }
| "else"			{ ELSE }
| "elif"			{ ELIF }
| "while"			{ WHILE }
| "do"				{ DO }
| "return"			{ RETURN }
| "struct"			{ STRUCT }
| "null"			{ NULL }
| "new"				{ NEW }
| "const"			{ CONST }
| "unsigned"		{ UNSIGNED }
| "[]"				{ ARRAY }
| "=="				{ EQ }
| "equals"			{ EQ }
| "is"				{ EQ }
| "!="				{ NEQ }
| "isnot"			{ NEQ }
| "isn't"			{ NEQ }
| '<'				{ LT }
| "<="				{ LE }
| '>'				{ GT }
| ">="				{ GE }
| '='				{ ASSIGN }
| ":="				{ CONSTASSIGN }
| '+'				{ PLUS }
| '-'				{ MINUS }
| '/'				{ DIVIDE }
| '*'				{ TIMES }
| '%'				{ MOD }
| '!'				{ NOT }
| "not"				{ NOT }
| "||"				{ OR }
| "or"				{ OR }
| "&&"				{ AND }
| "and"				{ AND }
| '|'				{ BITOR }
| "bitor"			{ BITOR }
| '&'				{ BITAND }
| "bitand"			{ BITAND }
| '^'				{ XOR }
| "xor"				{ XOR }
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
| eof				{ EOF }
| _ as char 		{ raise (Failure("illegal character " ^ Char.escaped char)) }

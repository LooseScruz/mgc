{ open Parser }

rule token = parse
  ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| 

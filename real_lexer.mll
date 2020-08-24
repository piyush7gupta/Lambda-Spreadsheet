{
  open Real_parser
  exception EOF
  exception Invalid_input
}
rule token =parse
  [' ' '\t' '\n']  {token lexbuf}
  | "SUM"   {SUM}
  | "COUNT" {COUNT}
  | "ROWCOUNT"  {ROWCOUNT}
  | "COLCOUNT"  {COLCOUNT}
  | "ROWSUM"    {ROWSUM}
  | "COLSUM"    {COLSUM}
  | "AVG"       {AVG}
  | "ROWAVG"    {ROWAVG}
  | "COLAVG"    {COLAVG}
  | "MIN"       {MIN}
  | "ROWMIN"    {ROWMIN}
  | "COLMIN"    {COLMIN}
  | "MAX"       {MAX}
  | "ROWMAX"    {ROWMAX}
  | "COLMAX"    {COLMAX}
  | "ADD"       {ADD}
  | "SUBT"      {SUBT}
  | "MULT"      {MULT}
  | "DIV"       {DIV}
  | (['+' '-']?) ( ('0')|(['1'-'9']['0'-'9']*) ) as lexm {INTEGER(int_of_string lexm)}
  | (['+' '-']?) ( ('0')|(['1'-'9']['0'-'9']*) ) (['.']) ( (['0'-'9']*['1'-'9'])| ['0']) as lexm {FLOAT(float_of_string lexm)}
  | '('         {L_PAREN}
  | ')'         {R_PAREN}
  | '['         {L_BRAC}
  | ']'         {R_BRAC}
  | ','         {COMMA}
  | ':'         {COLON}
  | ":="        {ASSIGNMENT}
  | ';'         {SEMICOLON}
  | eof         {raise EOF}
  | _           {raise Invalid_input}

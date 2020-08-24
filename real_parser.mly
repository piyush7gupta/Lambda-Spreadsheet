%{
  open Mainfile
  let rec yoyo (a:int) b : sheet =
  if(a=0) then [] else
  let line = input_line b in
    let split = String.split_on_char ',' in 
    let values = split line in
    (conversion2 values):: yoyo (a-1) b
;;

let m=int_of_string(Sys.argv.(2));;
let n=int_of_string(Sys.argv.(3));;
let yoboy : sheet =
  try let in_stream = open_in Sys.argv.(1) in
 let aa = (yoyo m in_stream) in 
  close_in in_stream;
  aa;
  with e -> raise e
  ;;
  
    let testm1 = yoboy;;
%}
%token <int> INTEGER 
%token <float> FLOAT 
%token ADD SUBT MULT DIV
%token COUNT COLCOUNT ROWCOUNT SUM COLSUM ROWSUM MIN COLMIN ROWMIN MAX COLMAX ROWMAX AVG COLAVG ROWAVG
%token L_PAREN R_PAREN L_BRAC R_BRAC
%token COMMA COLON ASSIGNMENT SEMICOLON
%start main             /* the entry point */
%type <Mainfile.sheet> main
%%
main:
     expr SEMICOLON                { $1 }
;
expr:
    i ASSIGNMENT COUNT r           {(full_count testm1 $4 $1) }
  | i ASSIGNMENT ROWCOUNT r        {(row_count testm1 $4 $1)}
  | i ASSIGNMENT COLCOUNT r        {(col_count testm1 $4 $1)}
  | i ASSIGNMENT AVG r             {(full_avg testm1 $4 $1)}
  | i ASSIGNMENT ROWAVG r          {(row_avg testm1 $4 $1)}
  | i ASSIGNMENT COLAVG r          {(col_avg testm1 $4 $1)}
  | i ASSIGNMENT SUM r             {(full_sum testm1 $4 $1)}
  | i ASSIGNMENT ROWSUM r          {(row_sum testm1 $4 $1)}
  | i ASSIGNMENT COLSUM r          {(col_sum testm1 $4 $1)}
  | i ASSIGNMENT MIN r             {(full_min testm1 $4 $1)}
  | i ASSIGNMENT ROWMIN r          {(row_min testm1 $4 $1)}
  | i ASSIGNMENT COLMIN r          {(col_min testm1 $4 $1)}
  | i ASSIGNMENT MAX r             {(full_max testm1 $4 $1)}
  | i ASSIGNMENT ROWMAX r          {(row_max testm1 $4 $1)}
  | i ASSIGNMENT COLMAX r          {(col_max testm1 $4 $1)}
  | i ASSIGNMENT ADD r r           {(add_range testm1 $4 $5 $1)}
  | i ASSIGNMENT ADD r i           {(add_const testm1 $4 (val1 testm1 $5 ) $1)}
  | i ASSIGNMENT ADD i r           {(add_const testm1 $5 (val1 testm1 $4 ) $1)}
  | i ASSIGNMENT ADD r FLOAT       {(add_const testm1 $4 $5 $1)}
  | i ASSIGNMENT ADD FLOAT r       {(add_const testm1 $5 $4 $1) }
  | i ASSIGNMENT SUBT r r          {(subt_range testm1 $4 $5 $1) }
  | i ASSIGNMENT SUBT r i          {(subt_const testm1 $4 (val1 testm1 $5 ) $1) }
  | i ASSIGNMENT SUBT i r          {(subt_const testm1 $5 (val1 testm1 $4 ) $1) }
  | i ASSIGNMENT SUBT r FLOAT      {(subt_const testm1 $4 $5 $1) }
  | i ASSIGNMENT SUBT FLOAT r      {(subt_const testm1 $5 $4 $1) }
  | i ASSIGNMENT MULT r r           {(mult_range testm1 $4 $5 $1) }
  | i ASSIGNMENT MULT r i           {(mult_const testm1 $4 (val1 testm1 $5 ) $1) }
  | i ASSIGNMENT MULT i r           {(mult_const testm1 $5 (val1 testm1 $4 ) $1) }
  | i ASSIGNMENT MULT r FLOAT       {(mult_const testm1 $4 $5 $1) }
  | i ASSIGNMENT MULT FLOAT r       {(mult_const testm1 $5 $4 $1) }
  | i ASSIGNMENT DIV r r           {(div_range testm1 $4 $5 $1) }
  | i ASSIGNMENT DIV r i           {(div_const testm1 $4 (val1 testm1 $5 ) $1) }
  | i ASSIGNMENT DIV i r           {(div_const testm1 $5 (val1 testm1 $4 ) $1) }
  | i ASSIGNMENT DIV r FLOAT       {(div_const testm1 $4 $5 $1) }
  | i ASSIGNMENT DIV FLOAT r       {(div_const testm1 $5 $4 $1) }
;

r:
    L_PAREN i COLON i R_PAREN   {(($2,$4))}
;
i:  
    L_BRAC INTEGER COMMA INTEGER R_BRAC       {(($2,$4))}
;  
%%



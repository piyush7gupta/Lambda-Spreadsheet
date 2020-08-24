
type indices =int*int;;
type range = indices*indices;;
type cell=Float of float| NULL ;;
type row =cell list;;
type sheet =cell list list;;


exception InvalidInput
exception Null_interpreted
exception Range_Incompatible


(* It convert the string to cell type *)
let conversion1 s : cell =
match s with 
"" -> NULL|
" " -> NULL|
a -> Float(float_of_string(a))
;;

(*  It convert the string list to cell list types*)
let rec conversion2 s : cell list =
  match s with
  [] ->[]|
  x::xs-> (conversion1 x):: (conversion2 xs)
  ;;

(* It prints the cell *)
let print_cell =function
NULL -> print_string "NULL";
| Float (f) -> print_float f;
;;

(* It prints the cell list*)
let rec print_list = function 
[] -> ()
| e::l -> print_cell e ; print_string " " ; print_list l;
;;

(* It prints the float list*)
let rec print_list1 = function 
[] -> ()
| e::l -> print_float e ; print_string " " ; print_list1 l;
;;

(* It prints the string list*)
let rec print_list2 = function 
[] -> ()
| e::l -> print_string "A";print_string e ; print_string " " ; print_list2 l;
;;

(* It prints the cell list list*)
let rec print_matrix = function 
[] -> ()
| e::l -> print_list e ; print_string "\n" ; print_matrix l;
;;

(* It prints the float list list*)
let rec print_matrix1 = function 
[] -> ()
| e::l -> print_list1 e ; print_string "\n" ; print_matrix1 l;
;;

(* Bastic list function self implemented*)
let rec map f l = match l with
[]->[]|
x::xs-> (f x) :: (map f xs);;

let head v = match v with
[] -> print_string "piyush\n"; raise InvalidInput |
x::xs -> x;;

let tail v = match v with
[] -> print_string "gupta\n";raise InvalidInput |
x::xs -> xs;;

(* this gives the first integer of index*)
let index_first (i1:indices) : int =
  match i1 with
  (a,b) -> a;;

(* this gives the second integer of index*)
let index_second (i1:indices) : int =
    match i1 with
    (a,b) -> b;; 

(* this gives the first index of range*)    
let range_first (r1:range) : indices=
  match r1 with
  (a,b) ->a;;

(* this gives the second index of range*)   
let range_second (r1:range) : indices=
    match r1 with
    (a,b) ->b;;

(* This gives the element at the index m of list *)
let rec get_elementv v1 m =
	if(m=1) then
	head v1
	else
	get_elementv (tail v1) (m-1)
;;

(* This gives the element at the index m n of matrix *)
let rec get_element (m1:sheet) (m:int) (n:int) : cell=
	if(m=1) then
	get_elementv (head m1) n
	else
	get_element (tail m1) (m-1) n
  ;;

(* This three function are for determining the size of matrix *)
  let add_first (a,b) (c,d) =(a +c,b);;

  let rec vdim v: int =match v with
  [] -> 0|
  x ::xs->  1 +vdim xs;;

  let rec mdim m: indices = match m with
	[] -> (0,0) |
	x :: xs -> add_first (1 ,vdim x) (mdim xs)

  
  
  
 

let rec mkrow_index (c1 : cell list) (size:int) (index:int) (count:float) :row=
if(size=0) then []
else
if (index=0) then ( Float(count) )::  mkrow_index (tail c1) (size-1) (index-1) count
else (head c1) :: mkrow_index (tail c1) (size-1) (index-1) count ;;


let rec  mksheet_index (s1:sheet) (i1:indices) (size :indices) (count:float) : sheet=
  if( (index_first size ) =0) then []
  else
  if( (index_first i1) =0 ) then (mkrow_index (head s1) (index_second size) (index_second i1) count ) :: (  mksheet_index (tail s1) ( ((index_first i1)-1), (index_second i1) )  ( ((index_first size)-1), (index_second size) ) count )
  else (head s1) :: (  mksheet_index (tail s1) ( ((index_first i1)-1), (index_second i1) )  ( ((index_first size)-1), (index_second size) ) count )
  ;;

 

let rec mksheet_row (s1:sheet) (i1:indices) (n:int) (size:indices) (temp:float list): sheet=
    if((index_first size) =0) then [] else
    if( (vdim temp ) =0 ) then (head s1) :: (mksheet_row (tail s1) i1 (n+1)  ( ((index_first size)-1), (index_second size) ) temp ) else
    if(n < (index_first i1))  then 
    
  (head s1) :: (mksheet_row (tail s1) i1 (n+1)  ( ((index_first size)-1), (index_second size) ) temp )
else begin  (mkrow_index (head s1) (index_second size) (index_second i1) (head temp) ) :: (mksheet_row (tail s1) i1 (n+1)  ( ((index_first size)-1), (index_second size) ) (tail temp) ) ; end

let rec mkrow_row (c1:cell list) (size:int) (i1:indices) (n:int) (temp:float list) : row=
  if(size=0) then []
  else 
  if( (vdim temp ) =0 ) then (head c1) :: ( mkrow_row (tail c1) (size-1) i1 (n+1) temp)
  else
  if(n< (index_second i1) ) then (head c1) :: ( mkrow_row (tail c1) (size-1) i1 (n+1) temp)
  else ( Float( head temp) ) :: ( mkrow_row (tail c1) (size-1) i1 (n+1) (tail temp));;

  let rec mksheet_col (s1:sheet) (i1:indices) (n:int) (size:indices) (temp:float list): sheet=
    if((index_first size) =0) then begin  [] end else
  if(n=(index_first i1))  then begin   (mkrow_row (head s1) (index_second size) i1 0 temp  ) ::  (mksheet_col (tail s1) i1 (n+1) ( ((index_first size)-1), (index_second size) ) temp ) end
    else (head s1) :: ( mksheet_col (tail s1) i1 (n+1) ( ((index_first size)-1), (index_second size) ) temp );;


 let rec row_count_helper (s1:sheet) (r1:range) : (float list)=
  if ( ( index_first (range_first r1) ) > ( index_first (range_second r1) ) ) then []
  else
  let count =ref 0.0 in
    for j = ( index_second (range_first r1) ) to ( index_second (range_second r1) ) do
      if get_element s1  (( index_first (range_first r1))+1 ) (j+1) = NULL then count := !count +. 0.0
      else count := !count +. 1.0;
      done;
      !count ::   ( row_count_helper s1  ( ( (( index_first (range_first r1))+1 ),( index_second (range_first r1) ) ) , (range_second r1) ) )

 let rec col_count_helper (s1:sheet) (r1:range) : (float list)=
  if ( ( index_second (range_first r1) ) > ( index_second (range_second r1) ) ) then []
  else
  let count =ref 0.0 in
    for j = ( index_first (range_first r1) ) to ( index_first (range_second r1) ) do
      if get_element s1 (j+1) (( index_second (range_first r1))+1 )  = NULL then count := !count +. 0.0
      else count := !count +. 1.0;
      done;
      !count ::   ( col_count_helper s1  ( ( (( index_first (range_first r1)) ),(( index_second (range_first r1) )+1) ) , (range_second r1) ) )

  
 
  let full_count (s1:sheet) (r1:range) (i1:indices) : sheet=
  let count =ref 0.0 in
    for i = ( index_first (range_first r1) )to ( index_first (range_second r1) ) do
      for j = ( index_second (range_first r1) ) to ( index_second (range_second r1) ) do
        
        if get_element s1 (i+1) (j+1) = NULL then count := !count +. 0.0
        else count := !count +. 1.0;
        done;
  done;
    ( mksheet_index s1 i1 (mdim s1) !count);;



  let row_count (s1:sheet) (r1:range) (i1:indices) : sheet=
   (mksheet_row s1 i1 0 (mdim s1) (row_count_helper s1 r1));;
    
  let col_count (s1:sheet) (r1:range) (i1:indices) : sheet=
   
    (mksheet_col s1 i1 0 (mdim s1) (col_count_helper s1 r1));;

    let rec row_sum_helper (s1:sheet) (r1:range) : (float list)=
      if ( ( index_first (range_first r1) ) > ( index_first (range_second r1) ) ) then []
      else
      let sum =ref 0.0 in
        for j = ( index_second (range_first r1) ) to ( index_second (range_second r1) ) do
          match get_element s1 (( index_first (range_first r1))+1 ) (j+1) with
          NULL -> raise Null_interpreted|
          Float(f) -> sum := !sum +. f ; 
          done;
        !sum ::   ( row_sum_helper s1  ( ( (( index_first (range_first r1))+1 ),( index_second (range_first r1) ) ) , (range_second r1) ) )
    
        let rec col_sum_helper (s1:sheet) (r1:range) : (float list)=
          if ( ( index_second (range_first r1) ) > ( index_second (range_second r1) ) ) then []
          else
          let sum =ref 0.0 in
            for j = ( index_first (range_first r1) ) to ( index_first (range_second r1) ) do
             match get_element s1  (j+1) (( index_second (range_first r1))+1 ) with
             NULL -> sum := raise Null_interpreted |
             Float(f) -> sum := !sum +. f ; 
             done;
              !sum ::   ( col_sum_helper s1  ( ( (( index_first (range_first r1)) ),(( index_second (range_first r1) )+1) ) , (range_second r1) ) )
        

let full_sum (s1:sheet) (r1:range) (i1:indices) : sheet=
  let sum =ref 0.0 in
  for i = ( index_first (range_first r1) )to ( index_first (range_second r1) ) do
    for j = ( index_second (range_first r1) ) to ( index_second (range_second r1) ) do
      match get_element s1 (i+1) (j+1) with
      NULL -> sum := raise Null_interpreted |
      Float(f) -> sum := !sum +. f
      done;
  done;
  ( mksheet_index s1 i1 (mdim s1) !sum);;


let row_sum (s1:sheet) (r1:range) (i1:indices) : sheet=
  (mksheet_row s1 i1 0 (mdim s1) (row_sum_helper s1 r1));;
let col_sum (s1:sheet) (r1:range) (i1:indices) : sheet=
  (mksheet_col s1 i1 0 (mdim s1) (col_sum_helper s1 r1));;

let full_avg (s1:sheet) (r1:range) (i1:indices) : sheet=
  let sum =ref 0.0 in
  let count =ref 0.0 in
  for i = ( index_first (range_first r1) )to ( index_first (range_second r1) ) do
    for j = ( index_second (range_first r1) ) to ( index_second (range_second r1) ) do
      match get_element s1 (i+1) (j+1) with
      NULL -> raise Null_interpreted|
      Float(f) -> sum := !sum +. f;
      count:=!count +. 1.0
      done;
  done;
  ( mksheet_index s1 i1 (mdim s1) (!sum /. !count));;

let rec row_avg_helper (s1:sheet) (r1:range) : (float list)=
  if ( ( index_first (range_first r1) ) > ( index_first (range_second r1) ) ) then []
  else
  let sum =ref 0.0 in
  let count =ref 0.0 in
    for j = ( index_second (range_first r1) ) to ( index_second (range_second r1) ) do
      match get_element s1  (( index_first (range_first r1))+1 ) (j+1) with
      NULL -> raise Null_interpreted|
    Float(f) -> sum := !sum +. f;
    count:=!count +. 1.0
      done;
      (!sum /. !count) ::   ( row_avg_helper s1  ( ( (( index_first (range_first r1))+1 ),( index_second (range_first r1) ) ) , (range_second r1) ) )

let rec col_avg_helper (s1:sheet) (r1:range) : (float list)=
  if ( ( index_second (range_first r1) ) > ( index_second (range_second r1) ) ) then []
  else
  let sum =ref 0.0 in
  let count =ref 0.0 in
    for j = ( index_first (range_first r1) ) to ( index_first (range_second r1) ) do
      match get_element s1  (j+1) (( index_second (range_first r1))+1 ) with
      NULL -> sum := raise Null_interpreted |
    Float(f) -> sum := !sum +. f; count:=!count +. 1.0
      done;
      (!sum /. !count) ::   ( col_avg_helper s1  ( ( (( index_first (range_first r1)) ),(( index_second (range_first r1) )+1) ) , (range_second r1) ) )



let row_avg (s1:sheet) (r1:range) (i1:indices) : sheet=
  (mksheet_row s1 i1 0 (mdim s1) (row_avg_helper s1 r1));;

let col_avg (s1:sheet) (r1:range) (i1:indices) : sheet=
  (mksheet_col s1 i1 0 (mdim s1) (col_avg_helper s1 r1));;

let full_min (s1:sheet) (r1:range) (i1:indices) : sheet=
  let min =ref 10000.0 in
  for i = ( index_first (range_first r1) )to ( index_first (range_second r1) ) do
    for j = ( index_second (range_first r1) ) to ( index_second (range_second r1) ) do
      match get_element s1 (i+1) (j+1) with
      NULL ->raise Null_interpreted |
      Float(f) -> if(f< !min) then min:=f;
      done;
  done;
  ( mksheet_index s1 i1 (mdim s1) !min);;

let rec row_min_helper (s1:sheet) (r1:range) : (float list)=
  if ( ( index_first (range_first r1) ) > ( index_first (range_second r1) ) ) then []
  else
  let min =ref 10000.0 in
    for j = ( index_second (range_first r1) ) to ( index_second (range_second r1) ) do
      match get_element s1 (( index_first (range_first r1))+1 ) (j+1) with
      NULL -> raise Null_interpreted |
    Float(f) -> if(f< !min) then min:=f;
    done;
    !min ::   ( row_min_helper s1  ( ( (( index_first (range_first r1))+1 ),( index_second (range_first r1) ) ) , (range_second r1) ) )

let rec col_min_helper (s1:sheet) (r1:range) : (float list)=
  if ( ( index_second (range_first r1) ) > ( index_second (range_second r1) ) ) then []
  else
  let min =ref 10000.0 in
    for j = ( index_first (range_first r1) ) to ( index_first (range_second r1) ) do
      match get_element s1  (j+1) (( index_second (range_first r1))+1 ) with
      NULL -> raise Null_interpreted |
      Float(f) -> if(f< !min) then min:=f;
      done;
      !min ::   ( col_min_helper s1  ( ( (( index_first (range_first r1)) ),(( index_second (range_first r1) )+1) ) , (range_second r1) ) )


let row_min (s1:sheet) (r1:range) (i1:indices) : sheet=(mksheet_row s1 i1 0 (mdim s1) (row_min_helper s1 r1));;
let col_min (s1:sheet) (r1:range) (i1:indices) : sheet=(mksheet_col s1 i1 0 (mdim s1) (col_min_helper s1 r1));;


let full_max (s1:sheet) (r1:range) (i1:indices) : sheet=
  let max =ref (-10000.0)  in
  for i = ( index_first (range_first r1) )to ( index_first (range_second r1) ) do
    for j = ( index_second (range_first r1) ) to ( index_second (range_second r1) ) do
      match get_element s1 (i+1) (j+1) with
      NULL -> raise Null_interpreted |
      Float(f) -> if(f> !max) then max:=f;
      done;
  done;
  ( mksheet_index s1 i1 (mdim s1) !max);;

let rec row_max_helper (s1:sheet) (r1:range) : (float list)=
  if ( ( index_first (range_first r1) ) > ( index_first (range_second r1) ) ) then []
  else
  let max =ref (-10000.0) in
    for j = ( index_second (range_first r1) ) to ( index_second (range_second r1) ) do
      match get_element s1  (( index_first (range_first r1))+1 ) (j+1) with
      NULL -> raise Null_interpreted |
    Float(f) -> if(f> !max) then max:=f;
    done;
    !max ::   ( row_max_helper s1  ( ( (( index_first (range_first r1))+1 ),( index_second (range_first r1) ) ) , (range_second r1) ) )

let rec col_max_helper (s1:sheet) (r1:range) : (float list)=
  if ( ( index_second (range_first r1) ) > ( index_second (range_second r1) ) ) then []
  else
  let max =ref (-10000.0) in
    for j = ( index_first (range_first r1) ) to ( index_first (range_second r1) ) do
      match get_element s1  (j+1) (( index_second (range_first r1))+1 ) with
      NULL -> raise Null_interpreted|
      Float(f) -> if(f> !max) then max:=f;
      done;
      !max ::   ( col_max_helper s1  ( ( (( index_first (range_first r1)) ),(( index_second (range_first r1) )+1) ) , (range_second r1) ) )



let row_max (s1:sheet) (r1:range) (i1:indices) : sheet=(mksheet_row s1 i1 0 (mdim s1) (row_max_helper s1 r1));;
let col_max (s1:sheet) (r1:range) (i1:indices) : sheet=(mksheet_col s1 i1 0 (mdim s1) (col_max_helper s1 r1));;

let rec add_const_helper (c1:row) (left:int) (right:int) (n:int) (a:float) : (float list) =
  if((vdim c1)=0) then []
  else if(n>right || n<left ) then
  add_const_helper (tail c1) left right (n+1) a
  else
  match (head c1) with
  NULL->  raise Null_interpreted |
  Float f->(a+.f):: (add_const_helper (tail c1) left right (n+1) a)
  ;;
  let rec sub_const_helper (c1:row) (left:int) (right:int) (n:int) (a:float) : (float list) =
    if((vdim c1)=0) then []
    else if(n>right || n<left ) then
    sub_const_helper (tail c1) left right (n+1) a
    else
    match (head c1) with
    NULL-> raise Null_interpreted |
    Float f->(f-.a):: (sub_const_helper (tail c1) left right (n+1) a)
    ;;

let rec add_const_helper2 (s1:sheet) (r1:range) (a:float) (n:int) : (float list list) =
  if(n> (index_first (range_second r1))) then []
  else
  if(n < (index_first(range_first r1))) then 
  add_const_helper2 (tail s1) r1 a (n+1)
  else 
  (add_const_helper (head s1) (index_second (range_first r1)) (index_second (range_second r1)) 0 a) :: (add_const_helper2 (tail s1) r1 a (n+1))
  ;;

 

let rec sub_const_helper2 (s1:sheet) (r1:range) (a:float) (n:int) : (float list list) =
  if(n> (index_first (range_second r1))) then []
  else
  if(n < (index_first(range_first r1))) then 
  sub_const_helper2 (tail s1) r1 a (n+1)
  else 
  (sub_const_helper (head s1) (index_second (range_first r1)) (index_second (range_second r1)) 0 a) :: (sub_const_helper2 (tail s1) r1 a (n+1))
  ;;

let rec mul_const_helper (c1:row) (left:int) (right:int) (n:int) (a:float) : (float list) =
  if((vdim c1)=0) then []
  else if(n>right || n<left ) then
  mul_const_helper (tail c1) left right (n+1) a
  else
  match (head c1) with
  NULL-> raise Null_interpreted |
  Float f->(a*.f):: (mul_const_helper (tail c1) left right (n+1) a)
  ;;

let rec mul_const_helper2 (s1:sheet) (r1:range) (a:float) (n:int) : (float list list) =
  if(n> (index_first (range_second r1))) then []
  else
  if(n < (index_first(range_first r1))) then 
  mul_const_helper2 (tail s1) r1 a (n+1)
  else 
  (mul_const_helper (head s1) (index_second (range_first r1)) (index_second (range_second r1)) 0 a) :: (mul_const_helper2 (tail s1) r1 a (n+1))
  ;;

let rec div_const_helper (c1:row) (left:int) (right:int) (n:int) (a:float) : (float list) =
  if((vdim c1)=0) then []
  else if(n>right || n<left ) then
  div_const_helper (tail c1) left right (n+1) a
  else
  match (head c1) with
  NULL-> raise Null_interpreted |
  Float f->(f/.a):: (div_const_helper (tail c1) left right (n+1) a)
  ;;

let rec div_const_helper2 (s1:sheet) (r1:range) (a:float) (n:int) : (float list list) =
  if(n> (index_first (range_second r1))) then []
  else
  if(n < (index_first(range_first r1))) then 
  div_const_helper2 (tail s1) r1 a (n+1)
  else 
  (div_const_helper (head s1) (index_second (range_first r1)) (index_second (range_second r1)) 0 a) :: (div_const_helper2 (tail s1) r1 a (n+1))
        ;;

let rec mksheet_sheet (s1:sheet) (i1:indices) (s2:float list list) (n:int)  : sheet=
  if((vdim s1)=0) then []
  else if ((vdim s2)=0) then (head s1) :: mksheet_sheet (tail s1) i1 s2 (n+1)
  else
  if(n< (index_first i1) ) then (head s1) :: mksheet_sheet (tail s1) i1 s2 (n+1)
  else (mkrow_row (head s1) (vdim (head s1)) (n,(index_second i1)) 0 (head s2) ) :: mksheet_sheet (tail s1) i1 (tail s2) (n+1)




let add_const (s1:sheet) (r1:range) (a:float) (i1:indices) : sheet=( mksheet_sheet s1 i1 (add_const_helper2 s1 r1 a 0) 0);;
let subt_const (s1:sheet) (r1:range) (a:float) (i1:indices) : sheet=( mksheet_sheet s1 i1 (sub_const_helper2 s1 r1 a 0) 0);;
let mult_const (s1:sheet) (r1:range) (a:float) (i1:indices) : sheet=( mksheet_sheet s1 i1 (mul_const_helper2 s1 r1 a 0) 0);;
let div_const (s1:sheet) (r1:range) (a:float) (i1:indices) : sheet=( mksheet_sheet s1 i1 (div_const_helper2 s1 r1 a 0) 0);;

let myval (c1:cell) : float=
  match c1 with 
  NULL ->raise Null_interpreted|
  Float (f) -> f;;

let rec add_range_helper (c1:row) (left:int) (right:int) (n:int) (c2:row) (left2:int) (right2:int) : (float list) =
  if(right-left != right2-left2) then raise Range_Incompatible else
  if((vdim c1)=0) then []
  else if(n>right || n<left ) then
  add_range_helper (tail c1) left right (n+1) c2 left2 right2
  else
  (myval (head c1) +. (myval (get_elementv c2 (n-left+left2+1))) ):: (add_range_helper (tail c1) left right (n+1) c2 left2 right2)
  ;;

let rec add_range_helper2 (s1:sheet) (r1:range) (r2:range) (n:int) (s2:sheet) : (float list list) =
  if( ((index_first (range_first r1)) - (index_first (range_second r1))) != ((index_first (range_first r2)) - (index_first (range_second r2)))) then  raise Range_Incompatible   else
  if(n> (index_first (range_second r1))) then []
  else
  if(n < (index_first(range_first r1))) then 
  add_range_helper2 (tail s1) r1 r2 (n+1) s2
  else 
  (add_range_helper (head s1) (index_second (range_first r1)) (index_second (range_second r1)) 0 (get_elementv s2 (n-(index_first(range_first r1))+ (index_first(range_first r2)) +1 ))  (index_second (range_first r2)) (index_second (range_second r2))) :: (add_range_helper2 (tail s1) r1 r2 (n+1) s2)
  ;; 

  let rec sub_range_helper (c1:row) (left:int) (right:int) (n:int) (c2:row) (left2:int) (right2:int) : (float list) =
    if(right-left != right2-left2) then raise Range_Incompatible else
    if((vdim c1)=0) then []
    else if(n>right || n<left ) then
    sub_range_helper (tail c1) left right (n+1) c2 left2 right2
    else
    (myval (head c1) -. (myval (get_elementv c2 (n-left+left2+1))) ):: (sub_range_helper (tail c1) left right (n+1) c2 left2 right2)
    ;;
  
  let rec sub_range_helper2 (s1:sheet) (r1:range) (r2:range) (n:int) (s2:sheet) : (float list list) =
    if( ((index_first (range_first r1)) - (index_first (range_second r1))) != ((index_first (range_first r2)) - (index_first (range_second r2)))) then  raise Range_Incompatible   else
    if(n> (index_first (range_second r1))) then []
    else
    if(n < (index_first(range_first r1))) then 
    sub_range_helper2 (tail s1) r1 r2 (n+1) s2
    else 
    (sub_range_helper (head s1) (index_second (range_first r1)) (index_second (range_second r1)) 0 (get_elementv s2 (n-(index_first(range_first r1))+ (index_first(range_first r2)) +1 ))  (index_second (range_first r2)) (index_second (range_second r2))) :: (sub_range_helper2 (tail s1) r1 r2 (n+1) s2)
    ;; 
    let rec mul_range_helper (c1:row) (left:int) (right:int) (n:int) (c2:row) (left2:int) (right2:int) : (float list) =
      if(right-left != right2-left2) then raise Range_Incompatible else
      if((vdim c1)=0) then []
      else if(n>right || n<left ) then
      mul_range_helper (tail c1) left right (n+1) c2 left2 right2
      else
      (myval (head c1) *. (myval (get_elementv c2 (n-left+left2+1))) ):: (mul_range_helper (tail c1) left right (n+1) c2 left2 right2)
      ;;
    
    let rec mul_range_helper2 (s1:sheet) (r1:range) (r2:range) (n:int) (s2:sheet) : (float list list) =
      if( ((index_first (range_first r1)) - (index_first (range_second r1))) != ((index_first (range_first r2)) - (index_first (range_second r2)))) then  raise Range_Incompatible   else
      if(n> (index_first (range_second r1))) then []
      else
      if(n < (index_first(range_first r1))) then 
      mul_range_helper2 (tail s1) r1 r2 (n+1) s2
      else 
      (mul_range_helper (head s1) (index_second (range_first r1)) (index_second (range_second r1)) 0 (get_elementv s2 (n-(index_first(range_first r1))+ (index_first(range_first r2)) +1 ))  (index_second (range_first r2)) (index_second (range_second r2))) :: (mul_range_helper2 (tail s1) r1 r2 (n+1) s2)
      ;; 
      let rec div_range_helper (c1:row) (left:int) (right:int) (n:int) (c2:row) (left2:int) (right2:int) : (float list) =
        if(right-left != right2-left2) then raise Range_Incompatible else
        if((vdim c1)=0) then []
        else if(n>right || n<left ) then
        div_range_helper (tail c1) left right (n+1) c2 left2 right2
        else
        (myval (head c1) /. (myval (get_elementv c2 (n-left+left2+1))) ):: (div_range_helper (tail c1) left right (n+1) c2 left2 right2)
        ;;
      
      let rec div_range_helper2 (s1:sheet) (r1:range) (r2:range) (n:int) (s2:sheet) : (float list list) =
        if( ((index_first (range_first r1)) - (index_first (range_second r1))) != ((index_first (range_first r2)) - (index_first (range_second r2)))) then  raise Range_Incompatible   else
        if(n> (index_first (range_second r1))) then []
        else
        if(n < (index_first(range_first r1))) then 
        div_range_helper2 (tail s1) r1 r2 (n+1) s2
        else 
        (div_range_helper (head s1) (index_second (range_first r1)) (index_second (range_second r1)) 0 (get_elementv s2 (n-(index_first(range_first r1))+ (index_first(range_first r2)) +1 ))  (index_second (range_first r2)) (index_second (range_second r2))) :: (div_range_helper2 (tail s1) r1 r2 (n+1) s2)
        ;; 


let add_range (s1:sheet) (r1:range) (r2:range) (i1:indices) : sheet=( mksheet_sheet s1 i1 (add_range_helper2 s1 r1 r2 0 s1) 0);;
let subt_range (s1:sheet) (r1:range) (r2:range) (i1:indices) : sheet=( mksheet_sheet s1 i1 (sub_range_helper2 s1 r1 r2 0 s1) 0);;
let mult_range (s1:sheet) (r1:range) (r2:range) (i1:indices) : sheet=( mksheet_sheet s1 i1 (mul_range_helper2 s1 r1 r2 0 s1) 0);;
let div_range (s1:sheet) (r1:range) (r2:range) (i1:indices) : sheet=( mksheet_sheet s1 i1 (div_range_helper2 s1 r1 r2 0 s1) 0);;

let val1 (s1:sheet) (i1:indices) : float =
      match get_element s1 ((index_first i1)+1)  ((index_second i1)+1)  with
      NULL -> raise Null_interpreted|
      Float(f) -> f;;
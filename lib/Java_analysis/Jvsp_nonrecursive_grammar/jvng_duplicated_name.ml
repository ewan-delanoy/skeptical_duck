(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_duplicated_name.ml";;

*)


type t = PDN of string ;;

module Private = struct 

let veil str = ((PDN str): t);;

let unveil (PDN str: t) =str ;;

let is_a_digit c= let i = int_of_char c in (48<=i)&&(i<=57) ;;

let is_a_digit_or_dot c = (is_a_digit c) || (c='.') ;; 

let is_not_a_digit_or_dot c = not(is_a_digit_or_dot c) ;;

let core str =
   match String_find_char.backwards_from_inclusive_opt is_not_a_digit_or_dot str (String.length str) with 
   None -> ""
   |Some k -> Cull_string.beginning k str ;;

let make str k = 
   let suffix = (if k=1 then "" else string_of_int k) in 
   veil ((core str)^suffix);;


let name pdn =
   let str = unveil pdn in 
   match String_find_char.backwards_from_inclusive_opt is_not_a_digit_or_dot str (String.length str) with 
   None -> ""
   |Some k -> Cull_string.beginning k str ;;

(*

name (veil "Gabriel234") ;;
name (veil "Gabriel") ;;

*)

let index pdn =
   let str = unveil pdn in 
   match String_find_char.backwards_from_inclusive_opt is_not_a_digit_or_dot str (String.length str) with 
   None -> int_of_string str
   |Some k -> 
      if k=String.length str 
      then 1   
      else int_of_string(Cull_string.cobeginning k str) ;; 

(*

index (veil "Gabriel234") ;;
index (veil "Gabriel") ;;

*)      

let order pdn1 pdn2 = Total_ordering.standard (unveil pdn1) (unveil pdn2) ;; 

end ;;

let make = Private.make ;;
let index = Private.index ;;
let name = Private.name ;;
let order = (Private.order : t Total_ordering_t.t) ;;
let of_string str = make str 1 ;;
let starts_with pdn ~prefix = String.starts_with (name pdn) ~prefix ;;


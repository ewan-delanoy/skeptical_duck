(*

#use"lib/Java_analysis/jvsp_token_types_list.ml";;

*)

type t = Jvsp_types.token_type_list =  TTL of (Jvsp_types.token_type list) ;;



module Private = struct 

let to_string (TTL l) = 
   Strung.with_size_limit ~size_limit:250
   (String.concat " " (Image.image Jvsp_util.summary_of_token_type l)) ;;

let rec find_and_forget_opt f l = match l with 
 [] -> None 
 | x :: others ->
    if f x 
   then Some l
   else find_and_forget_opt f others ;;

let rec helper_for_finding_and_remembering f (accu,l) = match l with 
 [] -> None
 | x :: others ->
    if f x 
   then Some(List.rev accu,x)
   else helper_for_finding_and_remembering f (x::accu,others) ;;

let find_and_remember_opt f  l = helper_for_finding_and_remembering f ([],l) ;;

end ;;    


let construct l =(TTL l) ;;

let find_and_forget_opt f (TTL l) = Option.map (fun z->TTL z)(Private.find_and_forget_opt f l);;

let find_and_remember_opt f (TTL l) = Private.find_and_remember_opt f l;;

let long_tail k (TTL l)= TTL(List_again.long_tail k l);;
  
(* This is a registered printer : print_out *)
let print_out (fmt:Format.formatter) ttl=
   Format.fprintf fmt "@[%s@]" (Private.to_string ttl);;

let starts_with (TTL l) prefix = List_again.starts_with l prefix;;

let to_string = Private.to_string ;;

let unveil (TTL l)= l;;     
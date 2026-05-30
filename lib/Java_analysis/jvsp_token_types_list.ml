(*

#use"lib/Java_analysis/jvsp_token_types_list.ml";;

*)

type t = Jvsp_types.token_type_list =  TTL of (Jvsp_types.token_type list) ;;

exception Find_opt_exn ;;

module Private = struct 

let to_string (TTL l) = 
   Strung.with_size_limit ~size_limit:250
   (String.concat " " (Image.image Jvsp_util.summary_of_token_type l)) ;;

let rec find_opt f l = match l with 
 [] -> raise Find_opt_exn 
 | x :: others ->
    if f x 
   then l
   else find_opt f others ;;

end ;;    


let construct l =(TTL l) ;;

let find_opt f (TTL l) = TTL(Private.find_opt f l);;


let long_tail k (TTL l)= TTL(List_again.long_tail k l);;
  
(* This is a registered printer : print_out *)
let print_out (fmt:Format.formatter) ttl=
   Format.fprintf fmt "@[%s@]" (Private.to_string ttl);;

let starts_with (TTL l) prefix = List_again.starts_with l prefix;;

let to_string = Private.to_string ;;

let unveil (TTL l)= l;;     
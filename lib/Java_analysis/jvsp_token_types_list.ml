(*

#use"lib/Java_analysis/jvsp_token_types_list.ml";;

*)

type t = TTL of (Jvsp_types.token_type list) ;;

module Private = struct 

let to_string (TTL l) = String.concat " " (Image.image Jvsp_util.summary_of_token_type l) ;;

end ;;    


let construct l =(TTL l) ;;

let find_opt f (TTL l) = List.find_opt f l ;;

let long_tail k (TTL l)= TTL(List_again.long_tail k l);;
  
(* This is a registered printer : print_out *)
let print_out (fmt:Format.formatter) ttl=
   Format.fprintf fmt "@[%s@]" (Strung.with_size_limit ~size_limit:250 (Private.to_string ttl));;

let unveil (TTL l)= l;;     
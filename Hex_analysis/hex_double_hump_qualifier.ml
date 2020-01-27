(* 

#use"Hex_analysis/hex_double_hump_qualifier.ml";;

*)

module Private = struct 

let to_readable_string = function 
     Hex_double_hump_qualifier_t.Big_followed_by_small -> "bs"
    |Small_followed_by_big -> "sb" ;;    

end ;; 

let print_out (fmt:Format.formatter) dh=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string dh);;     


let to_int = function 
    (* the correspondance is arbitrary, it just needs to be bijective. *)
     Hex_double_hump_qualifier_t.Big_followed_by_small -> 1
    |Small_followed_by_big -> 2 ;;

let to_readable_string = Private.to_readable_string;;  
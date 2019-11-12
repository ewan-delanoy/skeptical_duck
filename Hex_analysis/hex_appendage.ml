(* 

#use"Hex_analysis/hex_appendage.ml";;


*)

exception Bad_parameters of 
    Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;

module Private = struct 
   
let is_vertical = function 
    Hex_cardinal_direction_t.Down 
   |Hex_cardinal_direction_t.Up -> true 
   | _ -> false ;;

let pair_is_bad (direction1,direction2) =
   (is_vertical direction1)=(is_vertical direction2);;

let opt_of_string untrimmed_s =
    let s= Cull_string.trim_spaces untrimmed_s in 
    if String.length(s)<>3 then None else 
    let opt1 = Hex_cardinal_direction.opt_of_char (String.get s 0)
    and opt2 = Hex_cardinal_direction.opt_of_char (String.get s 2)
    in 
    if (opt1=None)||(opt2=None) then None else 
    let dir1 = Option.unpack opt1 
    and dir2 = Option.unpack opt2 in 
    if pair_is_bad (dir1,dir2)
    then raise(Bad_parameters(dir1,dir2))
    else Some(Hex_appendage_t.Eyed_claw(dir1,dir2));;


end ;;

let opt_of_string = Private.opt_of_string ;; 
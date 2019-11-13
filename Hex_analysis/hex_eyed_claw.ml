(* 

#use"Hex_analysis/hex_eyed_claw.ml";;


*)

exception Bad_parameters of 
    Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;
exception Bad_params_in_powder of 
    Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;    

module Private = struct 

let pair_is_bad (direction1,direction2) =
   (Hex_cardinal_direction.is_vertical direction1)=
   (Hex_cardinal_direction.is_vertical direction2);;

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
    else Some(Hex_eyed_claw_t.C(dir1,dir2));;


let powder_for_downwards_claw  = function 
    Hex_cardinal_direction_t.Left -> Hex_ipair.powder_for_left_eyed_downwards_claw
   |Hex_cardinal_direction_t.Right -> Hex_ipair.powder_for_right_eyed_downwards_claw 
   | other -> raise(Bad_params_in_powder(other,Hex_cardinal_direction_t.Down)) ;;


let powder_for_upwards_claw  = function 
    Hex_cardinal_direction_t.Left -> Hex_ipair.powder_for_left_eyed_downwards_claw
   |Hex_cardinal_direction_t.Right -> Hex_ipair.powder_for_right_eyed_downwards_claw 
   | other -> raise(Bad_params_in_powder(other,Hex_cardinal_direction_t.Up)) ;;




end ;;

let opt_of_string = Private.opt_of_string ;; 
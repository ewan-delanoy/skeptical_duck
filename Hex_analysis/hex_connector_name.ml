(* 

#use"Hex_analysis/hex_connector_name.ml";;

*)

(*
let base_for_starters =
*)   

let is_inner = function
    Hex_connector_name_t.Bridge(_)-> true
   |Eyed_claw(_,_,_) 
   |Noneyed_claw(_,_,_) 
   |Pyramid(_,_) 
   |Small_pyramid(_,_) -> false ;;

let to_readable_string = function 
    Hex_connector_name_t.Bridge(us)-> Hex_unit_side.to_readable_string us
   |Eyed_claw(_,d1,d2) -> 
         (Hex_cardinal_direction.for_eye_description d1)^"e"^(Hex_cardinal_direction.for_ground_description d2)
   |Noneyed_claw(_,dh,d) -> let s = (Hex_double_hump_qualifier.to_readable_string dh) in 
                          (String.sub s 0 1)^(Hex_cardinal_direction.for_ground_description d)^(String.sub s 1 1)
   |Pyramid(_,d) ->  (Hex_cardinal_direction.for_ground_description d)^"py" 
   |Small_pyramid(_,d) ->  (Hex_cardinal_direction.for_ground_description d)^"sy" ;;
(* 

#use"Hex_analysis/hex_border_connector_name.ml";;

*)



let to_readable_string = function 
    Hex_border_connector_name_t.Eyed_claw(d1,d2) -> 
         (Hex_cardinal_direction.for_eye_description d1)^"e"^(Hex_cardinal_direction.for_ground_description d2)
   |Noneyed_claw(dh,d) -> let s = (Hex_double_hump_qualifier.to_readable_string dh) in 
                          (String.sub s 0 1)^(Hex_cardinal_direction.for_ground_description d)^(String.sub s 1 1)
   |Pyramid(d) ->  (Hex_cardinal_direction.for_ground_description d)^"py" 
   |Small_pyramid(d) ->  (Hex_cardinal_direction.for_ground_description d)^"sy" ;;
   
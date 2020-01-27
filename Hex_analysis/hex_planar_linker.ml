(* 

#use"Hex_analysis/hex_planar_linker.ml";;

Static constructors for Hex_planar_linker_data_t.t objects 

*)

let check dim pllk cell = match pllk with 
    Hex_planar_linker_t.Eyed_claw(d1,d2) -> Hex_planar_linker_data.check_eyed_claw dim d1 d2 cell  
   |Noneyed_claw(double_hump,d) -> Hex_planar_linker_data.check_noneyed_claw dim double_hump d cell
   |Pyramid(d) -> Hex_planar_linker_data.check_pyramid dim d cell ;;


let level = function 
    Hex_planar_linker_t.Eyed_claw(d1,d2) -> 4*((Hex_cardinal_direction.to_int d1)-1)+(Hex_cardinal_direction.to_int d2)  
   |Noneyed_claw(dh,d) ->16+4*((Hex_double_hump_qualifier.to_int dh)-1)+(Hex_cardinal_direction.to_int d)
   |Pyramid(d) -> 24 + (Hex_cardinal_direction.to_int d) ;;


let support pllk cell = match pllk with 
    Hex_planar_linker_t.Eyed_claw(d1,d2) -> Hex_planar_linker_data.support_for_eyed_claw d1 d2 cell  
   |Noneyed_claw(double_hump,d) -> Hex_planar_linker_data.support_for_noneyed_claw double_hump d cell
   |Pyramid(d) -> Hex_planar_linker_data.support_for_pyramid d cell ;;



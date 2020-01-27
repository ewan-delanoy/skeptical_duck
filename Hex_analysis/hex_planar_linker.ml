(* 

#use"Hex_analysis/hex_planar_linker.ml";;

Static constructors for Hex_planar_linker_data_t.t objects 

*)

let support pllk cell = match pllk with 
    Hex_planar_linker_t.Eyed_claw(d1,d2) -> Hex_planar_linker_data.support_for_eyed_claw d1 d2 cell  
   |Noneyed_claw(double_hump,d) -> Hex_planar_linker_data.support_for_noneyed_claw double_hump d cell
   |Pyramid(d) -> Hex_planar_linker_data.support_for_pyramid d cell ;;



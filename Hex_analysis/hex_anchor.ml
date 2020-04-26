(* 

#use"Hex_analysis/hex_anchor.ml";;



*)

let to_readable_string = function 
     Hex_anchor_t.No_anchor -> ""
    |Single_anchor (d) -> "<"^(Hex_cardinal_direction.for_ground_description d)^">"
    |Double_anchor (d1,d2) -> 
       "<"^(Hex_cardinal_direction.for_ground_description d1)^
           (Hex_cardinal_direction.for_ground_description d2)^">"
     ;;
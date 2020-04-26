(* 

#use"Hex_analysis/hex_anchor.ml";;



*)

module Private = struct 

let normalized_double_anchor = function 
    Hex_cardinal_direction_t.Down 
   |Hex_cardinal_direction_t.Up -> Hex_anchor_t.Double_anchor (Hex_cardinal_direction_t.Down,Hex_cardinal_direction_t.Up)
   | _ -> Hex_anchor_t.Double_anchor (Hex_cardinal_direction_t.Left,Hex_cardinal_direction_t.Right) ;;  

end ;;

let any_side = function 
     Hex_anchor_t.No_anchor -> None
    |Single_anchor (d) -> Some d 
    |Double_anchor (d1,d2) -> Some d1 ;;

let oppose = function 
     Hex_anchor_t.No_anchor -> Hex_anchor_t.No_anchor
    |Single_anchor (d) -> Hex_anchor_t.Single_anchor (Hex_cardinal_direction.oppose d)
    |Double_anchor (d1,d2) -> Hex_anchor_t.Double_anchor (d1,d2) ;;

let reflect = function 
     Hex_anchor_t.No_anchor -> Hex_anchor_t.No_anchor
    |Single_anchor (d) -> Hex_anchor_t.Single_anchor (Hex_cardinal_direction.reflect d)
    |Double_anchor (d1,d2) -> Private.normalized_double_anchor (Hex_cardinal_direction.reflect d1) ;;

let to_readable_string = function 
     Hex_anchor_t.No_anchor -> ""
    |Single_anchor (d) -> "<"^(Hex_cardinal_direction.for_ground_description d)^">"
    |Double_anchor (d1,d2) -> 
       "<"^(Hex_cardinal_direction.for_ground_description d1)^
           (Hex_cardinal_direction.for_ground_description d2)^">"
     ;;
(* 

#use"Hex_analysis/hex_anchor.ml";;



*)


exception Unique_side_exn of Hex_anchor_t.t;; 

module Private = struct 

let normalized_double_anchor = function 
    Hex_cardinal_direction_t.Down 
   |Hex_cardinal_direction_t.Up -> Hex_anchor_t.Double_anchor (Hex_cardinal_direction_t.Down,Hex_cardinal_direction_t.Up)
   | _ -> Hex_anchor_t.Double_anchor (Hex_cardinal_direction_t.Left,Hex_cardinal_direction_t.Right) ;;  

let neighbors_for_side dim direction = 
   let temp1=Hex_cardinal_direction.Border.enumerate_all dim direction in 
   let temp2=Image.image Hex_cell.to_int_pair temp1 in 
   Set_of_poly_pairs.safe_set temp2;;

let of_list l= 
    match l with 
    []->Hex_anchor_t.No_anchor
    |side::_->
        if List.length(l)=1
        then Hex_anchor_t.Single_anchor side 
        else normalized_double_anchor side ;;
     
let to_list = function 
     Hex_anchor_t.No_anchor -> []
    |Single_anchor (d) -> [d]
    |Double_anchor (d1,d2) -> [d1;d2] ;;

end ;;

let any_side = function 
     Hex_anchor_t.No_anchor -> None
    |Single_anchor (d) -> Some d 
    |Double_anchor (d1,d2) -> Some d1 ;;


let merge l=
   let temp1 = List.flatten(Image.image Private.to_list l) in 
   let temp2 = Ordered.sort Total_ordering.standard temp1 in 
   Private.of_list temp2 ;;

let neighbors dim  = function 
     Hex_anchor_t.No_anchor -> Set_of_poly_pairs.empty_set
    |Single_anchor (d) -> Private.neighbors_for_side dim d 
    |Double_anchor (d1,d2) -> Set_of_poly_pairs.merge 
                (Private.neighbors_for_side dim d1) (Private.neighbors_for_side dim d2);;  

let of_list = Private.of_list ;;      


let oppose = function 
     Hex_anchor_t.No_anchor -> Hex_anchor_t.No_anchor
    |Single_anchor (d) -> Hex_anchor_t.Single_anchor (Hex_cardinal_direction.oppose d)
    |Double_anchor (d1,d2) -> Hex_anchor_t.Double_anchor (d1,d2) ;;

let reflect = function 
     Hex_anchor_t.No_anchor -> Hex_anchor_t.No_anchor
    |Single_anchor (d) -> Hex_anchor_t.Single_anchor (Hex_cardinal_direction.reflect d)
    |Double_anchor (d1,d2) -> Private.normalized_double_anchor (Hex_cardinal_direction.reflect d1) ;;

let touches_side anchor side = match anchor with 
   Hex_anchor_t.No_anchor -> false
    |Single_anchor (d) -> d=side
    |Double_anchor (d1,d2) -> (d1=side) || (d2=side) ;;
    
let touches_cell dim anchor cell = match anchor with 
   Hex_anchor_t.No_anchor -> false
    |Single_anchor (d) -> Hex_cardinal_direction.Border.test dim d cell 
    |Double_anchor (d1,d2) -> 
          ( Hex_cardinal_direction.Border.test dim d1 cell) ||
          ( Hex_cardinal_direction.Border.test dim d2 cell)
     ;;    


let to_readable_string = function 
     Hex_anchor_t.No_anchor -> ""
    |Single_anchor (d) -> "<"^(Hex_cardinal_direction.for_ground_description d)^">"
    |Double_anchor (d1,d2) -> 
       "<"^(Hex_cardinal_direction.for_ground_description d1)^
           (Hex_cardinal_direction.for_ground_description d2)^">"
     ;;

let unique_side anchor = match anchor with 
     Hex_anchor_t.Single_anchor (d) -> d 
    |_ -> raise(Unique_side_exn(anchor)) ;;     
(* 
#use"Hex_analysis/hex_atomic_linker.ml";;

*)

exception Degenerate_pair of Hex_cell_t.t;;
exception Incorrect_abscissa of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;
exception Incorrect_ordinate of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;
exception Mismatch of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;
exception Support_mismatch of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;
exception Of_concrete_object_exn;;

module Private = struct 

let down = Hex_cardinal_direction_t.Down;;
let left = Hex_cardinal_direction_t.Left;;
let right = Hex_cardinal_direction_t.Right;;
let up = Hex_cardinal_direction_t.Up;;
let high = up and low = down;;

(* Constructors *)

let pair (cell1,cell2) = 
   match Hex_cell.cmp cell1 cell2 with 
    Total_ordering.Lower -> Hex_atomic_linker_t.Pair(cell1,cell2)
   |Total_ordering.Equal -> raise(Degenerate_pair(cell1))
   |Total_ordering.Greater -> Hex_atomic_linker_t.Pair(cell2,cell1);;


let left_eyed_downwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if x<>dim-3         then raise(Incorrect_abscissa(left,down)) else 
    if (y<5)||(y>dim-2) then raise(Incorrect_ordinate(left,down)) else
    Hex_atomic_linker_t.Eyed_claw(left,down,cell);;

let right_eyed_downwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if x<>dim-3         then raise(Incorrect_abscissa(right,down)) else 
    if (y<6)||(y>dim-1) then raise(Incorrect_ordinate(right,down)) else
    Hex_atomic_linker_t.Eyed_claw(right,down,cell);;    


let left_eyed_upwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if x<>4         then raise(Incorrect_abscissa(left,up)) else 
    if (y<2)||(y>dim-5) then raise(Incorrect_ordinate(left,up)) else
    Hex_atomic_linker_t.Eyed_claw(left,up,cell);;

let right_eyed_upwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if x<>4         then raise(Incorrect_abscissa(right,up)) else 
    if (y<3)||(y>dim-4) then raise(Incorrect_ordinate(right,up)) else
    Hex_atomic_linker_t.Eyed_claw(right,up,cell);;    

let high_eyed_leftwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if  y<>4            then raise(Incorrect_ordinate(high,left)) else
    if (x<2)||(x>dim-5) then raise(Incorrect_abscissa(high,left)) else 
    Hex_atomic_linker_t.Eyed_claw(high,left,cell);;

let low_eyed_leftwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if  y<>4            then raise(Incorrect_ordinate(low,left)) else
    if (x<3)||(x>dim-4) then raise(Incorrect_abscissa(low,left)) else 
    Hex_atomic_linker_t.Eyed_claw(low,left,cell);;    


let high_eyed_rightwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if  y<>dim-3        then raise(Incorrect_ordinate(high,left)) else
    if (x<5)||(x>dim-2) then raise(Incorrect_abscissa(high,left)) else 
    Hex_atomic_linker_t.Eyed_claw(high,right,cell);;

let low_eyed_rightwards (Hex_dimension_t.D dim) cell=
    let x=Hex_cell.abscissa cell 
    and y=Hex_cell.ordinate cell in 
    if  y<>dim-3        then raise(Incorrect_ordinate(low,left)) else
    if (x<6)||(x>dim-1) then raise(Incorrect_abscissa(low,left)) else 
    Hex_atomic_linker_t.Eyed_claw(low,right,cell);;    


let left_eyed dim direction cell = match direction with 
     Hex_cardinal_direction_t.Down -> left_eyed_downwards dim cell 
    |Hex_cardinal_direction_t.Up   -> left_eyed_upwards dim cell    
    |                           _  -> raise(Mismatch(left,direction));;

let right_eyed dim direction cell = match direction with 
     Hex_cardinal_direction_t.Down -> right_eyed_downwards dim cell 
    |Hex_cardinal_direction_t.Up   -> right_eyed_upwards dim cell    
    |                           _  -> raise(Mismatch(right,direction));;

let high_eyed dim direction cell = match direction with 
     Hex_cardinal_direction_t.Left  -> high_eyed_leftwards dim cell 
    |Hex_cardinal_direction_t.Right -> high_eyed_rightwards dim cell    
    |                           _   -> raise(Mismatch(high,direction));;

let low_eyed dim direction cell = match direction with 
     Hex_cardinal_direction_t.Left  -> low_eyed_leftwards dim cell 
    |Hex_cardinal_direction_t.Right -> low_eyed_rightwards dim cell    
    |                           _   -> raise(Mismatch(low,direction));;

let eyed dim =function
   Hex_cardinal_direction_t.Down  -> low_eyed dim 
  |Hex_cardinal_direction_t.Left  -> left_eyed dim
  |Hex_cardinal_direction_t.Right -> right_eyed dim
  |Hex_cardinal_direction_t.Up    -> high_eyed dim ;;  

let translated_support_map f cell=
   let ipair=Hex_ipair.of_cell cell in 
   Hex_cell_set.safe_set (Image.image Hex_ipair.to_cell (f ipair)) ;; 


let support_for_eyed direction1 direction2 =  translated_support_map (Hex_ipair.ipair_powder_for_eyed_claw direction1 direction2);;

let salt = "Hex_"^"atomic_linker_t.";;
let pair_label = salt ^ "Pair";;
let eyed_label = salt ^ "Eyed_claw";;

let to_concrete_object = function 
   Hex_atomic_linker_t.Pair (cell1,cell2) ->
       Concrete_object_t.Variant (pair_label,Image.image Hex_cell.to_concrete_object [cell1;cell2])
 | Eyed_claw (dir1,dir2,cell) -> 
       Concrete_object_t.Variant (eyed_label,
          (Image.image Hex_cardinal_direction.to_concrete_object [dir1;dir2])@[Hex_cell.to_concrete_object cell])
 ;;

let of_concrete_object crobj=
   let (lab,(arg1,arg2,arg3,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
   if lab=eyed_label
   then Hex_atomic_linker_t.Eyed_claw(Hex_cardinal_direction.of_concrete_object arg1,
                                      Hex_cardinal_direction.of_concrete_object arg2,
                                      Hex_cell.of_concrete_object arg3)
   else 
   if lab=pair_label
   then Hex_atomic_linker_t.Pair(Hex_cell.of_concrete_object arg1,
                                 Hex_cell.of_concrete_object arg2)
   else raise(Of_concrete_object_exn);;                                   

let cmp_for_pair (cell1,cell2) = function 
   Hex_atomic_linker_t.Pair (cell3,cell4) -> Hex_cell.cmp_for_pairs (cell1,cell2) (cell3,cell4)
 | Eyed_claw (direction1,direction2,cell) -> Total_ordering.Lower ;;

let cmp_for_eyed (direction1,direction2,cell) = function 
   Hex_atomic_linker_t.Pair (_,_) ->  Total_ordering.Greater
 | Eyed_claw (direction3,direction4,cell2) ->  
     Total_ordering.triple_product 
        Total_ordering.standard Total_ordering.standard Hex_cell.cmp 
          (direction1,direction2,cell) (direction1,direction2,cell2)
 ;;

let cmp = ((function 
   Hex_atomic_linker_t.Pair (cell1,cell2) -> cmp_for_pair (cell1,cell2)
 | Eyed_claw (direction1,direction2,cell) -> cmp_for_eyed (direction1,direction2,cell)):>
    Hex_atomic_linker_t.t Total_ordering.t
 ) ;;  


let support = function 
   Hex_atomic_linker_t.Pair (cell1,cell2) -> Hex_cell_set.safe_set [cell1;cell2]
 | Eyed_claw (direction1,direction2,cell) -> support_for_eyed direction1 direction2 cell ;;
    
let ipair_support mlclr = 
   let (Hex_cell_set_t.S l)=support mlclr in 
   Image.image Hex_ipair.of_cell l;;


let opt_eyed dim (p,untrimmed_s) =
    let s= Cull_string.trim_spaces untrimmed_s in 
    if String.length(s)<>3 then None else 
    let opt1 = Hex_cardinal_direction.opt_of_char (String.get s 0)
    and opt2 = Hex_cardinal_direction.opt_of_char (String.get s 2)
    in 
    if (opt1=None)||(opt2=None) then None else 
    try Some (eyed dim (Option.unpack opt1) (Option.unpack opt2) (Hex_ipair.to_cell p)) with 
    _->None;;

let to_readable_string = function
  Hex_atomic_linker_t.Pair (cell1,cell2) -> 
    (Hex_cell.to_string cell1)^"-"^(Hex_cell.to_string cell2) 
 | Eyed_claw (direction1,direction2,cell) -> 
     let l=support_for_eyed direction1 direction2 cell in 
     (Hex_cardinal_direction.short_name_for_pair (direction1,direction2))^
     "("^(Hex_cell_set.to_string l)^")";;

(*

Note : the function below does not take into account the case where an 
eyed_claw strategy is progressively fulfilled by several moves.

*)

let nonfulfilment_by_ally_move cell = function 
    Hex_atomic_linker_t.Pair (cell1,cell2) -> (cell <> cell1) || (cell <> cell2) 
   | Eyed_claw (_,_,_) -> true;; 

let active_complement = function 
   Hex_atomic_linker_t.Pair (_,_) -> Hex_cell_set_t.S [] 
   | Eyed_claw (_,_,cell) -> Hex_cell_set_t.S [cell]  ;;

end;;

let active_complement = Private.active_complement;;
let cmp = Private.cmp;;
let eyed = Private.eyed;;
let ipair_support = Private.ipair_support;;
let nonfulfilment_by_ally_move = Private.nonfulfilment_by_ally_move;;
let of_concrete_object = Private.of_concrete_object;;
let opt_eyed = Private.opt_eyed;;
let pair = Private.pair;;
let support = Private.support;;
let to_concrete_object = Private.to_concrete_object;;
let to_readable_string = Private.to_readable_string;;
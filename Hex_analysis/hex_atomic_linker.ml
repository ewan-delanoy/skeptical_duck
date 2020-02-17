(* 
#use"Hex_analysis/hex_atomic_linker.ml";;

*)

exception Degenerate_pair of Hex_cell_t.t;;
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



let eyed dim d1 d2 cell=
   let _= Hex_connector_data.check_eyed_claw_parameters dim d1 d2 cell in 
   Hex_atomic_linker_t.Eyed_claw (d1,d2,cell);;  


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
 | Eyed_claw (d1,d2,cell) -> 
    let ipairs = Hex_connector_data.advanced_eyed_claw d1 d2 (Hex_cell.to_int_pair cell) in 
    Hex_cell_set.safe_set (Image.image Hex_cell.of_int_pair ipairs) ;;
    
let ipair_support mlclr = 
   let (Hex_cell_set_t.S l)=support mlclr in 
   Image.image Hex_cell.to_int_pair l;;



let opt_eyed dim (p,untrimmed_s) =
    let s= Cull_string.trim_spaces untrimmed_s in 
    if String.length(s)<>3 then None else 
    let opt1 = Hex_cardinal_direction.opt_of_char (String.get s 0)
    and opt2 = Hex_cardinal_direction.opt_of_char (String.get s 2)
    in 
    if (opt1=None)||(opt2=None) then None else 
    try Some (eyed dim (Option.unpack opt1) (Option.unpack opt2) (Hex_cell.of_int_pair p)) with 
    _->None;;

let to_readable_string = function
  Hex_atomic_linker_t.Pair (cell1,cell2) -> 
    (Hex_cell.to_string cell1)^"-"^(Hex_cell.to_string cell2) 
 | Eyed_claw (d1,d2,cell) -> 
      let ipairs = Hex_connector_data.advanced_eyed_claw d1 d2 (Hex_cell.to_int_pair cell) in 
      let cells =Hex_cell_set.safe_set (Image.image Hex_cell.of_int_pair ipairs) in
     (Hex_cardinal_direction.short_name_for_pair (d1,d2))^
     "("^(Hex_cell_set.to_string cells)^")";;

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

let test_for_passive_to_active_conversion cell atm = match atm with 
    Hex_atomic_linker_t.Pair (cell1,cell2) ->
        if cell = cell1  then Some(cell2,atm) else
        if cell = cell2  then Some(cell1,atm) else None    
   | Eyed_claw (_,_,cell) -> None  ;;  

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
let test_for_passive_to_active_conversion = Private.test_for_passive_to_active_conversion;;
let to_concrete_object = Private.to_concrete_object;;
let to_readable_string = Private.to_readable_string;;
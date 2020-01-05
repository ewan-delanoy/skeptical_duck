(* 

#use"Hex_analysis/hex_flattened_end_strategy_field.ml";;


*)

module Private = struct

let beneficiary fles = fles.Hex_flattened_end_strategy_t.beneficiary;;

let change_parts fles (new_active_part,new_passive_part) = 
   {
      fles with 
      Hex_flattened_end_strategy_t.active_part = new_active_part ;
      Hex_flattened_end_strategy_t.passive_part = new_passive_part ;
   }

let partial_unveil fles=
  (
     fles.Hex_flattened_end_strategy_t.beneficiary,
     Hex_cell_set.forget_order(fles.Hex_flattened_end_strategy_t.active_part),
     Hex_cell_set.forget_order(fles.Hex_flattened_end_strategy_t.passive_part) 
  );;

let cmp = 
  let cmp_for_cell_lists = Total_ordering.lex_compare Hex_cell.cmp in 
 ((fun fles1 fles2 ->
   (Total_ordering.triple_product 
     Total_ordering.standard
     cmp_for_cell_lists
     cmp_for_cell_lists)
   (partial_unveil fles1) (partial_unveil fles2)  
) :> Hex_flattened_end_strategy_t.t Total_ordering.t);;

let salt = "Hex_"^"flattened_end_strategy_t.";;

let beneficiary_label    = salt ^ "beneficiary";;
let character_label      = salt ^ "character";;
let active_part_label    = salt ^ "active_part";;
let passive_part_label   = salt ^ "passive_part";;
let index_label          = salt ^ "index";;


let of_concrete_object  crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
      Hex_flattened_end_strategy_t.beneficiary = Hex_player.of_concrete_object (g beneficiary_label);
      character = Hex_strategy_character.of_concrete_object (g character_label);
      active_part = Hex_cell_set.of_concrete_object (g active_part_label);
      passive_part = Hex_cell_set.of_concrete_object (g passive_part_label);
      index = Concrete_object_field.unwrap_int (g index_label);
   };;

let to_concrete_object fles =
 
   Concrete_object_t.Record([
     beneficiary_label,Hex_player.to_concrete_object(fles.Hex_flattened_end_strategy_t.beneficiary);
     character_label, Hex_strategy_character.to_concrete_object(fles.Hex_flattened_end_strategy_t.character);
     active_part_label, Hex_cell_set.to_concrete_object(fles.Hex_flattened_end_strategy_t.active_part);
     passive_part_label, Hex_cell_set.to_concrete_object(fles.Hex_flattened_end_strategy_t.passive_part);
     index_label, Concrete_object_t.Int(fles.Hex_flattened_end_strategy_t.index); 
   ]);;

end;;

let active_part fles = fles.Hex_flattened_end_strategy_t.active_part;;
let beneficiary = Private.beneficiary;;
let change_parts = Private.change_parts;;
let of_concrete_object = Private.of_concrete_object;;
let passive_part fles = fles.Hex_flattened_end_strategy_t.active_part;;
let to_concrete_object = Private.to_concrete_object;;

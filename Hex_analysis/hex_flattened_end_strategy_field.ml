(* 

#use"Hex_analysis/hex_flattened_end_strategy_field.ml";;


*)

module Private = struct


let salt = "Hex_"^"flattened_end_strategy_t.";;

let dimension_label      = salt ^ "dimension";;
let beneficiary_label    = salt ^ "beneficiary";;
let data_label           = salt ^ "data";;
let index_label          = salt ^ "index";;


let of_concrete_object  crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
      Hex_flattened_end_strategy_t.dimension = Hex_dimension.of_concrete_object (g dimension_label);
      beneficiary = Hex_player.of_concrete_object (g beneficiary_label);
      data = Hex_extended_molecular.of_concrete_object (g data_label);
      index = Concrete_object_field.unwrap_int (g index_label);
   };;

let to_concrete_object fles =
 
   Concrete_object_t.Record([
     dimension_label, Hex_dimension.to_concrete_object(fles.Hex_flattened_end_strategy_t.dimension);
     beneficiary_label,Hex_player.to_concrete_object(fles.Hex_flattened_end_strategy_t.beneficiary);
     data_label, Hex_extended_molecular.to_concrete_object(fles.Hex_flattened_end_strategy_t.data);
     index_label, Concrete_object_t.Int(fles.Hex_flattened_end_strategy_t.index); 
   ]);;

end;;

let active_part fles = Hex_extended_molecular.active_part fles.Hex_flattened_end_strategy_t.data;;
let beneficiary fles = fles.Hex_flattened_end_strategy_t.beneficiary;;
let index fles = fles.Hex_flattened_end_strategy_t.index;;
let of_concrete_object = Private.of_concrete_object;;
let passive_part fles = Hex_extended_molecular.passive_part fles.Hex_flattened_end_strategy_t.data;;
let set_index fles new_idx = {fles with Hex_flattened_end_strategy_t.index = new_idx};;
let to_concrete_object = Private.to_concrete_object;;

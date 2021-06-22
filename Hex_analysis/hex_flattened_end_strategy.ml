(* 

#use"Hex_analysis/hex_flattened_end_strategy.ml";;

*)

module Automatic = struct 

    module Private = struct


        let salt = "Hex_"^"flattened_end_strategy_t.";;
        
        let dimension_label      = salt ^ "dimension";;
        let beneficiary_label    = salt ^ "beneficiary";;
        let data_label           = salt ^ "data";;
        let index_label          = salt ^ "index";;
        
        
        let of_concrete_object  crobj= 
           let g = Concrete_object.get_record crobj in 
           {
              Hex_flattened_end_strategy_t.dimension = Hex_dimension.of_concrete_object (g dimension_label);
              beneficiary = Hex_player.of_concrete_object (g beneficiary_label);
              data = Hex_extended_molecular.of_concrete_object (g data_label);
              index = Crobj_converter.int_of_concrete_object (g index_label);
           };;
        
        let to_concrete_object fles =
         
           Concrete_object_t.Record([
             dimension_label, Hex_dimension.to_concrete_object(fles.Hex_flattened_end_strategy_t.dimension);
             beneficiary_label,Hex_player.to_concrete_object(fles.Hex_flattened_end_strategy_t.beneficiary);
             data_label, Hex_extended_molecular.to_concrete_object(fles.Hex_flattened_end_strategy_t.data);
             index_label, Crobj_converter.int_to_concrete_object(fles.Hex_flattened_end_strategy_t.index); 
           ]);;
        
        end;;
        
        let active_part fles = Hex_extended_molecular.active_part fles.Hex_flattened_end_strategy_t.data;;
        let beneficiary fles = fles.Hex_flattened_end_strategy_t.beneficiary;;
        let index fles = fles.Hex_flattened_end_strategy_t.index;;
        let of_concrete_object = Private.of_concrete_object;;
        let passive_part fles = Hex_extended_molecular.passive_part fles.Hex_flattened_end_strategy_t.data;;
        let set_index fles new_idx = {fles with Hex_flattened_end_strategy_t.index = new_idx};;
        let to_concrete_object = Private.to_concrete_object;;
        

end ;;    

module Private = struct


let use_ally_move_to_simplify_one cell old_fles=
    {
       old_fles with 
       Hex_flattened_end_strategy_t.data = 
         Hex_extended_molecular.use_ally_move_to_simplify_one cell 
          (old_fles.Hex_flattened_end_strategy_t.data)
    };;
  
     
let use_ally_move_to_simplify_several cell old_flesses =
    Image.image(use_ally_move_to_simplify_one cell) old_flesses;;
          
let use_enemy_move_to_simplify_one cell old_fles=
   let old_data =old_fles.Hex_flattened_end_strategy_t.data in 
   match Hex_extended_molecular.use_enemy_move_to_simplify_one cell old_data with 
   None -> None 
   |Some(new_data) ->
   Some({
       old_fles with 
       Hex_flattened_end_strategy_t.data = new_data
    });;
     
let use_enemy_move_to_simplify_several cell old_flesses =
    Option.filter_and_unpack (use_enemy_move_to_simplify_one cell) old_flesses;;

let use_move_to_simplify_one (player,cell) old_fles =
   if player = Automatic.beneficiary  old_fles
   then Some(use_ally_move_to_simplify_one cell old_fles)
   else use_enemy_move_to_simplify_one cell old_fles;;

let immediate_opportunities flesses =
   let temp1 = Option.filter_and_unpack (
       fun fles->
         let l=Automatic.active_part fles in 
         if Hex_cell_set.length(l)=1 
         then let passive_set = Automatic.passive_part fles in 
              let m = Hex_cell_set.min l in 
              let mandatory_set=Hex_cell_set.insert m passive_set in 
               Some(fles,mandatory_set,m)
         else None
   ) flesses in 
   let cells = Image.image (fun (_,_,cell)->cell) temp1 in 
   let older_extmols = Image.image (fun (fles,_,_)->fles.Hex_flattened_end_strategy_t.data) temp1 in
   let interesting_indices = Image.image (fun (fles,_,_)->Automatic.index fles) temp1 in 
   let mand = Hex_mandatory_compound.escape_compound_in_disjunction cells older_extmols in 
   (cells,interesting_indices,mand);;

let support fles =
   Hex_cell_set.fold_merge
   [Automatic.active_part fles;
    Automatic.passive_part fles];;

let visualize fles= 
    let dim = fles.Hex_flattened_end_strategy_t.dimension 
    and winner = fles.Hex_flattened_end_strategy_t.beneficiary in 
    print_string 
     (Hex_extended_molecular.visualization 
      (dim,winner) fles.Hex_flattened_end_strategy_t.data);;


end;;


let immediate_opportunities = Private.immediate_opportunities;;
let support = Private.support;;
let use_ally_move_to_simplify_several = Private.use_ally_move_to_simplify_several;;
let use_enemy_move_to_simplify_several = Private.use_enemy_move_to_simplify_several;;
let use_move_to_simplify_one = Private.use_move_to_simplify_one;;
let visualize = Private.visualize ;;


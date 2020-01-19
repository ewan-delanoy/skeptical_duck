(* 

#use"Hex_analysis/hex_data_inspection.ml";;

*)

(*
let seek_companions ()=
   let _=Hex_persistent.initialize_all_data_if_necessary () in 
   Hex_fg_double_list.seek_companions 
    (!(Hex_persistent.games_ref)) 
   (Hex_end_strategy_factory.compute_isolated_end_configs Hex_persistent.wes_pair);;
*)

let check_companions ()=
   let _=Hex_persistent.initialize_all_data_if_necessary () in 
   Hex_fg_double_list.check_companions
    (!(Hex_persistent.games_ref)) 
   (Hex_end_strategy_factory.compute_isolated_end_configs Hex_persistent.wes_pair);;

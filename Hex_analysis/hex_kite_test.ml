(* 

#use"Hex_analysis/hex_kite_test.ml";;

*)

module Private = struct 

let persistent_data player = 
  let _=Hex_persistent.initialize_all_data_if_necessary() in 
  let factory = (
    match player with 
      Hex_player_t.First_player -> (!(fst(Hex_persistent.wes_pair)))  
    | Hex_player_t.Second_player -> (!(snd(Hex_persistent.wes_pair))) 
  ) in 
   let Hex_end_strategy_factory_t.F(_,_,cogs) = factory in 
   let moleculars =Option.filter_and_unpack (
     fun (Hex_cog_in_machine_t.C(statconstr,_,_,fles))->
    match statconstr with
    Hex_strategy_static_constructor_t.Molecular(mlclr,actv)->
      let idx = fles.Hex_flattened_end_strategy_t.index in 
      Some(idx,mlclr,actv)
    |_->None  
   ) cogs in 
   (factory,cogs,moleculars);;

let elev = Hex_dimension.eleven ;;

let generic_test player tester=
    let (factory,_,moleculars) = persistent_data player in 
    match Explicit.opt_find tester moleculars with 
    None -> None 
    |Some(idx,mlclr,actv) -> 
      let bad_fles = Hex_end_strategy_factory.strat_with_index factory idx in 
      let _= Hex_ascii_grid.see_flesh bad_fles in 
      let eob = Hex_end_of_battle.of_activated_molecular (elev,player) (actv,mlclr) in 
      let (unsac_start,unsac_end) = Hex_kite_factory.data_for_debugging eob in 
      Some(bad_fles,eob,unsac_start,unsac_end)
    ;;

let weak_test player =
   generic_test player (fun (idx,mlclr,actv)->
      let eob = Hex_end_of_battle.of_activated_molecular (elev,player) (actv,mlclr) in 
      Hex_kite_factory.solutions(eob)=[]
   ) ;;
    
let strong_test player =
   generic_test player (fun (idx,mlclr,actv)->
      let eob = Hex_end_of_battle.of_activated_molecular (elev,player) (actv,mlclr) in 
      Hex_kite_factory.solutions(eob)=[]
   ) ;;    

end ;;

let strong_test = Private.strong_test ;;
let weak_test = Private.weak_test ;;
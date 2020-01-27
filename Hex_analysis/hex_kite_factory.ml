(* 

#use"Hex_analysis/hex_kite_factory.ml";;

*)


module Private = struct 

let starters eob = 
    {
      Hex_kite_factory_t.dimension  = eob.Hex_end_of_battle_t.dimension ;
      winner         = eob.Hex_end_of_battle_t.winner ;
      finished       = [] ;
      initial_data   = eob ;
      unfinished     = Hex_partial_kite.starters eob;
    };;    

let pusher (factory,_) = 
   let raw_result=Image.image (
         Hex_partial_kite.extensions factory.Hex_kite_factory_t.initial_data
   ) factory.Hex_kite_factory_t.unfinished in 
   let new_moleculars = List.flatten (Image.image fst raw_result)
   and new_partial_kites = List.flatten (Image.image snd raw_result) in 
   let ordered_new_moleculars = Ordered.sort Total_ordering.standard new_moleculars in 
   let new_finished_ones = Ordered.merge Total_ordering.standard 
          ordered_new_moleculars (factory.Hex_kite_factory_t.finished) in      
   ({
      factory with 
      Hex_kite_factory_t.finished = new_finished_ones ;
      unfinished     = new_partial_kites ;
    },new_partial_kites=[]);;

let rec main walker =
   let (factory,computation_has_finished) = walker in 
   if computation_has_finished 
   then (factory.Hex_kite_factory_t.finished,factory.Hex_kite_factory_t.unfinished)
   else main (pusher walker) ;; 


end ;;

let compute eob = Private.main (Private.starters eob,false);;

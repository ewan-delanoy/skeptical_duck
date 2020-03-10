(* 

#use"Hex_analysis/hex_kite_factory.ml";;

*)


module Private = struct 

let starters eob = 
    {
      Hex_kite_factory_t.dimension  = eob.Hex_end_of_battle_t.dimension ;
      winner         = eob.Hex_end_of_battle_t.winner ;
      initial_data   = eob ;
      finished       = [] ;
      failures       = [] ;
      unfinished     = Hex_partial_kite.starters eob;
    };;    

let pusher (factory,_) = 
   let raw_result=Image.image (
         fun pk->
         (pk,Hex_partial_kite.extensions pk) 
   ) factory.Hex_kite_factory_t.unfinished in  
   let (failures1,nonfailures1) = List.partition (fun (_,p)->p=([],[]) ) raw_result in 
   let new_failures = List.rev_append (Image.image fst failures1) factory.Hex_kite_factory_t.failures in 
   let new_moleculars = List.flatten (Image.image (fun (_,p)->fst p) nonfailures1)
   and new_partial_kites = List.flatten (Image.image (fun (_,p)->snd p) nonfailures1) in 
   let ordered_new_moleculars = Ordered.sort Total_ordering.standard2 new_moleculars in 
   let new_finished_ones = Ordered.merge Total_ordering.standard2 
          ordered_new_moleculars (factory.Hex_kite_factory_t.finished) in      
   ({
      factory with 
      Hex_kite_factory_t.finished = new_finished_ones ;
      failures = new_failures ;
      unfinished     = new_partial_kites ;
    },new_partial_kites=[]);;

let rec main walker =
   let (factory,computation_has_finished) = walker in 
   if computation_has_finished 
   then (factory.Hex_kite_factory_t.finished,factory.Hex_kite_factory_t.failures)
   else main (pusher walker) ;; 


end ;;

let compute eob = Private.main (Private.starters eob,false);;

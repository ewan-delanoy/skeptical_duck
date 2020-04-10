(* 

#use"Hex_analysis/hex_kite_factory.ml";;

*)


module Private = struct 

let nonsacrificial_starters eob = 
    {
      Hex_kite_factory_t.dimension  = eob.Hex_end_of_battle_t.dimension ;
      winner         = eob.Hex_end_of_battle_t.winner ;
      initial_data   = eob ;
      finished       = [] ;
      failures       = [] ;
      unfinished     = Hex_starters_for_kite.nonsacrificial_starters eob;
    };;    
   
let sacrificial_starter eob pk= 
    {
      Hex_kite_factory_t.dimension  = eob.Hex_end_of_battle_t.dimension ;
      winner         = eob.Hex_end_of_battle_t.winner ;
      initial_data   = eob ;
      finished       = [] ;
      failures       = [] ;
      unfinished     = [pk];
    };;      


let pusher factory = 
   let raw_result=Image.image (
         fun pk->
         (pk,Hex_partial_kite.extensions factory.Hex_kite_factory_t.initial_data pk) 
   ) factory.Hex_kite_factory_t.unfinished in  
   let (failures1,nonfailures1) = List.partition (fun (_,p)->p=([],[]) ) raw_result in 
   let new_failures = List.rev_append (Image.image fst failures1) factory.Hex_kite_factory_t.failures in 
   let new_moleculars = List.flatten (Image.image (fun (_,p)->(fst p)) nonfailures1)
   and new_partial_kites = List.flatten (Image.image (fun (_,p)->snd p) nonfailures1) in 
   let ordered_new_moleculars = Ordered.sort Total_ordering.standard new_moleculars in 
   let new_finished_ones = Ordered.merge Total_ordering.standard 
          ordered_new_moleculars (factory.Hex_kite_factory_t.finished) in      
   {
      factory with 
      Hex_kite_factory_t.finished = new_finished_ones ;
      failures = new_failures ;
      unfinished     = new_partial_kites ;
    };;

let rec main walker =
   if walker.Hex_kite_factory_t.unfinished = [] 
   then (walker.Hex_kite_factory_t.finished,walker.Hex_kite_factory_t.failures)
   else main (pusher walker) ;; 

let extract_solutions l = Ordered.sort Total_ordering.standard (Image.image (fun (_,_,_,b,c)->(b,c))  l);;

let solutions_from_factory factory =
  extract_solutions (fst(main(factory)));;

let nonsacrificial_compute eob = main (nonsacrificial_starters eob);;

let nonsacrificial_solutions eob = solutions_from_factory(nonsacrificial_starters eob);;

(*
let compute eob =
    let first_try = nonsacrificial_compute eob in 
    if (fst first_try)<>[] 
    then first_try
    else 
    let temp1 = Hex_starters_for_kite.sacrificial_starters eob in 
*)        

end ;;

let nonsacrificial_compute = Private.nonsacrificial_compute;;
let solutions = Private.nonsacrificial_solutions;;
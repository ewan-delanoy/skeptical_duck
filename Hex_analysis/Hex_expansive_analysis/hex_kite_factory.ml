(* 

#use"Hex_analysis/hex_kite_factory.ml";;

*)


module Private = struct 

let nonsacrificial_starters eob = 
    {
      Hex_kite_factory_t.dimension  = eob.Hex_end_of_battle_t.dimension ;
      winner         = eob.Hex_end_of_battle_t.winner ;
      finished       = [] ;
      failures       = [] ;
      unfinished     = Hex_starters_for_kite.nonsacrificial_starters eob;
    };;    
   
let sacrificial_starter eob pk= 
    {
      Hex_kite_factory_t.dimension  = eob.Hex_end_of_battle_t.dimension ;
      winner         = eob.Hex_end_of_battle_t.winner ;
      finished       = [] ;
      failures       = [] ;
      unfinished     = [pk];
    };;      


let pusher factory = 
   let raw_result=Image.image (
         fun pk->
         (pk,Hex_expsv_springful_extension.extensions factory.Hex_kite_factory_t.dimension pk) 
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

let full_solutions_from_factory factory = fst(main(factory)) ;;

let extract_solutions l = Ordered.sort Total_ordering.standard 
   (Image.image (fun (_,_,_,mlclr,actv)->(mlclr,actv))  l);;


let nonsacrificial_full_solutions eob = full_solutions_from_factory (nonsacrificial_starters eob);;

let sacrificial_full_solutions eob = 
   let temp1 = Hex_starters_for_kite.sacrificial_starters eob in 
   List.flatten( Image.image (fun (seed,pk)->
      let ttemp3 = full_solutions_from_factory (sacrificial_starter eob pk) in 
      Image.image (fun sol->(seed,sol)) ttemp3
   ) temp1);;  

let nonsacrificial_solutions eob = extract_solutions (nonsacrificial_full_solutions eob);;


let sacrificial_solutions eob=
   let temp1 = Image.image (
      fun (scr,(_,_,_,mlclr,actv))->
         (Hex_sacrifice.reconstruct_sacrificial_solutions scr mlclr, actv)
   ) (sacrificial_full_solutions eob) in 
   Ordered.sort Total_ordering.standard  temp1;;

let solutions eob =
   let temp1 = nonsacrificial_solutions eob in 
   if temp1<>[] then temp1 else 
   sacrificial_solutions eob ;;   

let data_for_debugging eob =
   let temp1 = nonsacrificial_starters eob in
   (temp1,snd(main temp1)) ;;

end ;;

let data_for_debugging = Private.data_for_debugging ;;
let dummy = {
      Hex_kite_factory_t.dimension  = Hex_dimension.eleven ;
      winner         = Hex_player_t.First_player ;
      finished       = [] ;
      failures       = [] ;
      unfinished     = [];
    };; 
let push = Private.pusher ;;    
let solutions = Private.solutions ;;
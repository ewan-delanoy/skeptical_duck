(* 

#use"Hex_analysis/hex_strategy_factory.ml";;

*)


exception Bad_index_in_factory of int;;
exception Overlap_in_linker of Hex_cell_t.t list;;
exception Overlap_in_gluing of ((Hex_cell_t.t * (Hex_cell_set_t.t list)) list) * 
                               ((Hex_cell_t.t * (Hex_cell_set_t.t list)) list);;
exception Escape_in_disjunction of Hex_cell_t.t list;;                               


module Private = struct 

let get_elt_at_idx (Hex_end_strategy_factory_t.F(player,l)) k=
   let n=List.length(l) in 
   if (k<1)||(k>n) then raise(Bad_index_in_factory(k)) else List.nth l (k-1);;

let compute_parts factory (static_constructor,indices)=
   let Hex_end_strategy_factory_t.F(player,l)=factory in 
   let temp1=Image.image (get_elt_at_idx factory) indices in 
   let active_parts = Image.image  (fun (_,_,ec)->ec.Hex_flattened_end_strategy_t.active_part) temp1
   and passive_parts = Image.image (fun (_,_,ec)->ec.Hex_flattened_end_strategy_t.passive_part) temp1 in 
   match static_constructor with
    Hex_strategy_static_constructor_t.Basic_Linker(active_ones,Hex_cell_pair_set_t.S(passive_pairs))->
        let temp2=Image.image (fun (x,y)->Hex_cell_set.safe_set [x;y]) passive_pairs in 
        (active_ones,Hex_cell_set.fold_merge temp2)
    | Gluing -> (Hex_cell_set.fold_merge active_parts,Hex_cell_set.fold_merge passive_parts) 
    | Disjunction (cells)->
        let temp3=List.combine cells active_parts in 
        let temp4=Image.image (fun (c,part)->Hex_cell_set.outsert c part) temp3 in 
        let active_whole=Hex_cell_set.fold_merge temp4 in 
        let temp5=Hex_cell_set.fold_merge passive_parts in 
        (active_whole,Hex_cell_set.setminus temp5 active_whole);;

let compute_end_configuration factory  (static_constructor,indices)=
   let Hex_end_strategy_factory_t.F(player,l)=factory 
   and (active_p,passive_p)=compute_parts factory (static_constructor,indices) in 
   {
        Hex_flattened_end_strategy_t.beneficiary = player;
        active_part = active_p ; 
        passive_part = passive_p ; 
        index = (List.length l)+1;
   }  ;;    



let create_and_remember_already_checked_params old_factory static_constructor indices=
    let ec = compute_end_configuration old_factory  (static_constructor,indices) in 
    let Hex_end_strategy_factory_t.F(player,l)=old_factory in
    let new_l = l @ [(static_constructor,indices,ec)] in 
    (Hex_end_strategy_factory_t.F(player,new_l),ec);;


let helper_during_gluing_check parts =
   let (Hex_cell_set_t.S whole)= Hex_cell_set.fold_merge parts in 
   let temp1=Image.image (fun x->(x,List.filter (fun y->Hex_cell_set.mem x y) parts)) whole in 
   List.filter ( fun (x,l)->List.length(l)>1) temp1;;


let check_basic_linker (active_ones,Hex_cell_pair_set_t.S(passive_pairs))=
  let temp1=Image.image (fun (x,y)->Hex_cell_set.safe_set [x;y]) passive_pairs in 
  let temp2=Hex_cell_set.fold_merge temp1 in 
  let (Hex_cell_set_t.S temp3)=Hex_cell_set.fold_intersect [active_ones;temp2] in 
  if temp3<>[]
  then raise(Overlap_in_linker(temp3))
  else ();;


let check_gluing factory indices=
   let temp1=Image.image (get_elt_at_idx factory) indices in 
   let active_parts = Image.image  (fun (_,_,ec)->ec.Hex_flattened_end_strategy_t.active_part) temp1
   and passive_parts = Image.image (fun (_,_,ec)->ec.Hex_flattened_end_strategy_t.passive_part) temp1 in 
   let redundant_actives = helper_during_gluing_check active_parts 
   and redundant_passives = helper_during_gluing_check passive_parts in 
   let check= (redundant_actives,redundant_passives) in 
   if check=([],[])
   then ()
   else raise(Overlap_in_gluing(redundant_actives,redundant_passives));;

let check_disjunction factory cells indices=
   let Hex_end_strategy_factory_t.F(player,l)=factory in 
   let temp1=Image.image (get_elt_at_idx factory) indices in 
   let active_parts = Image.image  (fun (_,_,ec)->ec.Hex_flattened_end_strategy_t.active_part) temp1
   and passive_parts = Image.image (fun (_,_,ec)->ec.Hex_flattened_end_strategy_t.passive_part) temp1 in 
   let temp3=List.combine cells active_parts in 
   let temp4=Image.image (fun (c,part)->Hex_cell_set.outsert c part) temp3 in 
   let active_whole=Hex_cell_set.fold_merge temp4 in 
   let temp6=List.combine active_parts passive_parts in 
   let temp7=Image.image (fun (a,p)->Hex_cell_set.fold_merge [a;p]) temp6 in 
   let (Hex_cell_set_t.S escape_set) = Hex_cell_set.setminus (Hex_cell_set.fold_intersect temp7) active_whole in 
   if escape_set = []
   then ()
   else raise(Escape_in_disjunction(escape_set));;


let check_new_strategy factory static_constructor indices = match static_constructor with 
  Hex_strategy_static_constructor_t.Basic_Linker(active_ones,passive_pairs)->check_basic_linker (active_ones,passive_pairs)
  | Gluing -> check_gluing factory indices 
  | Disjunction (cells)->check_disjunction factory cells indices;;

let create_new_strategy factory static_constructor indices =
    let _= check_new_strategy factory static_constructor indices in 
    create_and_remember_already_checked_params factory static_constructor indices;;

let create_new_strategy_in_ref factory_ref static_constructor indices =
  let (new_factory,new_ec)=create_new_strategy (!factory_ref) static_constructor indices in 
  let _=(factory_ref:=new_factory) in new_ec;;

let create_new_strategies old_factory entries =
   let walker=ref(old_factory) in 
   let _=Image.image (fun (constr,indices)->create_new_strategy_in_ref walker constr indices) in 
   !walker;;


let create_new_strategy_in_double_ref (ref1,ref2) player static_constructor indices =
  match player with 
   Hex_player_t.First_player -> create_new_strategy_in_ref ref1 static_constructor indices 
  |Hex_player_t.Second_player -> create_new_strategy_in_ref ref2 static_constructor indices ;;

let announce_beneficiary ="\nBeneficiary : \n";;
let announce_data ="\nData : \n";;

let to_string  (Hex_end_strategy_factory_t.F(player,l))=
   let shortened_l=Image.image (fun (x,y,_)->(x,y)) l in 
   let descr1=Hex_player.to_string player 
   and descr2=Hex_end_strategy_entry_summary.list_to_string shortened_l in 
   announce_beneficiary^descr1^announce_data^descr2;;

let of_string text = 
   let text1 = Cull_string.two_sided_cutting (announce_beneficiary,"") text in 
   let i1=Substring.leftmost_index_of_in announce_data text1 in
   let j1=i1+(String.length announce_data)-1 in 
   let descr1=Cull_string.interval text1 1 (i1-1) 
   and descr2=Cull_string.interval text1 (j1+1) (String.length text1) in  
   let initial_one = Hex_end_strategy_factory_t.F(Hex_player.of_string descr1,[]) in 
   create_new_strategies initial_one (Hex_end_strategy_entry_summary.list_of_string descr2);;

let compute_all_end_configs (Hex_end_strategy_factory_t.F(_,l1),Hex_end_strategy_factory_t.F(_,l2))=
  Hex_ec_double_indexed_list_t.DL(
      Image.image (fun (_,_,z)->z) l1,
      Image.image (fun (_,_,z)->z) l2
  );;

end;;

let compute_all_end_configs (raf1,raf2) = Private.compute_all_end_configs (!raf1,!raf2);;
let create_new_strategy = Private.create_new_strategy_in_double_ref;;
let empty_one player = Hex_end_strategy_factory_t.F(player,[]);;
let fill_with_string raf text= (raf:=Private.of_string text);;
let to_string raf = Private.to_string (!raf);;






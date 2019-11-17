(* 

#use"Hex_analysis/hex_end_strategy_factory.ml";;

*)


exception Bad_index_in_factory of int;;
exception Overlap_in_linker of Hex_cell_set_t.t * Hex_cell_set_t.t;;
exception Overlap_in_gluing of ((Hex_cell_t.t * (Hex_cell_set_t.t list)) list) * 
                               ((Hex_cell_t.t * (Hex_cell_set_t.t list)) list);;
exception Escape_in_disjunction of Hex_cell_t.t list;;                               
exception Bad_index_in_disjunction of int * (Hex_cell_t.t list);; 

module Private = struct 

let get_elt_at_idx (Hex_end_strategy_factory_t.F(player,l)) k=
   let n=List.length(l) in 
   if (k<1)||(k>n) then raise(Bad_index_in_factory(k)) else List.nth l (k-1);;

let compute_parts factory (static_constructor,indices)=
   let Hex_end_strategy_factory_t.F(player,l)=factory in 
   let temp1=Image.image (get_elt_at_idx factory) indices in 
   let active_parts = Image.image  (fun (Hex_cog_in_machine_t.C(_,_,_,ec))->ec.Hex_flattened_end_strategy_t.active_part) temp1
   and passive_parts = Image.image (fun (Hex_cog_in_machine_t.C(_,_,_,ec))->ec.Hex_flattened_end_strategy_t.passive_part) temp1 in 
   match static_constructor with
    Hex_strategy_static_constructor_t.Basic_Linker(octop,active_ones,Hex_cell_pair_set_t.S(passive_pairs))->
        let (a,p)=Hex_octopus.actives_and_passives octop in 
        let temp2=Image.image (fun (x,y)->Hex_cell_set.safe_set [x;y]) passive_pairs in 
        (Hex_cell_set.fold_merge [a;active_ones],Hex_cell_set.fold_merge (p::temp2))
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
        character = Hex_strategy_character_t.Total_strategy;
        active_part = active_p ; 
        passive_part = passive_p ; 
        index = (List.length l)+1;
   }  ;;    



let create_and_remember_already_checked_params show_msg old_factory static_constructor comment indices=
    let ec = compute_end_configuration old_factory  (static_constructor,indices) in 
    let Hex_end_strategy_factory_t.F(player,l)=old_factory in
    let new_l = l @ [Hex_cog_in_machine_t.C(static_constructor,comment,indices,ec)] in
    let sn=string_of_int(List.length(l)+1) in 
    let added_cmt=(if comment="" 
                   then (Hex_strategy_static_constructor.summarize_in_string static_constructor) 
                   else comment)  in  
    let msg="\n\n Just created strategy number "^sn^" ("^added_cmt^" for "^(Hex_player.color player)^")\n\n" in 
    let _=(if show_msg then print_string msg;flush stdout) in 
    (Hex_end_strategy_factory_t.F(player,new_l),ec);;


let helper_during_gluing_check parts =
   let (Hex_cell_set_t.S whole)= Hex_cell_set.fold_merge parts in 
   let temp1=Image.image (fun x->(x,List.filter (fun y->Hex_cell_set.mem x y) parts)) whole in 
   List.filter ( fun (x,l)->List.length(l)>1) temp1;;


let check_basic_linker (octop,active_ones,Hex_cell_pair_set_t.S(passive_pairs))=
  let temp1=Image.image (fun (x,y)->Hex_cell_set.safe_set [x;y]) passive_pairs in 
  let passive_ones=Hex_cell_set.fold_merge temp1 in 
  let (o_actives,o_passives) = Hex_octopus.actives_and_passives octop in 
  match Hex_cell_set.seek_nondisjoint_parts [o_actives;o_passives;active_ones;passive_ones] with 
  None -> ()
  |Some(a,b)->raise(Overlap_in_linker(a,b));;
  


let check_gluing factory indices=
   let temp1=Image.image (get_elt_at_idx factory) indices in 
   let active_parts = Image.image  (fun (Hex_cog_in_machine_t.C(_,_,_,ec))->ec.Hex_flattened_end_strategy_t.active_part) temp1
   and passive_parts = Image.image (fun (Hex_cog_in_machine_t.C(_,_,_,ec))->ec.Hex_flattened_end_strategy_t.passive_part) temp1 in 
   let redundant_actives = helper_during_gluing_check active_parts 
   and redundant_passives = helper_during_gluing_check passive_parts in 
   let check= (redundant_actives,redundant_passives) in 
   if check=([],[])
   then ()
   else raise(Overlap_in_gluing(redundant_actives,redundant_passives));;

let check_disjunction factory cells indices=
   let Hex_end_strategy_factory_t.F(player,l)=factory in 
   let temp1=Image.image (get_elt_at_idx factory) indices in 
   let active_parts = Image.image  (fun (Hex_cog_in_machine_t.C(_,_,_,ec))->ec.Hex_flattened_end_strategy_t.active_part) temp1
   and passive_parts = Image.image (fun (Hex_cog_in_machine_t.C(_,_,_,ec))->ec.Hex_flattened_end_strategy_t.passive_part) temp1 in 
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
  Hex_strategy_static_constructor_t.Basic_Linker(octop,active_ones,passive_pairs)->check_basic_linker (octop,active_ones,passive_pairs)
  | Gluing -> check_gluing factory indices 
  | Disjunction (cells)->check_disjunction factory cells indices;;

let create_new_strategy show_msg factory static_constructor comment indices =
    let _= check_new_strategy factory static_constructor indices in 
    create_and_remember_already_checked_params show_msg factory static_constructor comment indices;;



let create_new_strategy_in_ref show_msg factory_ref static_constructor comment indices =
  let (new_factory,new_ec)=create_new_strategy show_msg (!factory_ref) static_constructor comment indices in 
  let _=(factory_ref:=new_factory) in new_ec;;

let create_new_strategies show_msg old_factory entries =
   let walker=ref(old_factory) in 
   let _=Image.image (fun (constr,comment,indices)->
     create_new_strategy_in_ref show_msg walker constr comment indices) entries in 
   !walker;;


let create_new_strategy_in_double_ref show_msg (ref1,ref2) player static_constructor comment indices =
  match player with 
   Hex_player_t.First_player -> create_new_strategy_in_ref show_msg ref1 static_constructor comment indices 
  |Hex_player_t.Second_player -> create_new_strategy_in_ref show_msg ref2 static_constructor comment indices ;;

let empty_one player= Hex_end_strategy_factory_t.F(player,[]);;

let compute_all_end_configs (Hex_end_strategy_factory_t.F(_,l1),Hex_end_strategy_factory_t.F(_,l2))=
  Hex_fles_double_list_t.DL(
      Image.image (fun (Hex_cog_in_machine_t.C(_,_,_,ec))->ec) l1,
      Image.image (fun (Hex_cog_in_machine_t.C(_,_,_,ec))->ec) l2
  );;

let reconstruct_disjunction (Hex_end_strategy_factory_t.F(player,l)) occupied_cells indices =
   let cell_of_index=(fun k->
     let (Hex_cog_in_machine_t.C(_,_,_,fles)) = List.nth l (k-1) in 
     let missing_cells= Hex_cell_set.setminus 
       (fles.Hex_flattened_end_strategy_t.active_part) occupied_cells in 
     let  (Hex_cell_set_t.S l_missing_cells)= missing_cells in 
     if   List.length(l_missing_cells)<>1
     then raise(Bad_index_in_disjunction(k,l_missing_cells))
     else List.hd l_missing_cells 
   ) in 
   Image.image cell_of_index indices;;

let of_concrete_object crobj=
    let (_,(arg1,arg2,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
    Hex_end_strategy_factory_t.F(Hex_player.of_concrete_object arg1, Hex_cog_in_machine.list_of_concrete_object arg2);;

let to_concrete_object (Hex_end_strategy_factory_t.F(player,l))=
   Concrete_object_t.Variant("Hex_"^"end_strategy_factory_t.F",[
      Hex_player.to_concrete_object player;
      Hex_cog_in_machine.list_to_concrete_object l
   ]);;

let to_string factory = Crobj_parsing.unparse (to_concrete_object factory) ;;

let of_string text = 
   of_concrete_object (Crobj_parsing.parse text);;

let restrict_to_strats_with_indices (Hex_end_strategy_factory_t.F(player,l)) indices =
   let partial_l=List.filter (
      fun (Hex_cog_in_machine_t.C(_,_,_,fles))->
        List.mem (fles.Hex_flattened_end_strategy_t.index) indices
   ) l in 
   let pre_reindexer = Ennig.index_everything indices in 
   let reindexer=Image.image (fun (x,y)->(y,x)) pre_reindexer in
   let new_l=Image.image (Hex_reindex.cog reindexer) partial_l in 
   Hex_end_strategy_factory_t.F(player,new_l);; 


end;;

let compute_all_end_configs (raf1,raf2) = Private.compute_all_end_configs (!raf1,!raf2);;
let create_new_strategy = Private.create_new_strategy_in_double_ref;;
let empty_one player = Hex_end_strategy_factory_t.F(player,[]);;
let fill_with_string raf text= (raf:=Private.of_string text);;
let get_elt_at_idx raf = Private.get_elt_at_idx (!raf);;
let reconstruct_disjunction = Private.reconstruct_disjunction;;
let restrict_to_strats_with_indices = Private.restrict_to_strats_with_indices;;
let to_string raf = Private.to_string (!raf);;






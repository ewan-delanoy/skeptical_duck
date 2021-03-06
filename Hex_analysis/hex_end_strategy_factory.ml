(* 

#use"Hex_analysis/hex_end_strategy_factory.ml";;

*)


exception Bad_index_in_factory of int;;

exception Disjunction_is_too_long of (int list) * (int list) ;;
exception Inadequate_constructor  of Hex_strategy_static_constructor_t.t * Hex_strategy_static_constructor_t.t ;;


exception Escape_in_disjunction of Hex_cell_t.t list;;                               
exception Bad_index_in_disjunction of int * (Hex_cell_t.t list);; 

module Private = struct 

let get_elt_at_idx (Hex_end_strategy_factory_t.F(dim,player,l)) k=
   let n=List.length(l) in 
   if (k<1)||(k>n) then raise(Bad_index_in_factory(k)) else List.nth l (k-1);;

let get_elt_at_idx_in_pair (factory1,factory2) (player,idx)=
  match player with 
   Hex_player_t.First_player -> get_elt_at_idx factory1 idx 
  |Hex_player_t.Second_player -> get_elt_at_idx factory2 idx ;;  

let get_extmol_at_idx factory k= 
   let (Hex_cog_in_machine_t.C(_,_,_,fles))= get_elt_at_idx factory k in 
   fles.Hex_flattened_end_strategy_t.data;;


let compute_extmol_for_allegedly_exhaustive_disjunction factory cells indices=
   let Hex_end_strategy_factory_t.F(dim,player,l)=factory in 
   let older_extmols=Image.image (get_extmol_at_idx factory) indices in 
   let global_extmol = Hex_extended_molecular.disjunction cells older_extmols (* active part checked here *)
   and mand = Hex_mandatory_compound.escape_compound_in_disjunction cells older_extmols in 
   let _= Hex_mandatory_compound.assert_exhaustibility mand in 
   global_extmol;;


let compute_extmol factory (static_constructor,indices)=
   match static_constructor with
    Hex_strategy_static_constructor_t.Molecular(mlclr,active_ones)->
        Hex_extended_molecular.of_molecular_and_active_ones (mlclr,active_ones)
    | Exhaustive_Disjunction (cells)->compute_extmol_for_allegedly_exhaustive_disjunction factory cells indices;;



let compute_flattened_version factory  (static_constructor,indices)=
   let Hex_end_strategy_factory_t.F(dim,player,l)=factory 
   and extmol=compute_extmol factory (static_constructor,indices) in 
   {
      Hex_flattened_end_strategy_t.dimension = dim ;
      beneficiary = player ;
      data = extmol ;
      index = (List.length(l)+1)
   };;    


let create_new_strategy show_msg old_factory static_constructor comment indices=
    let presumed_fles = compute_flattened_version old_factory  (static_constructor,indices) in 
    let Hex_end_strategy_factory_t.F(dim,player,l)=old_factory in
    let added_cmt=(if comment="" 
                   then (Hex_strategy_static_constructor.summarize_in_string static_constructor indices) 
                   else comment)  in 
    let sn=string_of_int(List.length(l)+1) in 
    let (fles,new_l,msg,reduncancy)=(
       match Option.seek ( 
          fun (Hex_cog_in_machine_t.C(static_constructor1,_,indices1,_))->
             (static_constructor1,indices1) = (static_constructor,indices)
       ) l with 
       None ->(
                presumed_fles,
                l @ [Hex_cog_in_machine_t.C(static_constructor,comment,indices,presumed_fles)],
                "\n\n Just created strategy number "^sn^" ("^added_cmt^" for "^(Hex_player.color player)^")\n\n",
                false
              )
      |Some(Hex_cog_in_machine_t.C(_,_,_,old_fles))->
              let si = string_of_int(Hex_flattened_end_strategy_field.index old_fles) in 
              (
                 old_fles,
                 l,
                 "\n\n Strategy already exists (number "^si^"). Nothing created",
                 true
              )        
    ) in 
    let _=(if show_msg || reduncancy then print_string msg;flush stdout) in 
    (Hex_end_strategy_factory_t.F(dim,player,new_l),fles);;



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

let empty_one dim player= Hex_end_strategy_factory_t.F(dim,player,[]);;

let compute_all_end_configs 
  (Hex_end_strategy_factory_t.F(_,_,l1),
   Hex_end_strategy_factory_t.F(_,_,l2))=
  Hex_fles_double_list_t.DL(
      Image.image (fun (Hex_cog_in_machine_t.C(_,_,_,fles))->fles) l1,
      Image.image (fun (Hex_cog_in_machine_t.C(_,_,_,fles))->fles) l2
  );;



let of_concrete_object crobj=
    let (_,(arg1,arg2,arg3,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
    Hex_end_strategy_factory_t.F(
        Hex_dimension.of_concrete_object arg1, 
        Hex_player.of_concrete_object arg2, 
        Hex_cog_in_machine.list_of_concrete_object arg3);;

let to_concrete_object (Hex_end_strategy_factory_t.F(dim,player,l))=
   Concrete_object_t.Variant("Hex_"^"end_strategy_factory_t.F",[
      Hex_dimension.to_concrete_object dim ;
      Hex_player.to_concrete_object player;
      Hex_cog_in_machine.list_to_concrete_object l
   ]);;

let to_string factory = Crobj_parsing.unparse (to_concrete_object factory) ;;

let of_string text = 
   of_concrete_object (Crobj_parsing.parse text);;

let restrict_to_strats_with_indices 
   (Hex_end_strategy_factory_t.F(dim,player,l)) indices =
   let partial_l=List.filter (
      fun (Hex_cog_in_machine_t.C(_,_,_,fles))->
        List.mem (Hex_flattened_end_strategy_field.index fles) indices
   ) l in 
   let pre_reindexer = Ennig.index_everything indices in 
   let reindexer=Image.image (fun (x,y)->(y,x)) pre_reindexer in
   let new_l=Image.image (Hex_reindex.cog reindexer) partial_l in 
   Hex_end_strategy_factory_t.F(dim,player,new_l);; 


let remove_strats_with_indices factory  unordered_removed_indices =
   let removed_indices = Set_of_integers.sort unordered_removed_indices in 
   let (Hex_end_strategy_factory_t.F(dim,player,l)) = factory in 
   let old_indices=Image.image (
      fun (Hex_cog_in_machine_t.C(_,_,_,fles))->
         (Hex_flattened_end_strategy_field.index fles) 
   ) l in 
   let remaining_indices = List.filter (fun idx->
      not(Set_of_integers.mem idx removed_indices)
   ) old_indices in 
   restrict_to_strats_with_indices factory remaining_indices;;

let indices_used_in_exhaustive_disjunctions 
    (Hex_end_strategy_factory_t.F(dim,player,l))=
    let temp1 = Image.image (fun (Hex_cog_in_machine_t.C(constr,_,indices,_))->indices) l in 
    Ordered.fold_merge Total_ordering.standard temp1;;

let compute_isolated_end_configs_in_one_factory factory =
   let (Hex_end_strategy_factory_t.F(dim,player,l)) = factory in 
   let syndicated_indices = indices_used_in_exhaustive_disjunctions factory in 
   Option.filter_and_unpack (
     fun (Hex_cog_in_machine_t.C(_,_,_,fles))->
        if List.mem (Hex_flattened_end_strategy_field.index fles)  syndicated_indices 
        then None 
        else Some(fles)
   ) l;;

let compute_isolated_end_configs (factory1,factory2)=
  Hex_fles_double_list_t.DL(
      compute_isolated_end_configs_in_one_factory factory1,
      compute_isolated_end_configs_in_one_factory factory2
  );;

let get_activated_moleculars (Hex_end_strategy_factory_t.F(dim,player,l)) = 
   Option.filter_and_unpack (
     fun (Hex_cog_in_machine_t.C(constr,_,_,_))->
        match constr with  
        Hex_strategy_static_constructor_t.Molecular(mlclr,active_part)->Some(active_part,mlclr)
        |Exhaustive_Disjunction (cells)->None 
   ) l;;

let strat_with_index (Hex_end_strategy_factory_t.F(dim,player,l)) idx=
    let (Hex_cog_in_machine_t.C(_,_,_,fles0)) =Listennou.force_find (
     fun (Hex_cog_in_machine_t.C(_,_,_,fles))->
         fles.Hex_flattened_end_strategy_t.index = idx 
   ) l in 
   fles0;;
   

end;;

let compute_all_end_configs (raf1,raf2) = Private.compute_all_end_configs (!raf1,!raf2);;
let compute_isolated_end_configs (raf1,raf2) = Private.compute_isolated_end_configs (!raf1,!raf2);;
let create_new_strategy = Private.create_new_strategy_in_double_ref;;
let empty_one  = Private.empty_one ;;
let fill_with_string raf text= (raf:=Private.of_string text);;
let get_elt_at_idx raf = Private.get_elt_at_idx (!raf);;
let get_elt_at_idx_in_pair (raf1,raf2) = Private.get_elt_at_idx_in_pair (!raf1,!raf2);;
let get_activated_moleculars raf = Private.get_activated_moleculars (!raf);;
let indices_used_in_exhaustive_disjunctions = Private.indices_used_in_exhaustive_disjunctions;;
let remove_strats_with_indices raf indices= (raf:=Private.remove_strats_with_indices (!raf) indices);;
let strat_with_index = Private.strat_with_index ;;
let to_string raf = Private.to_string (!raf);;






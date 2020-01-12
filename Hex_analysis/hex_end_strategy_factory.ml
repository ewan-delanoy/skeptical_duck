(* 

#use"Hex_analysis/hex_end_strategy_factory.ml";;

*)


exception Bad_index_in_factory of int;;
exception Escape_in_disjunction of Hex_cell_t.t list;;                               
exception Bad_index_in_disjunction of int * (Hex_cell_t.t list);; 

module Private = struct 

let get_elt_at_idx (Hex_end_strategy_factory_t.F(player,l)) k=
   let n=List.length(l) in 
   if (k<1)||(k>n) then raise(Bad_index_in_factory(k)) else List.nth l (k-1);;

let get_elt_at_idx_in_pair (factory1,factory2) (player,idx)=
  match player with 
   Hex_player_t.First_player -> get_elt_at_idx factory1 idx 
  |Hex_player_t.Second_player -> get_elt_at_idx factory2 idx ;;  

let compute_parts factory (static_constructor,indices)=
   match static_constructor with
    Hex_strategy_static_constructor_t.Molecular(mlclr,active_ones)->
        Hex_extended_molecular.of_molecular_and_active_ones (mlclr,active_ones)
    | Disjunction (cells)->
        let temp1=Image.image (get_elt_at_idx factory) indices in 
        let constituants = Image.image  
           (fun (Hex_cog_in_machine_t.C(_,_,_,fles))->fles.Hex_flattened_end_strategy_t.data) temp1 in 
        Hex_extended_molecular.disjunction constituants;;


let compute_flattened_version factory  (static_constructor,indices)=
   let Hex_end_strategy_factory_t.F(player,l)=factory 
   and extmol=compute_parts factory (static_constructor,indices) in 
   let fles = {
      Hex_flattened_end_strategy_t.beneficiary = player ;
      data = extmol ;
      index = (List.length(l)+1)
   } in 
   fles;;    



let create_and_remember_already_checked_params show_msg old_factory static_constructor comment indices=
    let presumed_fles = compute_flattened_version old_factory  (static_constructor,indices) in 
    let Hex_end_strategy_factory_t.F(player,l)=old_factory in
    let added_cmt=(if comment="" 
                   then (Hex_strategy_static_constructor.summarize_in_string static_constructor) 
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
    (Hex_end_strategy_factory_t.F(player,new_l),fles);;

let check_disjunction factory cells indices=
   let Hex_end_strategy_factory_t.F(player,l)=factory in 
   let temp1=Image.image (get_elt_at_idx factory) indices in 
   let active_parts  = Image.image  (fun (Hex_cog_in_machine_t.C(_,_,_,fles))->
       Hex_flattened_end_strategy_field.active_part fles) temp1
   and passive_parts = Image.image (fun (Hex_cog_in_machine_t.C(_,_,_,fles))->
       Hex_flattened_end_strategy_field.passive_part fles) temp1 in 
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
  Hex_strategy_static_constructor_t.Molecular(_)->() (* checking should already have been done in  Hex_molecular_linker.constructor *)
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
      Image.image (fun (Hex_cog_in_machine_t.C(_,_,_,fles))->fles) l1,
      Image.image (fun (Hex_cog_in_machine_t.C(_,_,_,fles))->fles) l2
  );;

let reconstruct_disjunction (Hex_end_strategy_factory_t.F(player,l)) occupied_cells indices =
   let cell_of_index=(fun k->
     let (Hex_cog_in_machine_t.C(_,_,_,fles)) = List.nth l (k-1) in 
     let missing_cells= Hex_cell_set.setminus 
       (Hex_flattened_end_strategy_field.active_part fles) occupied_cells in 
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
        List.mem (Hex_flattened_end_strategy_field.index fles) indices
   ) l in 
   let pre_reindexer = Ennig.index_everything indices in 
   let reindexer=Image.image (fun (x,y)->(y,x)) pre_reindexer in
   let new_l=Image.image (Hex_reindex.cog reindexer) partial_l in 
   Hex_end_strategy_factory_t.F(player,new_l);; 


let remove_strats_with_indices factory  unordered_removed_indices =
   let removed_indices = Set_of_integers.sort unordered_removed_indices in 
   let (Hex_end_strategy_factory_t.F(player,l)) = factory in 
   let old_indices=Image.image (
      fun (Hex_cog_in_machine_t.C(_,_,_,fles))->
         (Hex_flattened_end_strategy_field.index fles) 
   ) l in 
   let remaining_indices = List.filter (fun idx->
      not(Set_of_integers.mem idx removed_indices)
   ) old_indices in 
   restrict_to_strats_with_indices factory remaining_indices;;



end;;

let compute_all_end_configs (raf1,raf2) = Private.compute_all_end_configs (!raf1,!raf2);;
let create_new_strategy = Private.create_new_strategy_in_double_ref;;
let empty_one player = Hex_end_strategy_factory_t.F(player,[]);;
let fill_with_string raf text= (raf:=Private.of_string text);;
let get_elt_at_idx raf = Private.get_elt_at_idx (!raf);;
let get_elt_at_idx_in_pair (raf1,raf2) = Private.get_elt_at_idx_in_pair (!raf1,!raf2);;
let reconstruct_disjunction = Private.reconstruct_disjunction;;
let remove_strats_with_indices raf indices= (raf:=Private.remove_strats_with_indices (!raf) indices);;
let to_string raf = Private.to_string (!raf);;






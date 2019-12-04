(*

#use"Filewatching/fw_final_cleaner.ml";;

*)

module Private = struct 

let salt = "Fw_"^"final_cleaner_t.";;

let redundant_dependencies_label = salt ^ "redundant_dependencies";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj 
   and of_crobj = Dfa_ending.of_concrete_object in
   {
      Fw_final_cleaner_t.redundant_dependencies = 
        Concrete_object_field.to_pair_list of_crobj of_crobj (g redundant_dependencies_label);
   };; 

let to_concrete_object cleaner=
   let to_crobj = Dfa_ending.to_concrete_object in
   let items= 
   [
    redundant_dependencies_label, Concrete_object_field.of_pair_list to_crobj to_crobj cleaner.Fw_final_cleaner_t.redundant_dependencies;
   ]  in
   Concrete_object_t.Record items;;

end ;; 

let constructor l= {
  Fw_final_cleaner_t.redundant_dependencies = l;
};;

let clean cleaner l_to_be_cleaned=
    let pairs = cleaner.Fw_final_cleaner_t.redundant_dependencies in 
    let l_to_be_excluded = Option.filter_and_unpack (fun 
       (Dfn_rootless_t.J(s,m,e))->
        match Option.seek (fun (has_priority,_)->has_priority=e) pairs with 
         None -> None 
        |Some(_,does_not_have_priority)-> 
           Some(Dfn_rootless_t.J(s,m,does_not_have_priority))
    ) l_to_be_cleaned in
    let to_be_excluded = Set_of_polys.sort(l_to_be_excluded) in 
    List.filter (fun x->Set_of_polys.nmem x to_be_excluded) l_to_be_cleaned ;; 

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;


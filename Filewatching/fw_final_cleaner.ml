(*

#use"Filewatching/fw_final_cleaner.ml";;

*)

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

    
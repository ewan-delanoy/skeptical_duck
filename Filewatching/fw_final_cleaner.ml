(*

#use"Filewatching/fw_final_cleaner.ml";;



*)

let test cleaner untested_l candidate=
    let pairs = cleaner.Fw_final_cleaner_t.redundant_dependencies in 
    let (Dfn_rootless_t.J(s,m,e),_,_) = candidate in 
    match Option.seek (fun (has_priority,does_not_have_priority)->has_priority=e) pairs with 
     None -> untested_l 
    |Some(_,does_not_have_priority)-> 
        let excluded_one = Dfn_rootless_t.J(s,m,does_not_have_priority) in 
        List.filter (
          fun (rootless,_,_) ->rootless <> excluded_one
        ) untested_l;; 
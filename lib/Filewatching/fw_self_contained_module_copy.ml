(*

#use"lib/Filewatching/fw_self_contained_module_copy.ml";;

*)

module Private = struct 
let unsharped_content hm=
    let mlx=Dfn_join.to_ending hm Dfa_ending.ml in
    let ap_ml=Dfn_full.to_absolute_path mlx in
    let naive_content=Io.read_whole_file ap_ml in
    if Sys.file_exists((Absolute_path.to_string ap_ml)^"l")
    then (
           let temp1=Str.split (Str.regexp_string "\n") naive_content in
           let temp2=List.filter_map (
                fun t->
                 if t="" then Some("") else
                 if String.get t 0='#' then None else Some(t) 
           ) temp1 in
           String.concat "\n" temp2
         )
    else naive_content;;

end ;;

let copy fw ~prefix hm=
   let nm=Dfn_endingless.to_module hm in
   let those_above=( Fwc_with_modular_infrastructure.ancestors_for_module fw nm)@[nm] in
   let temp1=Image.image (
       fun nm2->
         let hm2=Fwc_with_modular_infrastructure.endingless_at_module fw nm2 in
         let mlx=Dfn_join.to_ending hm2 Dfa_ending.ml in
         let ap=Dfn_full.to_absolute_path mlx in
         let naked_name=Modularize.module_name_from_path ap in
         let s_name=Dfa_module.to_line naked_name in
         let mname=String.capitalize_ascii s_name in
         let new_mname=String.capitalize_ascii (prefix^s_name) in
         let content=Private.unsharped_content hm2 in
         ((mname,new_mname),content)
   ) those_above in
   let replacements=Image.image (
       fun ((a,b),_)->(Dfa_module.of_line a,Dfa_module.of_line b)
   ) temp1 in
   let contents=Image.image snd temp1 in
   let new_contents=Image.image 
        (Look_for_module_names.change_several_module_names_in_ml_ocamlcode replacements  ) 
         contents in
   let temp2=List.combine replacements new_contents in
   let temp3=Image.image (
     fun ((_,new_mname),txt)->
      let name=String.capitalize_ascii(Dfa_module.to_line new_mname) in
      "module "^name^"=struct "^txt^" end"^Particular_string.double_semicolon
   ) temp2 in    
   String.concat "\n\n\n" temp3;;





(*
#use"self_contained_module_copy.ml";;
*)


let unsharped_content hm=
    let mlx=Dfn_join.to_ending hm Dfa_ending.ml in
    let ap_ml=Dfn_full.to_absolute_path mlx in
    let naive_content=Io.read_whole_file ap_ml in
    if Sys.file_exists((Absolute_path.to_string ap_ml)^"l")
    then (
           let temp1=Str.split (Str.regexp_string "\n") naive_content in
           let temp2=Option.filter_and_unpack (
                fun t->
                 if t="" then Some("") else
                 if String.get t 0='#' then None else Some(t) 
           ) temp1 in
           String.concat "\n" temp2
         )
    else naive_content;;

let self_contained_module_copy prefix hm=
   let cs=(!(Usual_coma_state.main_ref)) in 
   let nm=Dfn_endingless.to_module hm in
   let those_above=(Coma_state.above cs  hm)@[nm] in
   let temp1=Image.imagination (
       fun nm2->
         let hm2=Coma_state.endingless_at_module cs nm2 in
         let mlx=Dfn_join.to_ending hm2 Dfa_ending.ml in
         let ap=Dfn_full.to_absolute_path mlx in
         let naked_name=Modularize.module_name_from_path ap in
         let s_name=Dfa_module.to_line naked_name in
         let mname=String.capitalize_ascii s_name in
         let new_mname=String.capitalize_ascii (prefix^s_name) in
         let content=unsharped_content hm2 in
         ((mname,new_mname),content)
   ) those_above in
   let replacements=Image.imagination (
       fun ((a,b),_)->(Dfa_module.of_line a,Dfa_module.of_line b)
   ) temp1 in
   let contents=Image.imagination snd temp1 in
   let new_contents=Image.imagination 
        (Look_for_module_names.change_several_module_names_in_ml_ocamlcode replacements  ) 
         contents in
   let temp2=List.combine replacements new_contents in
   let temp3=Image.imagination (
     fun ((_,new_mname),txt)->
      let name=String.capitalize_ascii(Dfa_module.to_line new_mname) in
      "module "^name^"=struct "^txt^" end"^Double_semicolon.ds
   ) temp2 in    
   String.concat "\n\n\n" temp3;;





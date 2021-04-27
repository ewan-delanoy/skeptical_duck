(*

#use"look_for_module_names.ml";;

*)

exception Unknown_ending_during_modulename_changing of string ;;
exception Change_not_implemented of string ;;

exception Unknown_ending_during_modulename_reading of string ;;
exception Reading_not_implemented of string ;;

module Private = struct 


let indices_in_ml_ocamlcode code=
  let temp1=Outside_comments_and_strings.good_substrings code in
  let temp2=Image.image (fun (a,b,t)->
     let ttemp3=Alternative_str.find_all_occurrences Alternative_str_example.moodle_cases t 1 in
     Image.image (fun (case_index,(u,v))->
        (case_index,(u+a-1,v+a-1))
     ) ttemp3
  ) temp1 in
  List.flatten temp2;;

let indices_in_mli_ocamlcode code=  indices_in_ml_ocamlcode code ;; 
  
let indices_in_mlx_file ap=  
    let s_ap = Absolute_path.to_string ap in 
    let ending = Cull_string.after_rightmost s_ap '.' in 
    if ending = "ml"  then indices_in_ml_ocamlcode (Io.read_whole_file ap) else 
    if ending = "mli" then indices_in_mli_ocamlcode (Io.read_whole_file ap) else   
    if ending = "mll" then raise(Change_not_implemented s_ap) else 
    if ending = "mly" then raise(Change_not_implemented s_ap) else   
    raise(Unknown_ending_during_modulename_reading s_ap);;  

let names_in_ml_ocamlcode z=
  let temp1=indices_in_ml_ocamlcode z in
  let temp2=Image.image (fun (_,(a,b))->String.sub z (a-1) (b-a+1) ) temp1 in
  let temp3=Three_parts.generic temp2 in
  let temp4=List.filter (fun (x,y,z)->not(List.mem y x)) temp3 in
  let temp5=Image.image (fun (x,y,z)->Dfa_module.of_line 
      (String.uncapitalize_ascii  y)) temp4 in
  temp5;;

let indices_in_ml_file file=indices_in_ml_ocamlcode(Io.read_whole_file file);;  
let names_in_ml_file file=names_in_ml_ocamlcode(Io.read_whole_file file);;



let change_module_name_in_ml_ocamlcode
   old_naked_name
   new_naked_name old_code=
   let old_name=String.capitalize_ascii(Dfa_module.to_line(old_naked_name))
   and new_name=String.capitalize_ascii(Dfa_module.to_line(new_naked_name)) in
   let itv=(fun a b->String.sub old_code (a-1) (b-a+1)) in
   let temp1=indices_in_ml_ocamlcode old_code in
   let temp2=List.filter (fun (j,(a,b))->(itv a b)=old_name ) temp1 in
   if temp2=[]
   then old_code
   else
   let temp3 = Image.image (fun (j,(a,b))->((a,b),new_name) ) temp2 in  
   Strung.replace_ranges_in temp3 old_code;;
 
  

 let change_module_name_in_ml_file old_name new_name file=
   let s=Io.read_whole_file file in
   let new_s=change_module_name_in_ml_ocamlcode old_name new_name s in
   Io.overwrite_with file new_s;;  

 let change_module_name_in_mli_file old_name new_name file=
 change_module_name_in_ml_file old_name new_name file ;;

  let change_module_name_in_mlx_file old_name new_name ap=  
    let s_ap = Absolute_path.to_string ap in 
    let ending = Cull_string.after_rightmost s_ap '.' in 
    if ending = "ml"  then change_module_name_in_ml_file old_name new_name ap else 
    if ending = "mli" then change_module_name_in_mli_file old_name new_name ap else   
    if ending = "mll" then raise(Change_not_implemented s_ap) else 
    if ending = "mly" then raise(Change_not_implemented s_ap) else   
    raise(Unknown_ending_during_modulename_changing s_ap);;

let change_several_module_names_in_ml_ocamlcode l_changes s=
    List.fold_left(fun t (u,v)->change_module_name_in_ml_ocamlcode u v t) s l_changes;;

let change_several_module_names_in_ml_file l_changes file=
   let s=Io.read_whole_file file in
   let new_s=change_several_module_names_in_ml_ocamlcode l_changes s in
   Io.overwrite_with file new_s;;  

end ;;

let change_module_name_in_mlx_file = Private.change_module_name_in_mlx_file ;;
 let change_module_name_in_ml_ocamlcode = Private.change_module_name_in_ml_ocamlcode ;;
 let change_several_module_names_in_ml_ocamlcode = Private.change_several_module_names_in_ml_ocamlcode ;;
 let indices_in_mlx_file = Private.indices_in_mlx_file ;;
 let names_in_ml_file = Private.names_in_ml_file ;;
 let names_in_ml_ocamlcode = Private.names_in_ml_ocamlcode ;;

(*   
   
indices_in_string "123 Haag.012 open Garfield;8";;

indices_in_string "(* Haag. *)234 Dog.\"open Garfield;\"67 Corn.4";;


   
*)              
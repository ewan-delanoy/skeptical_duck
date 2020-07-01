(*

#use"look_for_module_names.ml";;

*)


let indices_in_ml_ocamlcode s=
  let temp1=Outside_comments_and_strings.good_substrings s in
  let temp2=Image.vorstellung (fun (a,b,t)->
     let ttemp3=Alternative_str.find_all_occurrences Alternative_str_example.moodle_cases t 1 in
     Image.vorstellung (fun (case_index,(u,v))->
        (case_index,(u+a-1,v+a-1))
     ) ttemp3
  ) temp1 in
  List.flatten temp2;;
  

let names_in_ml_ocamlcode z=
  let temp1=indices_in_ml_ocamlcode z in
  let temp2=Image.vorstellung (fun (_,(a,b))->String.sub z (a-1) (b-a+1) ) temp1 in
  let temp3=Three_parts.generic temp2 in
  let temp4=List.filter (fun (x,y,z)->not(List.mem y x)) temp3 in
  let temp5=Image.vorstellung (fun (x,y,z)->Dfa_module.of_line 
      (String.uncapitalize_ascii  y)) temp4 in
  temp5;;

let indices_in_ml_file file=indices_in_ml_ocamlcode(Io.read_whole_file file);;  
let names_in_ml_file file=names_in_ml_ocamlcode(Io.read_whole_file file);;



let change_module_name_in_ml_ocamlcode
   old_naked_name
   new_naked_name s=
   let old_name=String.capitalize_ascii(Dfa_module.to_line(old_naked_name))
   and new_name=String.capitalize_ascii(Dfa_module.to_line(new_naked_name)) in
   let itv=(fun a b->String.sub s (a-1) (b-a+1)) in
   let temp1=indices_in_ml_ocamlcode s in
   let temp2=List.filter (fun (j,(a,b))->(itv a b)=old_name ) temp1 in
   if temp2=[]
   then s
   else
   let (_,(a1,b1))=List.hd(temp2) in
   let rec sub_f=(fun (graet,ax,bx,da_ober)->
     match da_ober with
      []->List.rev((itv (bx+1) (String.length s))::graet)
     |(_,(ay,by))::peurrest->
       let s1=itv (bx+1) (ay-1) in
       sub_f(new_name::s1::graet,ay,by,peurrest)
   ) in
   let temp3=sub_f([new_name;itv 1 (a1-1)],a1,b1,List.tl(temp2)) in
   String.concat "" temp3;;
   
 let change_module_name_in_ml_file old_name new_name file=
   let s=Io.read_whole_file file in
   let new_s=change_module_name_in_ml_ocamlcode old_name new_name s in
   Io.overwrite_with file new_s;;  

let change_several_module_names_in_ml_ocamlcode l_changes s=
    List.fold_left(fun t (u,v)->change_module_name_in_ml_ocamlcode u v t) s l_changes;;

let change_several_module_names_in_ml_file l_changes file=
   let s=Io.read_whole_file file in
   let new_s=change_several_module_names_in_ml_ocamlcode l_changes s in
   Io.overwrite_with file new_s;;  


(*   
   
indices_in_string "123 Haag.012 open Garfield;8";;

indices_in_string "(* Haag. *)234 Dog.\"open Garfield;\"67 Corn.4";;


   
*)              
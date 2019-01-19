
(* 

#use"Country/Germany/german_values_in_modules.ml";;

*)

let replace_string root_dir mdata old_string new_string=
  let temp1=Coma_state.files_containing_string mdata old_string in
  let m=String.length(Root_directory.connectable_to_subpath root_dir) in
  let temp2=Image.image (fun ap->
    Cull_string.cobeginning m (Absolute_path.to_string ap)) temp1 in
  let message="\n\n The following files will be rewritten : \n\n"^
  (String.concat "\n" temp2) in
  let _=(print_string message;flush stdout) in
  List.iter (Replace_inside.replace_inside_file (old_string,new_string)) temp1;;

(*

if the string argument has a dot inside it, we interpret it
as a value inside a module.
Otherwise we interpret it as a mere string.

*)


let rename_string_or_value root_dir mdata old_name new_name=
  if not(String.contains old_name '.')
  then replace_string root_dir mdata old_name new_name
  else 
    let new_full_name=(Father_and_son.father old_name '.')^"."^new_name in
    (Rename_value_inside_module.rename_value_inside_module 
            root_dir old_name (Overwriter.of_string new_name); 
     replace_string root_dir mdata old_name new_full_name
    );;


let list_values_from_module_in_file module_name file=
   let s=Io.read_whole_file file in
   let temp1=Look_for_module_names.indices_in_file file in
   let temp2=List.filter (fun (t,(i,j))->
     (t=Alternative_str_example.index_for_pointed_case)&&
     (Cull_string.interval s i j=(String.capitalize_ascii module_name))
   ) temp1 in
   let temp3=Image.image(fun (t,(i,j))->
    let opt=After.after_star 
     Charset.ocaml_modulename_nonfirst_letters
     s (j+2) in
    let end_idx=(match opt with Some(k)->k-1 |None->String.length s) in
     Cull_string.interval s (j+2) end_idx
   ) temp2 in
   Ordered_string.diforchan temp3;;

let list_values_from_module_in_modulesystem module_name mdata=
   let temp1=Coma_state.all_mlx_paths mdata in
   let temp2=Image.image (fun ap->
    let ttemp1=list_values_from_module_in_file module_name ap in
    Ordered_string.image (fun x->(x,ap) ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
   let temp4=Image.image fst temp3 in 
   let temp5=Ordered_string.diforchan temp4 in
   let temp6=Ordered.forget_order temp5 in
   let temp7=Image.image (
      fun x->(x,Option.filter_and_unpack(
        fun (y,ap)->if y=x then Some(ap) else None
      ) temp3)
   ) temp6 in
   temp7;;
 
let list_value_occurrences_in_file t file=
   let s=Io.read_whole_file file in
   let temp1=Substring.occurrences_of_in t s in
   Image.image (fun j->Cull_string.closeup_around_index 
      s j
   ) temp1;; 
 
 
 


let show_value_occurrences_in_modulesystem root_dir t mdata=
   let m=String.length(Root_directory.connectable_to_subpath root_dir) in
   let temp1=Coma_state.all_mlx_paths mdata in
   let temp2=Image.image (fun ap->
    let ttemp1=list_value_occurrences_in_file t ap in
    let mname=Cull_string.cobeginning(m)(Absolute_path.to_string ap) in
    Image.image (fun x->mname^":\n"^x ) ttemp1
    ) temp1 in
   let temp3=List.flatten temp2 in
   let temp4=String.concat "\n\n\n" (""::temp3@[""]) in 
   print_string temp4;;



           
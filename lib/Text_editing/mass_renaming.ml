(*

#use"lib/Text_editing/mass_renaming.ml";;

This module was initially inspired by the need for mass
renaming on a Kotlin project. 

*)

type kotlin_system_t = {
   main_directory : string ;
   app_name : string 
} ;; 

module Private = struct 

let select_files ks sub_path ending =
  let full_path = (ks.main_directory)^sub_path in 
  let dir1 = Directory_name.of_string full_path in  
  let all_files = Unix_again.complete_ls dir1 in 
  List.filter (fun ap->
    let s = Absolute_path.to_string ap in 
    String.ends_with ~suffix:ending s) all_files ;;

let javacomexample = "java/com/example/" ;;
let reslayout = "res/layout" ;;

let human_edited_files =(fun ks ->
   (select_files ks (javacomexample^ks.app_name) ".kt")
   @(select_files ks reslayout  ".xml"));;

let occurrences words ks =
   let filenames = Image.image (Absolute_path.to_string)(human_edited_files ks) in 
   Detect_inside.occurrences_for_several_words_in_several_files words filenames ;; 

(*   
let command_for_subfile_renaming ks old_name new_name =
   let full_filename = (fun fn->
     Absolute_path.to_string(Absolute_path.of_string(ks.main_directory^fn))  
  ) in 
  "mv "^(full_filename old_name)^" "^(full_filename new_name) ;; 
*)

let dir_renaming_command_in_set_app_name ks new_app_name = 
  let full_javacomexample_path =
    Absolute_path.to_string(Absolute_path.of_string(ks.main_directory^javacomexample)) in
  "mv "^full_javacomexample_path^(ks.app_name)^" "^
  full_javacomexample_path^new_app_name ;;   

let replace_several_inside_files ks words =
   let files = human_edited_files ks in    
   List.iter(
     Replace_inside.replace_several_inside_file
      ~display_number_of_matches:true
      ~silent_on_ambiguity:false words
    ) files ;;

  
let set_app_name ks new_app_name =
   let old_name = ks.app_name in 
   let _ = Sys.command(dir_renaming_command_in_set_app_name ks new_app_name) in 
   let new_ks = {ks with app_name = new_app_name} in 
   let compacter = (fun s->
     String.lowercase_ascii(Replace_inside.replace_inside_string ("_","") s)
      )   in 
   let old_compact = compacter old_name
   and new_compact = compacter new_app_name in 
   let _ = replace_several_inside_files new_ks ["com.example."^old_compact,"com.example."^new_compact] in 
   new_ks
  ;;

let deduced_pairs_for_filenames (old_uncap,new_uncap) =
   let old_cap = String.capitalize_ascii old_uncap 
   and new_cap = String.capitalize_ascii new_uncap in 
   [
    (old_uncap,new_uncap);
     (old_cap,new_cap)
   ] ;; 

let commands_for_filename_renaming ks rep =
  let enhanced_rep = deduced_pairs_for_filenames rep in 
  let modifications = List.filter_map (
  fun ap ->
     let s_ap =  Absolute_path.to_string ap in 
     let (before,old_after) = Cull_string.split_wrt_rightmost s_ap '/' in 
     let new_after = Replace_inside.replace_several_inside_string 
          enhanced_rep old_after in 
     if new_after<>old_after  
     then Some(s_ap,before^"/"^new_after)
     else None    
  )(human_edited_files ks) in 
  Image.image (
    fun (old_fn,new_fn)->"mv "^old_fn^" "^new_fn
  ) modifications;; 


let rename_files ks rep =
   let _ = Image.image Sys.command (commands_for_filename_renaming ks rep) in
   ks ;;

let completions pairs (old_cap,new_cap) =
    Image.image(fun (a,b)->(a^old_cap^b,a^new_cap^b)) pairs ;; 


let data_for_uncap_pairs = ref [] ;;
let data_for_cap_pairs = ref [] ;;

data_for_uncap_pairs := 
[
  ("",".title"); ("",".images"); ("",".synopsis");
  ("","Title");
  ("","Title");
  ("","ItemCallback");
  ("","_iv");("","_title_tv");
  ("","Iv");
  ("","IV");("","_rv");("","Rv");
  ("","ListKey");
  (" ","s.");(" "," ");
  (" ","s:");
  ("(",")");
  ("(",",");
  ("(",":");
  ("(","s[");("(","s,");("(","s:");
] ;; 

data_for_cap_pairs := 
[
  ("","Api");
  ("","DetailActivity");
  ("","ItemCallback");
  ("","ListViewAdapter");
  ("","Service");
  ("","ViewHolder");
  ("Activity","DetailBinding");
  ("display","List");
  ("delete","");
  ("local","s");
  ("local","Storage");
  ("Local","Storage");
  ("save","");
  ("<",">");
  (" ",",");(" "," ");(" ",")");
  ("get","s(");
  ("onSave","(");
] ;; 



let deduced_pairs_for_filecontents (old_uncap,new_uncap) =
    let old_cap = String.capitalize_ascii old_uncap 
    and new_cap = String.capitalize_ascii new_uncap in 
    (
      completions (!data_for_uncap_pairs) (old_uncap,new_uncap)
    )@
    (
      completions  (!data_for_cap_pairs) (old_cap,new_cap)
    );; 

let quick_deduced_pairs_for_filecontents (old_uncap,new_uncap) =
      let old_cap = String.capitalize_ascii old_uncap 
      and new_cap = String.capitalize_ascii new_uncap in 
     [
      (old_uncap,new_uncap);
      (old_cap,new_cap)
     ];; 


let rename_inside_files ks rep = 
   let _ = replace_several_inside_files ks (deduced_pairs_for_filecontents rep) in
   ks ;; 

let modify ks new_app_name rep =
   let new_ks = set_app_name ks new_app_name in 
   rename_files (rename_inside_files new_ks rep) rep;; 

let modify_quick ks new_app_name rep =
  let new_ks = set_app_name ks new_app_name in 
  let _ = replace_several_inside_files new_ks (quick_deduced_pairs_for_filecontents rep) in
  rename_files new_ks rep ;; 
   
end ;;

let modify = Private.modify ;;
let modify_quick = Private.modify_quick ;;

(*

open Mass_renaming ;;

let ks1 = {main_directory = "~/Downloads/main/"; app_name = "animeapplication"} ;;    
let ks2 = modify_quick ks1 "EbookApplication" ("manga","ebook") ;; 


let ks_ref = ref ks1 ;; 
let act() = (ks_ref := (rename_inside_files (!ks_ref) ("manga","crocodile"))) ;; 
let see () = 
    let temp1 = occurrences ["anga"] (!ks_ref) in 
    (List.length temp1,List_again.long_head 10 temp1);; 
let ae () = let _ = act () in see () ;; 

ks_ref := (rename_files (!ks_ref) ("manga","crocodile")) ;; 

let see1 = commands_for_filename_renaming ks1 rep1 ;; 
let see2= rename_files 


let rep1 = ("manga","crocodile") ;; 
let enhanced_rep1 = deduced_pairs_for_filenames rep1 ;; 

let modifications = List.filter_map (
  fun ap ->
     let s_ap =  Absolute_path.to_string ap in 
     let (before,old_after) = Cull_string.split_wrt_rightmost s_ap '/' in 
     let new_after = Replace_inside.replace_several_inside_string 
          enhanced_rep1 old_after in 
     if new_after<>old_after  
     then Some(s_ap,before^"/"^new_after)
     else None    
)(human_edited_files ks1) ;; 



open Mass_renaming ;; 

let ks1 = {
  main_directory = "~/Downloads/main/" ;
  app_name = "animeapplication"
} ;; 

let ks1 = set_app_name ks1 "experimental" ;;
let ks1 = set_app_name ks1 "anime_application" ;;


let see1 = human_edited_files ks1 ;;
let see2 = occurrences ["anga"] ks1 ;; 
let see3 = dir_renaming_command_in_set_app_name ks1 "experimental" ;; 

let g1 = 

let ks1 = {
  main_directory = "~/Downloads/main/" ;
  app_name = "experimental"
} ;; 

*)

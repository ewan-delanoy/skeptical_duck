(*

#use"prepare_dircopy_update.ml";;

*)


let compute_deleted_in_diff sourcedir destdir=
   let s_sourcedir=Dfa_root.connectable_to_subpath sourcedir
   and s_destdir=Dfa_root.connectable_to_subpath destdir in
   let temp1=More_unix.quick_beheaded_complete_ls s_destdir in
   List.filter(
       fun s->(s<>"README")
              &&(not(Supstring.begins_with s ".git/")) 
              &&(not(Sys.file_exists(s_sourcedir^s)))
   ) temp1;;
   
let compute_nondeleted_in_diff (sourcedir,l) destdir=
   let s_sourcedir=Dfa_root.connectable_to_subpath sourcedir
   and s_destdir=Dfa_root.connectable_to_subpath destdir in
   let created_accu=ref[]
   and changed_accu=ref[] in
   let _=Image.image(
   	  fun s->
   	    if (not(Sys.file_exists(s_destdir^s)))
   	    then created_accu:=s::(!created_accu)
   	    else 
   	    (
   	    let txt1=Io.read_whole_file
   	    (Absolute_path.of_string(s_sourcedir^s))
   	    and txt2=Io.read_whole_file
   	    (Absolute_path.of_string(s_destdir^s)) in
   	    if txt1<>txt2
   	    then changed_accu:=s::(!changed_accu)
   	    )
   ) l in
   (Recently_created.of_string_list (!created_accu),
    Recently_changed.of_string_list (!changed_accu));;   
   
  
let compute_diff (sourcedir,l) destdir=
   let (created,changed)=compute_nondeleted_in_diff (sourcedir,l) destdir in
   Dircopy_diff.veil
   
   	(Recently_deleted.of_string_list(compute_deleted_in_diff sourcedir destdir))
   	changed
   	created
   ;;
   
   
let greedy_list sourcedir=
   let converted_to_dir=Directory_name.of_string
      (Dfa_root.without_trailing_slash sourcedir) in
   let source_paths=More_unix.complete_ls_with_nondirectories_only converted_to_dir in
   Image.image (fun ap->
     let rootless_path = Dfn_common.decompose_absolute_path_using_root ap sourcedir in 
     Dfn_rootless.to_line rootless_path ) 
   source_paths;;
      
let compute_greedy_diff sourcedir destdir=
   compute_diff (sourcedir,greedy_list sourcedir) destdir;;      
   
let restricted_list sourcedir (ignored_subdirs,ignored_files)=
   let s_dir =  Dfa_root.without_trailing_slash sourcedir in 
   let converted_to_dir=Directory_name.of_string
      (Dfa_root.without_trailing_slash sourcedir) in
   let absolute_paths1=More_unix.complete_ls_with_nondirectories_only converted_to_dir in 
   let ignored_subdirs1=Image.image Dfa_subdirectory.without_trailing_slash ignored_subdirs in 
   Option.filter_and_unpack (fun ap->
     let s_ap = Absolute_path.to_string ap in 
     let s_rootless = Cull_string.cobeginning (String.length(s_dir)+1) s_ap in 
     if (List.exists(Supstring.begins_with s_rootless) ignored_subdirs1) ||
        (List.mem s_rootless ignored_files)
     then None
     else 
     let rootless_path = Dfn_common.decompose_absolute_path_using_root ap sourcedir in 
     Some(Dfn_rootless.to_line rootless_path) ) 
   absolute_paths1;;
      

let compute_restricted_diff sourcedir destdir restrictions=
   compute_diff (sourcedir,restricted_list sourcedir restrictions) destdir;;
   

   


let commands_for_update (source_dir,destination_dir) diff=
   if Dircopy_diff.is_empty diff
   then []
   else 
   let s_destination=Dfa_root.connectable_to_subpath destination_dir in
   let created_ones=Dircopy_diff.recently_created diff  in
   let temp2=Option.filter_and_unpack
   (fun fn->
     if String.contains fn '/'
     then let dn=Cull_string.before_rightmost fn '/' in
          Some("mkdir -p "^s_destination^dn)
     else None 
    )
   created_ones in
   let temp3=Erdurod.sort Total_ordering.silex_for_strings temp2 in
   let s_source=Dfa_root.connectable_to_subpath source_dir in
   let temp4=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^(Cull_string.before_rightmost fn '/')
   ) created_ones in
   let changed_ones=Dircopy_diff.recently_changed diff in
   let temp5=Image.image(
      fun fn->
      "cp "^s_source^fn^" "^s_destination^fn
   ) changed_ones in
   let temp7=Image.image(
      fun fn->
      "rm "^s_destination^fn
   ) (Dircopy_diff.recently_deleted diff) in
   (temp3@temp4@temp5@temp7);;  
   
              
(*

#use"lib/Java_project/jpr_main.ml";;

*) 



module Private = struct

let files_in_project =Memoized.make(fun (Jpr_types.Pr(path)) -> 
  let dir = Directory_name.of_string path in 
  let files_or_dirs = Unix_again.complete_ls dir in  
   List.filter (fun ap->
  not(Unix_again.is_a_directory ap)
) files_or_dirs);; 


let java_files_in_project = Memoized.make(fun 
	jproj -> List.filter_map (fun ap->
  let s = Absolute_path.to_string ap in 
  if Supstring.ends_with s ".java"
  then Some(Jpr_types.Jf(s))
  else None
) (files_in_project jproj) );; 

let extract_source_from_java_filename (Jpr_types.Jf(s)) =
   let i1 = Substring.rightmost_index_of_in "/src/" s in
   Jpr_types.Jsrc(Cull_string.beginning (i1-1) s);;
     
let order_for_java_sources = (
   (fun (Jpr_types.Jsrc jsrc1) (Jpr_types.Jsrc jsrc2) ->
   	 Total_ordering.silex_for_strings jsrc1 jsrc2
   ) : Jpr_types.java_source Total_ordering_t.t
) ;;

let sources_in_project =Memoized.make(fun  
   jproj ->
   Ordered.sort order_for_java_sources
     (Explicit.image extract_source_from_java_filename 
        (java_files_in_project jproj)) 
 );; 
  

end ;;

let sources_in_project = Private.sources_in_project ;; 
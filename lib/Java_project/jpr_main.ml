(*

#use"lib/Java_project/jpr_main.ml";;

*) 



module Private = struct

let order_for_java_sources = (
   (fun (Jpr_types.Jsrc jsrc1) (Jpr_types.Jsrc jsrc2) ->
   	 Total_ordering.silex_for_strings jsrc1 jsrc2
   ) : Jpr_types.java_source Total_ordering_t.t
) ;;

let order_for_java_classnames = (
   (fun (Jpr_types.Jcn jcn1) (Jpr_types.Jcn jcn2) ->
   	 Total_ordering.lex_for_strings jcn1 jcn2
   ) : Jpr_types.java_classname Total_ordering_t.t
) ;;


let order_for_subdirs = (
   (fun (Jpr_types.Jsbd jsbd1) (Jpr_types.Jsbd jsbd2) ->
   	 Total_ordering.silex_for_strings jsbd1 jsbd2
   ) : Jpr_types.java_subdir Total_ordering_t.t
) ;;

let order_for_packages = (
   (fun (Jpr_types.Jpkg jpkg1) (Jpr_types.Jpkg jpkg2) ->
   	 Total_ordering.silex_for_strings jpkg1 jpkg2
   ) : Jpr_types.java_package Total_ordering_t.t
) ;;

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
     

let sources_in_project =Memoized.make(fun  
   jproj ->
   Ordered.sort order_for_java_sources
     (Explicit.image extract_source_from_java_filename 
        (java_files_in_project jproj)) 
 );; 
  
let files_under_source = Memoized.make(fun 
	(Jpr_types.Jsrc path) ->
   let dir = Directory_name.of_string path in 
   let all_paths = Unix_again.complete_ls dir in 
   List.filter (fun x->not(Unix_again.is_a_directory x))
      all_paths );;


let java_files_under_source = 
   Memoized.make(fun src ->
    let all_files = files_under_source src in 
   List.filter_map (
   	fun ap->
  	 let s = Absolute_path.to_string ap in 
  	 if Supstring.ends_with s ".java"
  	 then Some(Jpr_types.Jf(s))
  	 else None) 
    all_files) ;;

let extract_java_classname_from_filename (Jpr_types.Jf(s)) = 
  let s2 = Cull_string.after_rightmost s '/' in 
  Jpr_types.Jcn(Cull_string.before_rightmost s2 '.');; 



let java_classnames_under_source = 
   Memoized.make(fun src ->
    let java_files = java_files_under_source src in 
    let classnames_inpo = 
       Explicit.image extract_java_classname_from_filename java_files in 
     Ordered.sort order_for_java_classnames 
      classnames_inpo
   ) ;;

let paths_for_classname src cname = 
   List.filter (fun ap ->
      extract_java_classname_from_filename ap = cname
   ) (java_files_under_source src) ;; 


let table_for_classname_paths_under_source = 
   Memoized.make(fun src ->
    let java_classes = java_classnames_under_source src in 
    Image.image (
   fun cname->(cname,paths_for_classname src cname)
) java_classes 
   ) ;;

let table_for_filecontents = 
  Memoized.make(fun jproj ->
  Chronometer.it (Explicit.image (
  fun fn ->(fn,Io.read_whole_file fn)
)) (files_in_project jproj) 
);; 

let java_classnames_in_project 
  = Memoized.make(fun jproj->
   let temp1 = Image.image 
     java_classnames_under_source 
      (sources_in_project jproj) in 
   Ordered.fold_merge
      order_for_java_classnames
    temp1 
);; 	



let current_java_project_data =  

   let seed = Jpr_constant.spring_5_3_with_boot_2_7 in 
   ref (
     seed,
     None,
     None,
     None 
   );;

let compute_and_remember_table_for_filecontents () = 
	let (seed,_fcs,cns,sbds) = !current_java_project_data in 
    let new_fcs =table_for_filecontents(seed) in 
    let _ = (current_java_project_data:=(seed,Some(new_fcs),cns,sbds)) in 
    new_fcs ;;  

let compute_and_remember_classnames () = 
	let (seed,fcs,_cns,sbds) = !current_java_project_data in 
    let new_cns =java_classnames_in_project(seed) in 
    let _ = (current_java_project_data:=(seed,fcs,Some(new_cns),sbds)) in 
    new_cns ;;  


let current_java_project_root () = 
  (fun (seed,_fcs,_cns,_sbds)->seed) 
     (!current_java_project_data) ;;  

let current_table_for_filecontents () = 
   match (fun (_seed,fcs,_cns,_sbds)->fcs) (!current_java_project_data) with 
   (Some already_computed) ->
       already_computed
   |None -> compute_and_remember_table_for_filecontents ();;
 
let current_classnames () = 
   match (fun (_seed,_fcs,cns,_sbds)->cns)(!current_java_project_data) with 
   (Some already_computed) ->
       already_computed
   |None -> compute_and_remember_classnames ();;


let file_contents = Memoized.make(fun fn ->
   List.assoc fn (current_table_for_filecontents())
) ;;

let package_from_line_opt untrimmed_line = 
  let line = Cull_string.trim_spaces untrimmed_line in 
  let beginning = "package " 
  and ending = ";" in 
  if not(
  	(Supstring.begins_with line beginning) &&
    (Supstring.ends_with line ending) 
  )
  then None 
  else
  let line2 = Cull_string.two_sided_cutting
        (beginning,ending) line in 
  let line3 = Cull_string.trim_spaces line2 in       
  Some(Jpr_types.Jpkg line3);;


exception Extract_package_from_text_exn ;;
   
let extract_package_from_text text = 
    let lines = Lines_in_string.lines text in 
    match List.find_map package_from_line_opt lines with 
     None -> raise Extract_package_from_text_exn 
    |Some (pn) -> pn ;;  

exception Extract_package_from_java_file_exn of string ;;
 
let extract_package_from_java_file =Memoized.make(fun 
	 (Jpr_types.Jf jf) ->
  let ap = Absolute_path.of_string jf in 	  
  try extract_package_from_text (file_contents ap) with 
  Extract_package_from_text_exn ->
   raise(Extract_package_from_java_file_exn(jf) ) );;
   

let extract_subdir_from_java_file (Jpr_types.Jf path) = 
  Jpr_types.Jsbd(Cull_string.before_rightmost path '/') ;;   
   
let subdirs_and_packages = Memoized.make (fun jproj ->
  Explicit.image 
  (fun jf ->(
     extract_subdir_from_java_file jf
     ,extract_package_from_java_file jf) )
     (java_files_in_project jproj) );; 
   

let all_subdirs_in_project = Memoized.make (fun jproj ->
   Ordered.sort order_for_subdirs 
     (Image.image fst (subdirs_and_packages jproj)) );;

let compute_and_remember_subdirs () = 
	let (seed,fcs,cns,_sbds) = !current_java_project_data in 
    let new_sbds =all_subdirs_in_project(seed) in 
    let _ = (current_java_project_data:=(seed,fcs,cns,Some(new_sbds))) in 
    new_sbds ;;  

 
let current_subdirs () = 
   match (fun (_seed,_fcs,_cns,sbds)->sbds) (!current_java_project_data) with 
   (Some already_computed) ->
       already_computed
   |None -> compute_and_remember_subdirs ();;
 

let import_from_line_opt untrimmed_line = 
  let line = Cull_string.trim_spaces untrimmed_line in 
  let beginning = "import " 
  and ending = ";" in 
  if not(
  	(Supstring.begins_with line beginning) &&
    (Supstring.ends_with line ending) 
  )
  then None 
  else
  let line2 = Cull_string.two_sided_cutting
        (beginning,ending) line in 
  let line3 = Cull_string.trim_spaces line2 in   
  let (a,b)= Cull_string.split_wrt_rightmost line3 '.' in 
  Some(Jpr_types.Jiprt(Jpr_types.Jpkg a,Jpr_types.Jcn b));;      

let extract_imports_from_text text = 
    let lines = Lines_in_string.lines text in 
    List.filter_map import_from_line_opt lines ;;

let extract_imports_from_java_file =Memoized.make(fun 
	(Jpr_types.Jf jf) -> 
  let ap = Absolute_path.of_string jf in 
  extract_imports_from_text (file_contents ap)
);;

let imported_packages_in_java_file =Memoized.make(fun jf -> 
  Ordered.sort order_for_packages
  (Image.image (fun (Jpr_types.Jiprt(pkg,_))->pkg) 
   (extract_imports_from_java_file jf))
);;

exception Java_separator_in_subdirname of string ;;

let java_separator_in_subdirname subdir = 
  match List.find_opt (
    Supstring.contains subdir
 )	["/java/";"/javaTemplate/";"/javaTemplates/"] with 
 None -> raise (Java_separator_in_subdirname(subdir))
 |Some sep -> sep ;; 

let natural_subdir_to_package (Jpr_types.Jsbd subdir) = 
   let sep = java_separator_in_subdirname subdir in 
   let ocs = Substring.occurrences_of_in sep subdir in 
   let last_oc = List.hd(List.rev ocs) in 
   let offset = String.length(sep)-1 in 
   let after_sep = Cull_string.cobeginning (last_oc+offset) subdir  in 
   Jpr_types.Jpkg(Replace_inside.replace_inside_string 
    ~display_number_of_matches:false
     ("/",".") after_sep) ;;

(*

observed experimentally, perhaps it's a mistake in the original code

*)
let war_reactor_example = Jpr_types.Jsbd
    ("spring-boot-2.7.x/spring-boot-project/"^
   	"spring-boot-tools/spring-boot-maven-plugin/src/intTest/"^
   	"projects/war-reactor/war/src/main/java/com/example") ;; 

let subdir_to_package subdir = 
   if subdir = war_reactor_example 
   then Jpr_types.Jpkg "org.test"
   else natural_subdir_to_package subdir ;;

let imported_packages_in_project 
  = Memoized.make(fun jproj->
   let temp1 = Image.image 
     imported_packages_in_java_file 
      (java_files_in_project jproj) in 
   Ordered.fold_merge
      order_for_packages
    temp1 
);; 	

 let subdirs_for_package jproj pkg=
   List.filter (
       fun sbd -> (subdir_to_package sbd) = pkg
   )(all_subdirs_in_project jproj) ;;  

 let table_for_package_location 
   = Memoized.make(fun jproj->
   Explicit.image 
     (fun pkg->(pkg,subdirs_for_package jproj pkg))
      (imported_packages_in_project jproj) 
 );; 	


end ;;

let sources_in_project = Private.sources_in_project ;; 
let table_for_package_location = Private.table_for_package_location ;;
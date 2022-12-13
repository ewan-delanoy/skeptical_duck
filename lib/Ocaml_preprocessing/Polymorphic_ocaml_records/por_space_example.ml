(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_space_example.ml";;

*)

module Private = struct 

  let decode_pair_of_converters s =
    match String.index_opt s '#' with 
     None -> None 
   |Some idx -> Some(Cull_string.beginning idx s,Cull_string.cobeginning (idx+1) s) ;;
 
   let field_constructor  (a,b,c,d,e) = {
     Por_field_t.field_name = a ;
     field_type = b ;
     var_name =c ;
     default_value = d ;
     crobj_converters = decode_pair_of_converters e ;
  } ;;

let field_list_constructor l = Image.image field_constructor l;;


let fields_for_gw_configuration = field_list_constructor [
 "root","Dfa_root_t.t","r","Dfa_root.of_line \"dummy\"","Dfa_root.of_concrete_object#Dfa_root.to_concrete_object";
 "ignored_subdirectories","Dfa_subdirectory_t.t list","ign_subdirs","[]","Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object#Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object";
 "ignored_files","Dfn_rootless_t.t list","ign_files","[]","Crobj_converter_combinator.to_list Dfn_rootless.of_concrete_object#Crobj_converter_combinator.of_list Dfn_rootless.to_concrete_object"     ;
] ;; 

let fields_for_gw_life_watcher = field_list_constructor [
 "watched_files", "(Dfn_rootless_t.t * string) list","files","[]","Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Crobj_converter.string_of_concrete_object#Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Crobj_converter.string_to_concrete_object";
] ;; 

let fields_for_gw_with_archives = field_list_constructor [
 "subdirs_for_archived_mlx_files","Dfa_subdirectory_t.t list","archives_subdirs","[]","Crobj_converter_combinator.to_list Dfa_subdirectory.of_concrete_object#Crobj_converter_combinator.of_list Dfa_subdirectory.to_concrete_object";
] ;; 

let fields_for_gw_with_small_details = field_list_constructor [
 "small_details_in_files","(Dfn_rootless_t.t * Fw_file_small_details_t.t) list","small_details","[]","Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Fw_file_small_details.of_concrete_object#Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Fw_file_small_details.to_concrete_object";
] ;; 

let fields_for_gw_with_dependencies = field_list_constructor [
 "index_for_caching", "Fw_instance_index_t.t * Fw_state_index_t.t", "cache_idx", "(Fw_indexer.make_full_instance ())", "";
] ;; 

let fields_for_gw_with_batch_compilation = field_list_constructor [
 "last_compilation_result_for_module","(Dfa_module_t.t * bool) list","compilation_results","[]","Crobj_converter_combinator.to_pair_list Dfa_module.of_concrete_object Crobj_converter.bool_of_concrete_object#Crobj_converter_combinator.of_pair_list Dfa_module.to_concrete_object Crobj_converter.bool_to_concrete_object" ;
] ;; 

let fields_for_gw_with_githubbing = field_list_constructor [
 "dir_for_backup","Dfa_root_t.t","backup_dir","Dfa_root.of_line \"dummy\"","Dfa_root.of_concrete_object#Dfa_root.to_concrete_object";
 "gitpush_after_backup","bool","gab","false","Crobj_converter.bool_of_concrete_object#Crobj_converter.bool_to_concrete_object";
 "github_url","string","url","\"\"","Crobj_converter.string_of_concrete_object#Crobj_converter.string_to_concrete_object";
 "encoding_protected_files","(Dfn_rootless_t.t * Dfn_rootless_t.t) list","protected_pairs","[]","Crobj_converter_combinator.to_pair_list Dfn_rootless.of_concrete_object Dfn_rootless.of_concrete_object#Crobj_converter_combinator.of_pair_list Dfn_rootless.to_concrete_object Dfn_rootless.to_concrete_object";
] ;; 



let first_base =  [
"gw_configuration",fields_for_gw_configuration ;
"gw_life_watcher",fields_for_gw_life_watcher ;
"gw_with_archives",fields_for_gw_with_archives ;
"gw_with_small_details",fields_for_gw_with_small_details ;
"gw_with_dependencies",fields_for_gw_with_dependencies ;
"gw_with_batch_compilation",fields_for_gw_with_batch_compilation ;
"gw_with_githubbing",fields_for_gw_with_githubbing ;
] ;; 



let all_fields = List.flatten (Image.image snd first_base) ;;

let cumulative_first_base =
let temp1 = Three_parts.generic first_base in 
List.rev_map (fun (b,a,_)->
  let ttemp3 = List.rev_map snd (a::b) in
  (fst a,List.flatten ttemp3) 
  ) temp1 ;;

let root_field = List.find (fun fd->
  fd.Por_field_t.field_name = "root"
) fields_for_gw_configuration ;;

let second_base = [
  "gw_guthib_configuration",root_field :: fields_for_gw_with_githubbing ;
] ;; 

let full_base =  cumulative_first_base @ second_base ;;     





let list_for_constructors = 
   ["gw_configuration";"gw_guthib_configuration"] ;;

let list_for_extensions =   
   ["gw_configuration","gw_life_watcher"] ;;

let list_for_parenting =
[
  "gw_with_archives","gw_life_watcher";
  "gw_with_small_details","gw_with_archives";                       
  "gw_with_dependencies","gw_with_small_details";
  "gw_with_batch_compilation","gw_with_dependencies";
  "gw_with_githubbing","gw_with_batch_compilation";
] ;;

let list_for_restrictions = 
  ["gw_guthib_configuration";"gw_configuration"] ;;

let subclass_list_constructor l = Image.image (
  fun (scl_name,scl_fields) -> {
    Por_subclass_t.subclass_name = scl_name ;
    subclass_fields = scl_fields ;
    parent = List.assoc_opt scl_name list_for_parenting;
    extensions_leading_here = 
      More_option.filter_and_unpack (
        fun (x,y)->
          if y=scl_name  
          then Some x 
          else None  
      ) list_for_extensions ;
    has_restriction = List.mem scl_name list_for_restrictions;
    has_constructor = List.mem scl_name list_for_constructors;
  }
) l;;

let field_order = ((fun fld1 fld2 ->
  let trial1 = Total_ordering.lex_for_strings 
     fld1.Por_field_t.field_name fld2.Por_field_t.field_name in 
  if trial1<> Total_ordering_result_t.Equal then trial1 else
     Total_ordering.standard fld1 fld2         
) : Por_field_t.t Total_ordering_t.t);;

let fields_in_order = Ordered.sort field_order all_fields ;; 

let select_field fd_name =
   List.find (
    fun fd -> fd.Por_field_t.field_name = fd_name
   ) 
   fields_in_order ;;
  
let unabridged_subclasses = subclass_list_constructor full_base ;; 

let full_example = 
  let home = Sys.getenv "HOME" in 
  let file_there = (fun s-> 
    Absolute_path.create_file_if_absent(home^"/Teuliou/OCaml/skeptical_duck/lib/Filewatching/"^s^".ml")) in 
 {
   Por_space_t.main_type_name = "t" ;
   module_name = "gw_poly" ;
   subclasses = subclass_list_constructor full_base ;
   type_signature_file = (file_there "gw_poly_t") ;
   implementation_file = (file_there "gw_poly") ;
   has_crobj_conversion = true ;
   incomplete_extensions = [];
} ;;  

let bare_filewatching = 
  let home = Sys.getenv "HOME" in 
  let file_there = (fun s-> 
    Absolute_path.create_file_if_absent(home^"/Teuliou/OCaml/skeptical_duck/lib/Filewatching/"^s^".ml")) in 
 {
   Por_space_t.main_type_name = "t" ;
   module_name = "gw_poly" ;
   subclasses = [] ;
   type_signature_file = (file_there "gw_poly_t") ;
   implementation_file = (file_there "gw_poly") ;
   has_crobj_conversion = true ;
   incomplete_extensions = [];
} ;;  

end ;;   

let full_example = Private.full_example ;; 

let filewatching = 
  let home = Sys.getenv "HOME" in 
  let file_there = (fun s-> 
    Absolute_path.create_file_if_absent(home^"/Teuliou/OCaml/skeptical_duck/lib/Filewatching/"^s^".ml")) in 
 ref({
   Por_space_t.main_type_name = "t" ;
   module_name = "gw_poly" ;
   subclasses = [] ;
   type_signature_file = (file_there "gw_poly_t") ;
   implementation_file = (file_there "gw_poly") ;
   has_crobj_conversion = true ;
   incomplete_extensions = [];
}) ;;  


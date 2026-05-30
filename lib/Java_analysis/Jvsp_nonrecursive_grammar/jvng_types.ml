(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_types.ml";;

*)

type ancestry_data = {
   ancestry : Jvng_duplicated_name.t list ;
   position_in_birth_list : int ;
} ;;

type ancestry_manager = AM of (Jvng_duplicated_name.t * ancestry_data) list ;;

type grammar_with_ancestry_manager = {
   source : Jvag_types.grammar ; 
   receiver : Jvng_jvag_types.grammar ;
   manager : ancestry_manager ;
} ;;

type stream = {
   cursor : int ;
   remaining_list : Jvsp_token_types_list.t ;
} ;;

type local_analizer = {
  first_approach : (Jvsp_types.token_type_list -> string option) ;
  case_by_case : (((Jvsp_types.token_type list)*string) list) ; 
  analysis_is_complete : bool ;
};;

type battery_of_analizers = {
  deciders_for_optionals_or_stars : (string  * local_analizer) list ;  
  choosers_for_disjunctions : (string * local_analizer) list;
} ;;
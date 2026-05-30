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
  first_approach : (Jvsp_types.token_type_list -> Jvng_duplicated_name.t option) ;
  case_by_case : (((Jvsp_types.token_type list)*Jvng_duplicated_name.t) list) ; 
  analysis_is_complete : bool ;
};;

type battery_of_analizers = {
  deciders_for_optionals_or_stars : (Jvng_duplicated_name.t  * local_analizer) list ;  
  choosers_for_disjunctions : (Jvng_duplicated_name.t * local_analizer) list;
  precomputed_first_tokens : ( Jvng_duplicated_name.t * (Jvsp_types.token_type list)) list;
} ;;

type global_analizer = {
  managed_grammar : grammar_with_ancestry_manager ;
  battery : battery_of_analizers ;
  head : Jvng_duplicated_name.t ;
  tail : Jvng_duplicated_name.t list ;
  consumable : stream ;
} ;;

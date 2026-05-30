(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_types.ml";;

*)

type ancestry_data = {
   ancestry : Jvng_duplicated_name.t list ;
   position_in_birth_list : int ;
} ;;

type ancestry_manager = AM of (Jvng_duplicated_name.t * ancestry_data) list ;;

type with_ancestry_manager = {
   source : Jvag_types.grammar ; 
   receiver : Jvng_jvag_types.grammar ;
   manager : ancestry_manager ;
} ;;

type stream = {
   cursor : int ;
   remaining_list : Jvsp_token_types_list.t ;
} ;;

type local_analizer = Rm of 
  (Jvsp_types.token_type_list -> string option) * 
  (((Jvsp_types.token_type list)*string) list) * 
   bool;;
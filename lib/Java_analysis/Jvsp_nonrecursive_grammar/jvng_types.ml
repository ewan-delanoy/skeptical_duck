(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_types.ml";;

*)

type ancestry_data = {
   ancestry : Jvng_duplicated_name.t list ;
   position_in_birth_list : int ;
}

type with_ancestry_manager = {
   source : Jvag_types.grammar ; 
   receiver : Jvng_jvag_grammar.t ;
   ancestry_manager : (Jvng_duplicated_name.t * ancestry_data) list ;
} ;;

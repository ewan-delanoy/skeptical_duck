(*

#use"lib/Cee_language/cee_inclusion_item_t.ml";;

*)


type  t = {
 line_number : int ;
 inclusion_index : int ;
 included_file_opt : string option; 
} ;; 
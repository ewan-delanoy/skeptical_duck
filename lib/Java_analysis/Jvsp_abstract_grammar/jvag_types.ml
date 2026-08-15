(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_types.ml";;

*)

     
type form = 
    Optional of string 
   |Molecular of Jvsp_types.token_type list
   |Concat of string list 
   |Disjunction of string list 
   |Star of string 
   |Synonym of string
  ;;

type grammar = AL of  ((string * form) list) ;; 

type location_in_disjunction = I of int | N of string ;;


type local_modification = 
   Lm_collapse_synonym of int 
       (*
       collapse_synonym gf (index_in_disj) replaces the line located at index_in_disj 
        (chich must be a synonym form) with its older synonym according to the grammar rules.
       *)
  |Lm_remove_left_recursive_line_in_disjunction of string * int 
   (*
     remove_left_recursive_line_in_disjunction gf original_name index_in_disj make a global transformation on the value associated with
     original_name key in the grammar (this value must be a disjunction, whose index_in_disjth element expands
     to a concatentaion starting with original_name, in other words a left recursion, of the form
     original_name -> original_name A. The left recursion is removed in a standard way, by putting
     original_name = original_name' Starred(A).
   *)
  |Lm_expand_point_in_line of location_in_disjunction * int
      (* expand_point_in_line gf (lid,index_in_concat) expands the point at index index_in_concat
      of the line defined by lid. If this point is a Dijsunction, this line will be replaced by
      more lines. If this point is a Concat, a Synonym or a Molecule, it will be replaced by a longer line. *)  
 |Lm_reunite_in_concatenation of location_in_disjunction * (int * int)  
      (*
      implode_molecule gf (lid,(range_start,range_end)) replaces line defined by lid
      with a shorter line, by mergeing all the elements all the elements in the range 
      described by (range_start,range_end) into a Molecular or a Concat. *)  
 |Lm_reunite_in_disjunction of location_in_disjunction * location_in_disjunction
   (*
      reunite_in_disjunction gf (lid_start,lid_end) collapses the lines in the range described
     by (lid_start,lid_end) into a single line, by factoring left and right and mergeing all
     the center elements into an Optional, a Star or a Disjunction.
   *)
;;


type modification = 
   Set_production of string * form
  |Create_production of string * form  
  |Rename of string * string 
  |Remove_productions of string list
  |Expand_in_disjunction of string * string 
  |Expand_in_synonym of string * string
  |Collapse_synonym_locally of string * string
  |Collapse_synonym_globally of string 
  |Local of string * (local_modification list) ;;

type nonrecursive_grammar = {
   sons_and_fathers : (string * string) list ;
   productions : (string * (form * string list)) list;
} ;;

type link =
    Optional_L 
   |Concat_L
   |Disjunction_L 
   |Star_L
   |Synonym_L
  ;;

type thumbnail = Tn of (string list) list ;;
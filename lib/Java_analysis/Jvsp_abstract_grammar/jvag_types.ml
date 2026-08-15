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
  |Lm_reunite_optional of int * (int * int)
      (*
        reunite_optional gf (index_in_disj,(length_before,length_after)) checks that lines
        number index_in_disj and index_in_disj+1 in the disjunction list are of
        the form BA,B name A (not necessarily in that order), and if so, merges them
        into a single line BOptional(name)A. The length of B is length_before and the length of A is length_after.
      *)
  |Lm_expand_disjunction of int * int 
       (*
       expand_disjunction gf (index_in_disj,index_in_concat) expands line number index_in_disj
       into more lines, by expanding the element number index_in_concat (which must be a disjunction) in
       that line.
       *)
  |Lm_expand_synonym of int * int 
       (*
       expand_synonym gf (index_in_disj,index_in_concat) replaces the element located at
       (index_in_disj,index_in_concat) with its older synonym according to the grammar rules.
       *)
  |Lm_expand_concat of int * int  
      (*
       expand_concat gf (index_in_disj,index_in_concat) replaces line number index_in_disj
       with a longer line, by expanding the element number index_in_concat (which must be a concatenation) in
       that line.
       *)
  |Lm_explode_molecule of int * int 
      (*
       explode_molecule gf (index_in_disj,index_in_concat) replaces line number index_in_disj
       with a longer line, by exploding the element number index_in_concat (which must be a molecule) in
       that line.
       *) 
  |Lm_implode_concat of int * (int * int)
      (*
      implode_concat gf (index_in_disj,(range_start,range_end)) replaces line number index_in_disj
      with a shorter line, by replacing all the elements in the range defined by (range_start,range_end) inside
      that line with a single concatenation form. 
      *)
  |Lm_implode_molecule of int * (int * int)  
      (*
      implode_molecule gf (index_in_disj,(range_start,range_end)) replaces line number index_in_disj
      with a shorter line, by replacing all the elements in the range defined by (range_start,range_end) inside
      that line with a single molecule. 
      *)
  |Lm_reunite_star of int * (int * int)
       (*
        reunite_star gf (index_in_disj,(length_before,length_after)) checks that lines
        number index_in_disj and index_in_disj+1 in the disjunction list are of
        the form BA,B name Starred(name)A or B Starred(name) name A  (not necessarily in that order), and if so, merges them
        into a single line BStarred(name)A. The length of B is length_before and the length of A is length_after.
      *)
  |Lm_remove_left_recursive_line_in_disjunction of string * int 
   (*
     remove_left_recursive_line_in_disjunction gf original_name index_in_disj make a global transformation on the value associated with
     original_name key in the grammar (this value must be a disjunction, whose index_in_disjth element expands
     to a concatentaion starting with original_name, in other words a left recursion, of the form
     original_name -> original_name A. The left recursion is removed in a standard way, by putting
     original_name = original_name' Starred(A).
   *)
 |Lm_guantanamera of location_in_disjunction * location_in_disjunction
   (*
      guantanamera gf (lid_start,lid_end) collapses the lines in the range described
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
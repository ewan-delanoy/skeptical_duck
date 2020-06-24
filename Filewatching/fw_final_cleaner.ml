(*

#use"Filewatching/fw_final_cleaner.ml";;

*)

module Private = struct 

let salt = "Fw_"^"final_cleaner_t.";;

type t ={
  linked_dependencies : (Dfa_ending_t.t * Dfa_ending_t.t) list;
  reserved_terminations : string list;
  git_ignored_files : Dfn_rootless_t.t list;
};;

let linked_dependencies_label   = salt ^ "linked_dependencies";;
let reserved_terminations_label = salt ^ "reserved_terminations";;
let git_ignored_files_label     = salt ^ "git_ignored_files";;


let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj 
   and of_crobj = Dfa_ending.of_concrete_object in
   {
      Fw_final_cleaner_t.linked_dependencies = 
        Concrete_object_field.to_pair_list of_crobj of_crobj (g linked_dependencies_label);
      reserved_terminations = 
        Concrete_object_field.to_string_list (g reserved_terminations_label);
      git_ignored_files = 
        Concrete_object_field.to_list Dfn_rootless.of_concrete_object (g git_ignored_files_label);    
   };; 

let to_concrete_object cleaner=
   let to_crobj = Dfa_ending.to_concrete_object in
   let items= 
   [
    linked_dependencies_label, Concrete_object_field.of_pair_list to_crobj to_crobj cleaner.Fw_final_cleaner_t.linked_dependencies;
    reserved_terminations_label, Concrete_object_field.of_string_list cleaner.Fw_final_cleaner_t.reserved_terminations;
    git_ignored_files_label, Concrete_object_field.of_list Dfn_rootless.to_concrete_object cleaner.Fw_final_cleaner_t.git_ignored_files;
    
   ]  in
   Concrete_object_t.Record items;;

end ;; 

let constructor (l1,l2,l3)= {
  Fw_final_cleaner_t.linked_dependencies = l1;
  reserved_terminations = l2;
  git_ignored_files = l3;    
};;

let default = 
  constructor(Coma_constant.endings_for_cleaning,
               Coma_constant.reserved_terminations,
                Coma_constant.git_ignored_files);;

let of_concrete_object = Private.of_concrete_object;;
let to_concrete_object = Private.to_concrete_object;;


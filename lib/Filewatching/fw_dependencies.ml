(*

#use"lib/Filewatching/fw_dependencies.ml";;

*)

module Crobj = struct
  
let salt = "Fw_dependencies_t." ;;
let modularized_details_label = salt ^ "modularized_details" ;;
let order_label               = salt ^ "order" ;;
let needed_dirs_label         = salt ^ "needed_dirs" ;;
let needed_libs_label         = salt ^ "needed_libs" ;;
let all_subdirectories_label  = salt ^ "all_subdirectories" ;;
let all_printables_label      = salt ^ "all_printables" ;;





let of_concrete_object ccrt_obj = 
  let g=Concrete_object.get_record ccrt_obj 
  and cl = Crobj_converter_combinator.to_list in 
  let h = (fun converter label ->
   Crobj_converter_combinator.to_pair_list 
   Dfa_module.of_concrete_object converter  
   (g label)
  ) in 
  let hl = (fun elt_converter label ->
   h (Crobj_converter_combinator.to_list elt_converter) label

  )
  and mlist_of_crobj = Crobj_converter_combinator.to_list 
  Dfa_module.of_concrete_object  in 
  let mlpair_of_crobj = Crobj_converter_combinator.to_pair
  mlist_of_crobj mlist_of_crobj in
 {
   Fw_dependencies_t.modularized_details = 
       h Fw_module_small_details.Crobj.of_concrete_object modularized_details_label;
   order = h mlpair_of_crobj order_label;
   needed_dirs = hl Dfa_subdirectory.of_concrete_object needed_dirs_label;
   needed_libs = hl Ocaml_library.of_concrete_object needed_libs_label;
   all_subdirectories = cl Dfa_subdirectory.of_concrete_object   (g all_subdirectories_label);
   all_printables =  cl Dfn_middle.of_concrete_object  (g all_printables_label);
 } ;;


let to_concrete_object fwd = 
  let cl = Crobj_converter_combinator.of_list in 
  let h = (fun converter pair_list ->
   Crobj_converter_combinator.of_pair_list 
   Dfa_module.to_concrete_object converter  
   pair_list
  ) in 
  let hl = (fun elt_converter complicated_list ->
   h (Crobj_converter_combinator.of_list elt_converter) complicated_list
  )
  and mlist_to_crobj = Crobj_converter_combinator.of_list 
  Dfa_module.to_concrete_object  in 
  let mlpair_to_crobj = Crobj_converter_combinator.of_pair
  mlist_to_crobj mlist_to_crobj in
  let items=
 [
   modularized_details_label, h Fw_module_small_details.Crobj.to_concrete_object   (fwd.Fw_dependencies_t.modularized_details);
   order_label,  h mlpair_to_crobj   (fwd.Fw_dependencies_t.order);
   needed_dirs_label,  hl Dfa_subdirectory.to_concrete_object   (fwd.Fw_dependencies_t.needed_dirs);
   needed_libs_label,  hl Ocaml_library.to_concrete_object   (fwd.Fw_dependencies_t.needed_libs);
   all_subdirectories_label, cl Dfa_subdirectory.to_concrete_object   (fwd.Fw_dependencies_t.all_subdirectories);
   all_printables_label, cl Dfn_middle.to_concrete_object   (fwd.Fw_dependencies_t.all_printables);
 ] in 
Concrete_object_t.Record items;;

end ;;  

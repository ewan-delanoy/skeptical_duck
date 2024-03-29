(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_space.ml";;

*)

exception Duplicate_subclass_name of string ;;


module Private = struct 

  let write_to_type_signature_file (por:Por_space_t.t) = 
      let text = Por_main_type_definition.text por 
      and file = por.Por_space_t.type_signature_file in 
      Io.overwrite_with file text ;;

  let initial_comment_in_implementation_file por =
       let ap = por.Por_space_t.implementation_file 
       and root = Coma_big_constant.This_World.root in 
       let s_ap=Absolute_path.to_string ap in 
       let s_cdir=Dfa_root.connectable_to_subpath root in 
       let shortened_path=Cull_string.cobeginning (String.length s_cdir) s_ap in 
       "(*\n\n#use\""^shortened_path^"\";"^";\n\n*)\n\n\n" ;;  

  let text_for_implementation_file (por:Por_space_t.t) = 
     (initial_comment_in_implementation_file por)^
     (Por_private_component.main por)^"\n\n"^
     (Por_public_component.main por) ;;
   
  let write_to_implementation_file (por:Por_space_t.t) = 
     let text = text_for_implementation_file por 
     and file = por.Por_space_t.implementation_file in 
     Io.overwrite_with file text ;;

  let write por = 
    (
      write_to_type_signature_file por;
      write_to_implementation_file por
    ) ;;
     
let add_subclass_on_nonref old_por scl = 
      let old_subclasses = old_por.Por_space_t.subclasses 
      and scl_name = scl.Por_subclass_t.subclass_name  in 
      if (Por_common.get_subclass_opt old_por scl_name)<>None 
      then raise(Duplicate_subclass_name scl_name)
      else     
      let initial_complete_subclasses = old_subclasses @ [scl] 
      and initial_incomplete_subclasses = old_por.Por_space_t.incomplete_extensions in 
      let (final_complete_subclasses,final_incomplete_subclasses) =
         Por_common.exhaust_possible_linkings
         ~complete:initial_complete_subclasses
         ~incomplete:initial_incomplete_subclasses in 
      {
       old_por with 
       Por_space_t.subclasses = final_complete_subclasses ;
       incomplete_extensions = final_incomplete_subclasses ;
      }  ;;

   
let add_extension_on_nonref old_por parent_name scl = 
  match Por_common.get_subclass_opt old_por parent_name with 
   None -> let old_ie = old_por.Por_space_t.incomplete_extensions in 
           {
              old_por with 
              Por_space_t.incomplete_extensions = (parent_name,scl) :: old_ie ;
           }
  |Some parent_subclass ->
     let final_child = 
        Por_common.link_extension 
          ~parent:parent_subclass ~incomplete_child:scl in         
      add_subclass_on_nonref old_por final_child;;

     
let add_dependencies_on_nonref old_por new_deps = 
  let old_deps = old_por.Por_space_t.dependencies  in 
  {
    old_por with 
         Por_space_t.dependencies = old_deps @ new_deps ;
  }  ;;
  


end ;;   

let add_dependencies por_ref new_deps = 
  let old_por = (!por_ref) in 
  por_ref := (Private.add_dependencies_on_nonref old_por new_deps) ;;

let add_extension por_ref parent_name scl = 
  let old_por = (!por_ref) in 
  por_ref := (Private.add_extension_on_nonref old_por parent_name scl) ;;

let add_subclass por_ref scl = 
  let old_por = (!por_ref) in 
  por_ref := (Private.add_subclass_on_nonref old_por scl) ;;

let write = Private.write ;; 
(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_space.ml";;

*)

exception Duplicate_subclass_name of string ;;

module Private = struct 

  let pair_for_field (porf:Por_types.field_t) =
    (String.make 3 ' ')^
    porf.Por_types.field_name^" : "^
    porf.Por_types.field_type^" ;" ;;
  
  let initial_comment_in_type_signature_file por =
      let ap = por.Por_space_t.type_signature_file 
      and root = Coma_big_constant.This_World.root in 
      let s_ap=Absolute_path.to_string ap in 
      let s_cdir=Dfa_root.connectable_to_subpath root in 
      let shortened_path=Cull_string.cobeginning (String.length s_cdir) s_ap in 
      "(*\n\n#use\""^shortened_path^"\";"^";\n\n*)\n\n\n" ;;
  
  let text_for_type_signature_file (por:Por_space_t.t) = 
    let pairs = 
      ((String.make 3 ' ')^"type_name : string ;")::
      (Image.image pair_for_field (Por_common.all_fields por)) in 
    (initial_comment_in_type_signature_file por)^
    "type "^(por.Por_space_t.main_type_name)^" = { \n"^ 
    (String.concat "\n" pairs) ^ 
    "\n} ;;" ;;
  
  let write_to_type_signature_file (por:Por_space_t.t) = 
      let text = text_for_type_signature_file por 
      and file = por.Por_space_t.type_signature_file in 
      Io.overwrite_with file text ;;
 
  


  let private_followed_by_public por = 
     (Por_private_component.main por)^"\n\n"^
     (Por_public_component.main por)  ;;


     let initial_comment_in_implementation_file por =
       let ap = por.Por_space_t.implementation_file 
       and root = Coma_big_constant.This_World.root in 
       let s_ap=Absolute_path.to_string ap in 
       let s_cdir=Dfa_root.connectable_to_subpath root in 
       let shortened_path=Cull_string.cobeginning (String.length s_cdir) s_ap in 
       "(*\n\n#use\""^shortened_path^"\";"^";\n\n*)\n\n\n" ;;  

  let text_for_implementation_file (por:Por_space_t.t) = 
     (initial_comment_in_implementation_file por)^
     (private_followed_by_public por) ;;
   
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
      {
       old_por with 
       Por_space_t.subclasses = old_subclasses @ [scl] ;
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


end ;;   

let add_extension por_ref parent_name scl = 
  let old_por = (!por_ref) in 
  por_ref := (Private.add_extension_on_nonref old_por parent_name scl) ;;

let add_subclass por_ref scl = 
  let old_por = (!por_ref) in 
  por_ref := (Private.add_subclass_on_nonref old_por scl) ;;

let write = Private.write ;; 
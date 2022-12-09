(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_space.ml";;

*)

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
      (Image.image pair_for_field por.Por_space_t.fields) in 
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
    )
     


end ;;   

let write = Private.write ;; 
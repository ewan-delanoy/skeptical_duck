(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_main_type_definition.ml";;

*)


module Private = struct 

  let pair_for_field (porf:Por_field_t.t) =
    (String.make 3 ' ')^
    porf.Por_field_t.field_name^" : "^
    porf.Por_field_t.field_type^" ;" ;;
  
  let initial_comment_in_type_signature_file por =
      let ap = por.Por_space_t.type_signature_file 
      and root = Coma_big_constant.This_World.root in 
      let s_ap=Absolute_path.to_string ap in 
      let s_cdir=Dfa_root.connectable_to_subpath root in 
      let shortened_path=Cull_string.cobeginning (String.length s_cdir) s_ap in 
      "(*\n\n#use\""^shortened_path^"\";"^";\n\n*)\n\n\n" ;;
  
  let text (por:Por_space_t.t) = 
    let pairs = 
      ((String.make 3 ' ')^"type_name : string ;")::
      (Image.image pair_for_field (Por_common.all_fields por)) in 
    (initial_comment_in_type_signature_file por)^
    "type "^(por.Por_space_t.main_type_name)^" = { \n"^ 
    (String.concat "\n" pairs) ^ 
    "\n} ;;" ;;
  

end ;;   


let text = Private.text ;; 
(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_space.ml";;

*)

module Private = struct 

  let field_order = ((fun fld1 fld2 ->
    let trial1 = Total_ordering.lex_for_strings 
       fld1.Por_types.field_name fld2.Por_types.field_name in 
    if trial1<> Total_ordering_result_t.Equal then trial1 else
       Total_ordering.standard fld1 fld2         
  ) : Por_types.field_t Total_ordering_t.t);;

   let all_fields porsp =
      let unordered_fields = List.flatten(Image.image (
        fun scl -> Image.image (fun fd_name->
           Por_common.get_field porsp fd_name
          )
        (scl.Por_subclass_t.adbridged_subclass_fields)
      ) porsp.Por_space_t.subclasses) in 
      Ordered.sort field_order unordered_fields ;; 

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

let all_fields = Private.all_fields ;; 
let write = Private.write ;; 
(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_record_or_variant.ml";;



*)

module Private = struct 

  let ds = Particular_string.double_semicolon ;;

  let second_tab = String.make 4 ' ';;

  let broken_modulename_quote vague_modname =
    let  modname = String.capitalize_ascii  vague_modname in 
    let cut_index = (if String.length(modname)<4 then 1 else 3) in 
    let  left_part = Cull_string.beginning   cut_index modname 
    and right_part = Cull_string.cobeginning cut_index modname in 
    "\""^left_part^"\"^\""^right_part^".\"";; 

  

  let write_converters_for_record ~tab_width rov= failwith("undefined1");;
  
  let write_converter_from_crobj_for_variant rov= 
     let data = rov.Scct_record_or_variant_t.data in 
     let max_arity = snd(Max.maximize_it (fun 
     (Scct_element_in_record_or_variant_t.U(vague_variant_name,is_a_list,prod)) ->
        List.length prod
     ) data) in
     let first_arg_uple = Scct_common.arguments_in_input "arg" max_arity in   
     let first_line_in_body = "let (hook,"^first_arg_uple^")=Concrete_object_field.unwrap_bounded_variant crobj in " in 
     let old_function_body =  
     first_line_in_body
      ::(
       List.flatten(Image.image (
         Scct_element_in_record_or_variant.converter_from_crobj_in_variant
           ~module_name:rov.Scct_record_or_variant_t.modulename 
       ) data) 
     ) in 
     let addition_to_last_line = " raise(Of_concrete_object_exn(crobj)) "^ds in 
     let (temp1,temp2) = Listennou.ht (List.rev old_function_body) in 
     let function_body = List.rev ((temp1^addition_to_last_line)::temp2) in 
    [
      "exception Of_concrete_object_exn of string "^ds;
      "\n";
       "let of_concrete_object crobj = ";
    ] @ ( Image.image (fun line->second_tab^line)  function_body) 
       ;;
  
  
  let write_converters_for_variant ~tab_width rov= 
     let first_tab = String.make tab_width ' ' in 
     let data = rov.Scct_record_or_variant_t.data in 
     let broken_mod = broken_modulename_quote rov.Scct_record_or_variant_t.modulename 
     and hooks = Image.image ( fun
      (Scct_element_in_record_or_variant_t.U(vague_variant_name,is_a_list,prod)) ->
       let c_variant = String.capitalize_ascii vague_variant_name 
       and l_variant =  String.lowercase_ascii vague_variant_name  in 
       "let hook_for_"^l_variant^" = salt ^ \""^c_variant^"\" "^Particular_string.double_semicolon
     ) data in
     let lines =[
      "let salt = "^broken_mod^" "^ds ;
      "\n";
    ] @ 
     (Strung.reposition_left_hand_side_according_to_separator "=" hooks)  @
    [
      "\n";
    ] @ 
      (write_converter_from_crobj_for_variant rov)@
    [] in 
     String.concat "\n" (Image.image (fun line->first_tab^line) lines);;
     
  let write_converters ~tab_width rov=
    if rov.Scct_record_or_variant_t.is_variant 
    then  write_converters_for_variant ~tab_width rov
    else  write_converters_for_record ~tab_width rov ;; 
  
  let write_out_record ~tab_width rov= 
    let first_tab = String.make tab_width ' ' in 
    let data = rov.Scct_record_or_variant_t.data in 
    let indexed_data = Ennig.index_everything data 
    and total = List.length data in
    let middle_lines =Image.image (
     fun (idx,elt)->
       let end_part = (if idx = total then "" else " ;") in 
        second_tab ^ (Scct_element_in_record_or_variant.write_variant_in_ocaml elt) ^ end_part
    ) indexed_data in 
    let lines = [first_tab ^"{"] @
      (Image.image (fun line->first_tab^line) middle_lines )@
    ["}"] in 
   String.concat "\n" lines ;;

  let write_out_variant ~tab_width rov= 
    let first_tab = String.make tab_width ' ' in 
    let data = rov.Scct_record_or_variant_t.data in 
    let indexed_data = Ennig.index_everything data in
    let middle_lines =Image.image (
     fun (idx,elt)->
       let beg_part = (if idx = 1 then "" else "| ") in 
        second_tab ^ beg_part ^ (Scct_element_in_record_or_variant.write_variant_in_ocaml elt)
    ) indexed_data in 
    let lines = (Image.image (fun line->first_tab^line) middle_lines ) in 
   String.concat "\n" lines ;;
  
  let write_out ~tab_width rov=
    if rov.Scct_record_or_variant_t.is_variant 
    then  write_out_variant ~tab_width rov
    else  write_out_record ~tab_width rov ;; 

end ;;  

let write_converters = Private.write_converters ;;
let write_out = Private.write_out ;;

(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_record_or_variant.ml";;



*)

module Private = struct 

  
  let second_tab = String.make 4 ' ';;

  let broken_modulename_quote vague_modname =
    let  modname = String.capitalize_ascii  vague_modname in 
    let cut_index = (if String.length(modname)<4 then 1 else 3) in 
    let  left_part = Cull_string.beginning   cut_index modname 
    and right_part = Cull_string.cobeginning cut_index modname in 
    "\""^left_part^"\"^\""^right_part^"\"";; 

  let write_converters_for_record ~tab_width rov= failwith("undefined1");;
  let write_converters_for_variant ~tab_width rov= 
     let first_tab = String.make tab_width ' ' in 
     let broken_mod = broken_modulename_quote rov.Scct_record_or_variant_t.modulename in 
     let lines =[
      "let salt = "^broken_mod^" "^Particular_string.double_semicolon ;
    ] in 
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
        (Scct_element_in_record_or_variant.write_variant_in_ocaml elt) ^ end_part
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
        beg_part ^ (Scct_element_in_record_or_variant.write_variant_in_ocaml elt)
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
(*

#use"Ocaml_analysis/write_crobj_converters_for_ocaml_record_type.ml";;

*)

module Private = struct 

  let cof = "Concrete_"^"object_field.";;
  
  let list_for_converters = 
    [
      ("string"                ,cof^"wrap_string"        ,cof^"unwrap_string"); 
      ("string list"           ,cof^"of_string_list"     ,cof^"to_string_list"); 
      ("(string * string) list",cof^"of_string_pair_list",cof^"to_string_pair_list"); 
    ] ;;
  
  let list_for_ofconverters = Image.image (fun (x,y,z)->(x,y)) list_for_converters ;;
  let list_for_toconverters = Image.image (fun (x,y,z)->(x,z)) list_for_converters ;;  
  
  let default_converter typename =
      if Supstring.ends_with typename "_t.t"
      then let tn = Cull_string.coending 4 typename in 
           (tn^".to_concrete_object",tn^".of_concrete_object")
      else ("to_concrete_object","of_concrete_object") ;;                
  
  let default_ofconverter typename = fst(default_converter typename);;
  let default_toconverter typename = snd(default_converter typename);;
  
  let ofconverter typename =
      match List.assoc_opt typename list_for_ofconverters with 
      Some(converter) -> converter 
      | None -> default_ofconverter typename;;
  
  let toconverter typename =
      match List.assoc_opt typename list_for_toconverters with 
      Some(converter) -> converter 
      | None -> default_toconverter typename;;    
      
  let ds = String.make 2 (char_of_int 59) ;;
  
  let tab2 = String.make 2 ' ' ;;
  let tab3 = String.make 3 ' ' ;;
  let tab5 = String.make 5 ' ' ;;
  
  let write_label_element (field_name,field_type) = 
    "let "^ field_name ^ "_label = " ^ 
    "salt ^ \"" ^ field_name ^ "\"" ^ ds ;;
  
    
(*
  let write_labels modulename data=
    let synarchy1 = Image.image labels data in 
    let synarchy2 = Strung.reposition_according_to_separator "=" synarchy1 in
    let oligarch1 = tab2 ^ (String.capitalize_ascii modulename)^"."^synarch1 in
    let temp1 = oligarch1 :: rest_of_synarchy in 
    let oligarchy = Strung.reposition_according_to_separator "=" temp1 in
    let temp2 = [
     "let g = Concrete_object_field.get_record crobj in ";
     "{"
     ]@oligarchy@
     [ 
     "}"^ds
     ] in 
    let nonfirst_lines = Image.image (fun line->tab3^line) temp2 in 
    let lines = "let of_concrete_object  crobj= ":: nonfirst_lines in 
    String.concat "\n" lines;;    
*)

  let write_ofconverter_element (field_name,field_type) = 
    field_name ^ " = " ^ (ofconverter field_type) 
    ^ "(g " ^ field_name ^ "_label);";;
  
  let write_ofconverter modulename data=
     let old_synarch = Image.image write_ofconverter_element data in 
     let synarch1 = List.hd old_synarch 
     and rest_of_synarchy = Image.image (fun line->tab5^line) (List.tl old_synarch) in
     let oligarch1 = tab2 ^ (String.capitalize_ascii modulename)^"."^synarch1 in
     let temp1 = oligarch1 :: rest_of_synarchy in 
     let oligarchy = Strung.reposition_according_to_separator "=" temp1 in
     let temp2 = [
      "let g = Concrete_object_field.get_record crobj in ";
      "{"
      ]@oligarchy@
      [ 
      "}"^ds
      ] in 
     let nonfirst_lines = Image.image (fun line->tab3^line) temp2 in 
     let lines = "let of_concrete_object  crobj= ":: nonfirst_lines in 
     String.concat "\n" lines;;    
  
  let write_toconverter modulename data=
      let synarchy = (
       Image.image (fun (field_name,field_type)->
         " " ^ field_name ^ "_label , " ^ (toconverter field_type) 
         ^ "(item." ^ (String.capitalize_ascii modulename) ^ "." ^field_name ^ ");"
        ) data
      ) in 
      String.concat "\n"
      ("let to_concrete_object  item= "::
       (Image.image (fun line->tab3^line)
       ([
       "Concrete_object_t.Record([ ";
       ]@(Strung.reposition_according_to_separator "," synarchy)@
       [ 
       "]"^ds
       ])));;  
  
  
  let print_converters modulename data = 
      let msg = "\n\n\n"^(write_ofconverter modulename data)^
                "\n\n\n"^(write_toconverter modulename data)^"\n\n\n" in 
      print_string msg;;
  
  let print ~module_vaguename = 
        let eless = Usual_coma_state.find_endingless module_vaguename in 
        let ap = Dfn_full.to_absolute_path (Dfn_join.to_ending eless Dfa_ending.ml) in 
        let s_ap = Absolute_path.to_string ap in 
        let modulename = Cull_string.before_rightmost (Cull_string.after_rightmost s_ap '/') '.' in 
        let text = Io.read_whole_file ap in 
        let i1 = Substring.rightmost_index_of_in "{" text in 
        let i2 = Substring.leftmost_index_of_in_from "}" text i1 in 
        let type_description = Cull_string.interval text (i1+1) (i2-1) in 
        let temp1 = Str.split (Str.regexp_string ";") type_description in 
        let temp2 = Option.filter_and_unpack (
         fun line ->
            let (left,right) = Cull_string.split_wrt_rightmost line ':' in 
            if left="" then None else 
            Some(Cull_string.shorten_blanks left,Cull_string.shorten_blanks right)  
        ) temp1 in 
        print_converters modulename  temp2;; 
  
  end ;;
  
  let print = Private.print ;;  
  
  
  
  
  
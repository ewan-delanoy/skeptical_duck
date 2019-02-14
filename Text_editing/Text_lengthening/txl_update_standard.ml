(*

#use "Text_editing/Text_lengthening/txl_update_standard.ml";;

*)

let location_for_persisting =
  "Text_editing/Text_lengthening/txl_standard.ml";;

let persist_to_file ()=
   let description = Txl_print.print  (!(Txl_standard.one)) in 
   let text = "\n\n\nlet state_container = ref(\n "^description^"\n );;\n\n\n" in 
   let ap=Absolute_path.of_string location_for_persisting in   
   Replace_inside.overwrite_between_markers_inside_file
     (Overwriter.of_string text)
     (
       "(* Description of standard text lengthener starts here *)",
       "(* Description of standard text lengthener ends here *)"
     ) 
     ap;;



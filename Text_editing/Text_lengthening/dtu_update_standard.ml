(*

#use "Text_editing/Text_lengthening/dtu_update_standard.ml";;

*)

let location_for_persisting =
  "Text_editing/Text_lengthening/dtu_standard.ml";;

let persist_to_file ()=
   let description = Dtu_print.print  (!(Dtu_standard.one)) in 
   let text = "\n\n\nlet state_container = \n "^description^"\n ;;\n\n\n" in 
   let ap=Absolute_path.of_string location_for_persisting in   
   Replace_inside.overwrite_between_markers_inside_file
     (Overwriter.of_string text)
     (
       "(* Description of standard double tunnel starts here *)",
       "(* Description of standard double tunnel ends here *)"
     ) 
     ap;;



(*

#use "Text_editing/Text_lengthening/text_lengthener_modify_standard.ml";;

*)



module Private = struct

let location_for_printing =
  "Text_editing/Text_lengthening/text_lengthener_standard.ml";;

let persist_to_file ()=
   let description = Text_lengthener_print.print  (!(Text_lengthener_standard.one)) in 
   let text = "\n\n\nlet one = ref(\n "^description^"\n );;\n\n\n" in 
   let ap=Absolute_path.of_string location_for_printing in   
   Replace_inside.overwrite_between_markers_inside_file
     (Overwriter.of_string text)
     (
       "(* Description of standard text lengthener starts here *)",
       "(* Description of standard text lengthener ends here *)"
     ) 
     ap;;

let add_persistence_to_method f x=
   let new_standard = f (!(Text_lengthener_standard.one)) x in 
    (
        Text_lengthener_standard.one:=new_standard ;
        persist_to_file 
    ) ;;  
  

end;;


let order_fix = 
    Private.add_persistence_to_method 
    (fun x ()->Text_lengthener_insert_or_remove.order_fix x) ;;

(* Insert *)

let i_decompression=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.i_decompression ;;

let i_adjustment=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.i_adjustment ;;

let i_expansion=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.i_expansion ;;

let i_inert_word=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.i_inert_word ;;    

let i_left_core_abbreviation=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.i_left_core_abbreviation ;;

let i_prefix_abbreviation=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.i_prefix_abbreviation ;;

(* Remove *)

let r_decompression=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.r_decompression ;;

let r_adjustment=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.r_adjustment ;;

let r_expansion=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.r_expansion ;;

let r_inert_word=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.r_inert_word ;;    

let r_left_core_abbreviation=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.r_left_core_abbreviation ;;

let r_prefix_abbreviation=
    Private.add_persistence_to_method 
    Text_lengthener_insert_or_remove.r_prefix_abbreviation ;;
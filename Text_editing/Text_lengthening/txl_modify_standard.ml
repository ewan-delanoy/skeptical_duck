(*

#use "Text_editing/Text_lengthening/text_lengthener_modify_standard.ml";;

*)



module Private = struct

let location_for_printing =
  "Text_editing/Text_lengthening/text_lengthener_standard.ml";;

let persist_to_file ()=
   let description = Txl_print.print  (!(Txl_standard.one)) in 
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
   let new_standard = f (!(Txl_standard.one)) x in 
    (
        Txl_standard.one:=new_standard ;
        persist_to_file 
    ) ;;  
  

end;;


let order_fix = 
    Private.add_persistence_to_method 
    (fun txl ()->Txl_modify.order_fix txl) ;;

(* Insert *)

let i_decompression=
    Private.add_persistence_to_method 
    Txl_modify.i_decompression ;;

let i_adjustment=
    Private.add_persistence_to_method 
    Txl_modify.i_adjustment ;;

let i_expansion=
    Private.add_persistence_to_method 
    Txl_modify.i_expansion ;;

let i_inert_word=
    Private.add_persistence_to_method 
    Txl_modify.i_inert_word ;;    

let i_left_core_abbreviation=
    Private.add_persistence_to_method 
    Txl_modify.i_left_core_abbreviation ;;

let i_prefix_abbreviation=
    Private.add_persistence_to_method 
    Txl_modify.i_prefix_abbreviation ;;

(* Remove *)

let r_decompression=
    Private.add_persistence_to_method 
    Txl_modify.r_decompression ;;

let r_adjustment=
    Private.add_persistence_to_method 
    Txl_modify.r_adjustment ;;

let r_expansion=
    Private.add_persistence_to_method 
    Txl_modify.r_expansion ;;

let r_inert_word=
    Private.add_persistence_to_method 
    Txl_modify.r_inert_word ;;    

let r_left_core_abbreviation=
    Private.add_persistence_to_method 
    Txl_modify.r_left_core_abbreviation ;;

let r_prefix_abbreviation=
    Private.add_persistence_to_method 
    Txl_modify.r_prefix_abbreviation ;;
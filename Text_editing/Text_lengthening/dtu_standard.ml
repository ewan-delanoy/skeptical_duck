(*
#use "Text_editing/Text_lengthening/dtu_standard.ml";;
*)

let location_for_persisting =
  "Text_editing/Text_lengthening/dtu_standard.ml";;

module Private = struct

(* Description of standard double tunnel starts here *)

let state_container = ref(
 Dtu_construct.construct( [
    ("Conférences","Conférences");
    ("de","de");
])
 );;


(* Description of standard double tunnel ends here *)

end ;; 


module Friend = struct

let set_state txl=(Private.state_container:= txl);;

end ;;

let current_state ()=(!(Private.state_container));;

let persist_to_file ()=
   let description = Dtu_print.print  (!(Private.state_container)) in 
   let text = "\n\n\nlet state_container = \n "^description^"\n ;;\n\n\n" in 
   let ap=Absolute_path.of_string location_for_persisting in   
   Replace_inside.overwrite_between_markers_inside_file
     (Overwriter.of_string text)
     (
       "(* Description of standard double tunnel starts here *)",
       "(* Description of standard double tunnel ends here *)"
     ) 
     ap;;
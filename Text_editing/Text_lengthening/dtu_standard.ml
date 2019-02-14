(*
#use "Text_editing/Text_lengthening/dtu_standard.ml";;
*)

module Private = struct

(* Description of standard double tunnel starts here *)


let state_container = ref(Dtu_construct.construct
[

]);;


(* Description of standard double tunnel ends here *)

end ;; 


module Friend = struct

let set_state txl=(Private.state_container:= txl);;

end ;;

let current_state ()=(!(Private.state_container));;
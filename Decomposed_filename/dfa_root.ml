(*

The rightmost trailing slash is removed.

#use"Decomposed_filename/dfa_root.ml";;

*)

let without_trailing_slash (Dfa_root_t.R s)=s;;
let connectable_to_subpath (Dfa_root_t.R s)=s^"/";;  
let dummy = Dfa_root_t.R "" ;;

let of_line line = Dfa_root_t.R(Tools_for_absolute_path.remove_trailing_slash line);;


let to_concrete_object (Dfa_root_t.R(line))=
    Concrete_object_t.Variant("Dfa_"^"root.R",[Crobj_converter.string_to_concrete_object(line)]);;

let of_concrete_object ccrt_obj =
   let (_,(arg1,_,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant ccrt_obj in 
   Dfa_root_t.R(Crobj_converter.string_of_concrete_object arg1);;


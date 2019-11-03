(* 

#use"Hex_analysis/hex_strategy_static_constructor.ml";;

*)

let summarize_in_string = function
    Hex_strategy_static_constructor_t.Basic_Linker(_,_)->"Basic linker"
   |Gluing -> "Gluing" 
   |Disjunction (_)->"Disjunction";;
   
let salt = "Hex_"^"strategy_static_constructor_t.";;

let hook_for_basic_linker = "Basic_Linker";;
let hook_for_gluing = "Gluing";;
let hook_for_disjunction = "Disjunction";;

(*
let of_concrete_object crobj =
    let (hook,(arg1,arg2,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
    if hook = hook_for_basic_linker then Hex_strategy_static_constructor_t.Basic_Linker(_,_) else 
*)      
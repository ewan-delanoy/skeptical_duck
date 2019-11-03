(* 

#use"Hex_analysis/hex_strategy_static_constructor.ml";;

*)

let summarize_in_string = function
    Hex_strategy_static_constructor_t.Basic_Linker(_,_)->"Basic linker"
   |Gluing -> "Gluing" 
   |Disjunction (_)->"Disjunction";;
   
let salt = "Hex_"^"strategy_static_constructor_t.";;

let hook_for_basic_linker = salt ^"Basic_Linker";;
let hook_for_gluing       = salt ^ "Gluing";;
let hook_for_disjunction  = salt ^ "Disjunction";;

exception Of_concrete_object_exn of string;;

let of_concrete_object crobj =
    let (hook,(arg1,arg2,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
    if hook = hook_for_basic_linker 
    then Hex_strategy_static_constructor_t.Basic_Linker(
           Hex_cell_set.of_concrete_object arg1,Hex_cell_pair_set.of_concrete_object arg1 ) 
    else 
    if hook = hook_for_gluing
    then Hex_strategy_static_constructor_t.Gluing
    else 
    if hook = hook_for_disjunction
    then Hex_strategy_static_constructor_t.Disjunction(
        Concrete_object_field.to_list Hex_cell.of_concrete_object arg1
         )
    else raise(Of_concrete_object_exn(hook));;

let to_concrete_object = function
    Hex_strategy_static_constructor_t.Basic_Linker(cells,pairs)->
        Concrete_object_t.Variant(hook_for_basic_linker,
           [Hex_cell_set.to_concrete_object cells;Hex_cell_pair_set.to_concrete_object pairs])
   |Gluing -> Concrete_object_t.Variant(hook_for_gluing,[])
   |Disjunction (cells)->
            Concrete_object_t.Variant(hook_for_disjunction,
           [Concrete_object_field.of_list Hex_cell.to_concrete_object cells]) ;;
   
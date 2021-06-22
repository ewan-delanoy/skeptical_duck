(* 

#use"Hex_analysis/hex_strategy_static_constructor.ml";;

*)

let summarize_in_string constr indices= match constr with 
    Hex_strategy_static_constructor_t.Molecular(_,_)->"Molecular"
   |Exhaustive_Disjunction (_)->"Disjunction, from "^(Strung.of_intlist indices);;
   
let salt = "Hex_"^"strategy_static_constructor_t.";;

let hook_for_molecular    = salt ^ "Molecular";;
let hook_for_disjunction  = salt ^ "Exhaustive_Disjunction";;

exception Of_concrete_object_exn of string;;

let of_concrete_object crobj =
    let (hook,(arg1,arg2,_,_,_,_,_))=Concrete_object.unwrap_bounded_variant crobj in 
    if hook = hook_for_molecular 
    then Hex_strategy_static_constructor_t.Molecular(
           Hex_molecular_linker.of_concrete_object arg1,
           Hex_cell_set.of_concrete_object arg2
         )  
    else 
    if hook = hook_for_disjunction
    then Hex_strategy_static_constructor_t.Exhaustive_Disjunction(
    Crobj_converter_combinator.to_list Hex_cell.of_concrete_object arg1
         )
    else raise(Of_concrete_object_exn(hook));;

let to_concrete_object = function
    Hex_strategy_static_constructor_t.Molecular(mlclr,active_part)->
        Concrete_object_t.Variant(hook_for_molecular,
           [Hex_molecular_linker.to_concrete_object mlclr;
            Hex_cell_set.to_concrete_object active_part
           ])
   |Exhaustive_Disjunction (cells)->
            Concrete_object_t.Variant(hook_for_disjunction,
           [Crobj_converter_combinator.of_list Hex_cell.to_concrete_object cells]) ;;
   
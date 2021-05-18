(* 

#use"Hex_analysis/hex_cog_in_machine.ml";;

*)



let of_concrete_object crobj= 
   let (arg1,arg2,arg3,arg4,arg5,_,_)=Concrete_object_field.unwrap_bounded_uple crobj in 
   Hex_cog_in_machine_t.C
   (
    Hex_strategy_static_constructor.of_concrete_object arg1,
    Crobj_converter.string_of_concrete_object(arg2),
    Crobj_converter_combinator.to_list Crobj_converter.int_of_concrete_object (arg3),
    Hex_flattened_end_strategy_field.of_concrete_object(arg4)
   );;

let to_concrete_object (Hex_cog_in_machine_t.C(constr,comment,indices,fles)) =
   Concrete_object_t.Uple [
      Hex_strategy_static_constructor.to_concrete_object constr;
      Crobj_converter.string_to_concrete_object(comment);
      Crobj_converter_combinator.of_list Crobj_converter.int_to_concrete_object (indices);
      Hex_flattened_end_strategy_field.to_concrete_object(fles)
   ]  ;;

let list_of_concrete_object crobj = Crobj_converter_combinator.to_list of_concrete_object crobj;;
let list_to_concrete_object l = Crobj_converter_combinator.of_list to_concrete_object l;;


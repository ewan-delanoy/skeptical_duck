(* 

#use"Hex_analysis/hex_old_cog_in_machine.ml";;

*)


let of_concrete_object crobj= 
   let (arg1,arg2,arg3,arg4,_,_,_)=Concrete_object_field.unwrap_bounded_uple crobj in 
   Hex_old_cog_in_machine_t.C
   (
    Hex_old_strategy_static_constructor.of_concrete_object arg1,
    Concrete_object_field.unwrap_string(arg2),
    Concrete_object_field.to_int_list(arg3),
    Hex_flattened_end_strategy.of_concrete_object(arg4)
   );;

let to_concrete_object (Hex_old_cog_in_machine_t.C(constr,comment,indices,fles)) =
   Concrete_object_t.Uple [
      Hex_old_strategy_static_constructor.to_concrete_object constr;
      Concrete_object_field.wrap_string(comment);
      Concrete_object_field.of_int_list(indices);
      Hex_flattened_end_strategy.to_concrete_object(fles)
   ]  ;;

let list_of_concrete_object crobj = Concrete_object_field.to_list of_concrete_object crobj;;
let list_to_concrete_object l = Concrete_object_field.of_list to_concrete_object l;;


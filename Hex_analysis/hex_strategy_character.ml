(* 

#use"Hex_analysis/hex_strategy_character.ml";;

*)

module Private = struct

let salt = "Hex_"^"strategy_character.";;

let namings =
   [
     Hex_strategy_character_t.Partial_strategy, salt ^ "Partial_strategy";
     Hex_strategy_character_t.Total_strategy, salt ^ "Total_strategy";
   ];;

end ;;

let of_concrete_object = Concrete_object_field.unwrap_lonely_variant Private.namings;;
let to_concrete_object = Concrete_object_field.wrap_lonely_variant Private.namings;;


(* 

#use"Hex_analysis/hex_cardinal_direction.ml";;

*)

module Private = struct 
let correspondences = 
    [
      'd', Hex_cardinal_direction_t.Down  ;
      'l', Hex_cardinal_direction_t.Left  ;
      'r', Hex_cardinal_direction_t.Right ;
      'u', Hex_cardinal_direction_t.Up ;
    ];;
end ;;

let opt_of_char c =
   Option.find_and_stop (
       fun p->if fst(p)=c then Some(snd p) else None) 
       Private.correspondences ;;
   
let is_vertical = function 
    Hex_cardinal_direction_t.Down 
   |Hex_cardinal_direction_t.Up -> true 
   | _ -> false ;;   
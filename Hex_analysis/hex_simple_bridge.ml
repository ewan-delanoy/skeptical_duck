(* 

#use"Hex_analysis/hex_simple_bridge.ml";;

*)

module Private = struct 
let to_uple x=
   let lh = x.Hex_simple_bridge_t.left_half 
   and rh = x.Hex_simple_bridge_t.right_half in 
   (
       lh.Hex_simple_bridge_t.start_side,
       lh.Hex_simple_bridge_t.bridger,
       lh.Hex_simple_bridge_t.end_side,
       rh.Hex_simple_bridge_t.start_side,
       rh.Hex_simple_bridge_t.bridger,
       rh.Hex_simple_bridge_t.end_side
   ) ;;
end ;;


let inner_sea x = 
  let (lh_start,lh_bridge,lh_end,rh_start,rh_bridge,rh_end) = Private.to_uple x in 
  Hex_cell_set.safe_set [lh_bridge;rh_bridge] ;;

let to_uple = Private.to_uple ;;
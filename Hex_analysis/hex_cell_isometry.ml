(* 

#use"Hex_analysis/hex_cell_isometry.ml";;

*)
module Private = struct 
let reflect cell = Hex_cell.of_int_pair (Hex_ipair.reflect (Hex_cell.to_int_pair cell));;
let oppose dim cell = Hex_cell.of_int_pair (Hex_ipair.oppose dim (Hex_cell.to_int_pair cell));;
end ;;



let oppflect dim cell = Private.oppose dim (Private.reflect cell);;
let oppose = Private.oppose;;
let reflect = Private.reflect ;;

let translate (dx,dy) cell=
   let (i,j) = Hex_cell.to_int_pair cell in 
   Hex_cell.of_int_pair (i+dx,j+dy) ;;




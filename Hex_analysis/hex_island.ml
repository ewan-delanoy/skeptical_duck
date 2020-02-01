(* 

#use"Hex_analysis/hex_island.ml";;

*)

exception Disconnected_cells of (int * int) list;;
exception Lonely_side of Hex_cardinal_direction_t.t ;;

let constructor dim opt_direction l=
  let z=Set_of_poly_pairs.safe_set l in 
  let ccs = Hex_common.compute_connected_components dim l in 
  if (List.length ccs) <> 1 
  then  raise(Disconnected_cells (Image.image List.hd ccs)) 
  else 
  match opt_direction with 
   None -> Hex_island_t.I(None,z)
  |Some(direction) ->
  if not(List.exists (fun 
     p-> Hex_cardinal_direction.Border.test dim direction 
         (Hex_cell.of_int_pair p)
  ) l)
  then raise(Lonely_side(direction))
  else Hex_island_t.I(opt_direction,z) ;;

    
let neighbors dim (Hex_island_t.I(opt_direction,z)) =
   let part1 =(
     match opt_direction with 
      None -> Set_of_poly_pairs.empty_set
     |Some(direction) -> 
       let temp1=Hex_cardinal_direction.Border.enumerate_all dim direction in 
       let temp2=Image.image Hex_cell.to_int_pair temp1 in 
       Set_of_poly_pairs.safe_set temp2
   ) 
   and part2 = Hex_common.neighbors_for_several dim z in 
   let excessive_whole = Set_of_poly_pairs.merge part1 part2 in 
   Set_of_poly_pairs.setminus excessive_whole z;;




   

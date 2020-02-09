(* 

#use"Hex_analysis/hex_island.ml";;

*)

exception Disconnected_cells of (int * int) list;;
exception Lonely_side of Hex_cardinal_direction_t.t ;;

let constructor dim opt_direction l=
  let z=Set_of_poly_pairs.safe_set l in 
  let ccs = Hex_ipair.compute_connected_components dim l in 
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

let decompose eob =     
    let dim = eob.Hex_end_of_battle_t.dimension 
    and w = eob.Hex_end_of_battle_t.winner 
    and cells = eob.Hex_end_of_battle_t.ally_territory in 
    let ipairs = Hex_cell_set.image Hex_cell.to_int_pair cells in 
    let components = Hex_ipair.compute_connected_components dim ipairs in 
    let sides = Hex_cardinal_direction.sides_for_player w in 
    let pre_answer = Image.image (
       fun l->
       let opt = Option.seek (fun side->
          List.exists(fun 
          p-> Hex_cardinal_direction.Border.test dim side 
         (Hex_cell.of_int_pair p)) l
       ) sides in 
       Hex_island_t.I(opt,Set_of_poly_pairs.safe_set l)
    ) components in 
    let complements = Option.filter_and_unpack (
       fun side -> 
        if List.exists (fun (Hex_island_t.I(opt,_)) ->
           opt = Some side
         ) pre_answer   
        then None 
        else Some(Hex_island_t.I(Some side,Set_of_poly_pairs.empty_set))   
    )  sides in 
    pre_answer @ complements ;;


let inner_earth (Hex_island_t.I(opt,z))= 
   Hex_cell_set.safe_set (Set_of_poly_pairs.image Hex_cell.of_int_pair z) ;;

let is_included_in 
  (Hex_island_t.I(opt1,z1)) 
    (Hex_island_t.I(opt2,z2)) = 
  if (not (Set_of_poly_pairs.is_included_in z1 z2)) 
  then false 
  else match opt1 with 
       None -> true 
       | _ -> (opt2 = opt1);;
 


let neighbors dim (Hex_island_t.I(opt_direction,z)) =
   let part1 =(
     match opt_direction with 
      None -> Set_of_poly_pairs.empty_set
     |Some(direction) -> 
       let temp1=Hex_cardinal_direction.Border.enumerate_all dim direction in 
       let temp2=Image.image Hex_cell.to_int_pair temp1 in 
       Set_of_poly_pairs.safe_set temp2
   ) 
   and part2 = Hex_ipair.neighbors_for_several dim 
       (Set_of_poly_pairs.forget_order z) in 
   let excessive_whole = Set_of_poly_pairs.merge part1 part2 in 
   Set_of_poly_pairs.setminus excessive_whole z;;






let oppose dim (Hex_island_t.I(old_opt,z)) =
   let new_opt = (match old_opt with 
      None -> None 
      |Some(direction)->Some(Hex_cardinal_direction.oppose direction)
   ) 
   and new_z = Set_of_poly_pairs.safe_set 
      (Set_of_poly_pairs.image (Hex_ipair.oppose dim) z) in 
   Hex_island_t.I(new_opt,new_z);;


let reflect (Hex_island_t.I(old_opt,z)) =
   let new_opt = (match old_opt with 
      None -> None 
      |Some(direction)->Some(Hex_cardinal_direction.reflect direction)
   ) 
   and new_z = Set_of_poly_pairs.safe_set 
      (Set_of_poly_pairs.image Hex_ipair.reflect z) in 
   Hex_island_t.I(new_opt,new_z);;


let to_readable_string (Hex_island_t.I(opt,z)) =
   match opt with 
    Some(direction)->"<"^(Hex_cardinal_direction.for_ground_description direction)^">"
   |None -> 
       let l = Ordered.sort Hex_cell.cmp (Set_of_poly_pairs.image Hex_cell.of_int_pair z) in 
       let n = List.length l in 
       let p1 = Hex_cell.to_string(List.hd l) in 
       if n=1 then p1 else 
       let p2 = Hex_cell.to_string(List.hd(List.rev l)) in 
       let joiner = (if n>2 then ".." else ",") in 
       p1^joiner^p2;;

   

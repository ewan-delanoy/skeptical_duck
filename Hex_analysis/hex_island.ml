(* 

#use"Hex_analysis/hex_island.ml";;

*)

exception Missing_side of Hex_cardinal_direction_t.t * (Hex_island_t.t list) ;;

module Private = struct 

let inner_to_readable_string z =
       let l = Ordered.sort Hex_cell.cmp (Set_of_poly_pairs.image Hex_cell.of_int_pair z) in 
       let n = List.length l in 
       if n=0 then "" else 
       let p1 = Hex_cell.to_string(List.hd l) in 
       if n=1 then p1 else 
       let p2 = Hex_cell.to_string(List.hd(List.rev l)) in 
       let joiner = (if n>2 then ".." else ",") in 
       p1^joiner^p2;;

let to_readable_string (Hex_island_t.I(anchor,z)) =
   let border_part = Hex_anchor.to_readable_string anchor 
   and inner_part = inner_to_readable_string z in 
   border_part^inner_part;;


let neighbors dim (Hex_island_t.I(anchor,z)) =
   let part1 = Hex_anchor.neighbors dim anchor 
   and part2 = Hex_ipair.neighbors_for_several dim 
       (Set_of_poly_pairs.forget_order z) in 
   let excessive_whole = Set_of_poly_pairs.merge part1 part2 in 
   Set_of_poly_pairs.setminus excessive_whole z;;  


end ;; 


let anchor (Hex_island_t.I(a,_)) = a;; 


let bare_side side =
 let anchor = Hex_anchor_t.Single_anchor(side) in 
 Hex_island_t.I(anchor,Set_of_poly_pairs.empty_set);;


let common_neighbors 
  dim island1 island2 =
      if island1 = island2 then Set_of_poly_pairs.empty_set else 
      Set_of_poly_pairs.intersect 
        (Private.neighbors dim island1)
           (Private.neighbors dim island2) ;;

let decompose eob =     
    let dim = eob.Hex_end_of_battle_t.dimension 
    and w = eob.Hex_end_of_battle_t.winner 
    and cells = eob.Hex_end_of_battle_t.ally_territory in 
    let ipairs = Hex_cell_set.image Hex_cell.to_int_pair cells in 
    let components = Hex_ipair.compute_connected_components dim ipairs in 
    let (side1,side2) = Hex_cardinal_direction.sides_for_player w in 
    let pre_answer = Image.image (
       fun l->
       let touched_sides = List.filter (fun side->
          List.exists(fun 
          p-> Hex_cardinal_direction.Border.test dim side 
         (Hex_cell.of_int_pair p)) l
       ) [side1;side2] in 
       Hex_island_t.I(Hex_anchor.of_list touched_sides,Set_of_poly_pairs.safe_set l)
    ) components in 
    let complements = Option.filter_and_unpack (
       fun side -> 
        if List.exists (fun (Hex_island_t.I(anchor,_)) ->
           Hex_anchor.touches_side anchor side
         ) pre_answer   
        then None 
        else Some(Hex_island_t.I(Hex_anchor_t.Single_anchor side,Set_of_poly_pairs.empty_set))   
    )  [side1;side2] in 
    pre_answer @ complements ;;

let eviscerate (Hex_island_t.I(anchor,z))= 
(Hex_island_t.I(anchor,Set_of_poly_pairs.empty_set)) ;;

let get_side side l=
   match Option.seek (fun (Hex_island_t.I(anchor,_))->Hex_anchor.touches_side anchor side) l with 
   Some(island) -> island 
   |None ->raise(Missing_side(side,l));;

let inner_earth (Hex_island_t.I(_,z))= 
   Hex_cell_set.safe_set (Set_of_poly_pairs.image Hex_cell.of_int_pair z) ;;

let is_included_in 
  (Hex_island_t.I(anchor1,z1)) 
    (Hex_island_t.I(anchor2,z2)) = 
     (Hex_anchor.is_included_in anchor1 anchor2) &&
     (Set_of_poly_pairs.is_included_in z1 z2) ;;

let is_two_edged (Hex_island_t.I(anchor,z)) = Hex_anchor.is_two_edged anchor ;; 

let minimal_connection (Hex_island_t.I(_,z1),Hex_island_t.I(_,z2)) (Hex_island_t.I(_,z)) =
  (* as a first approximation, we de not optimize at all and take 
  everything *)  
  let ipairs = Set_of_poly_pairs.setminus z (Set_of_poly_pairs.merge z1 z2) in 
  Hex_cell_set.safe_set (Set_of_poly_pairs.image Hex_cell.of_int_pair ipairs);;

let minimal_version_for_two_edged (Hex_island_t.I(anchor,z)) =
   (* as a first approximation, we de not optimize at all and take 
  everything *)  
  (*
  let one_side = Hex_anchor.any_side anchor in 
  let other_side = Hex_cardinal_direction.oppose one_side in 
   *)
  Hex_cell_set.safe_set (Set_of_poly_pairs.image Hex_cell.of_int_pair z);;

let neighbors = Private.neighbors ;;

let oppose dim (Hex_island_t.I(old_anchor,z)) =
   let new_anchor = Hex_anchor.oppose old_anchor 
   and new_z = Set_of_poly_pairs.safe_set 
      (Set_of_poly_pairs.image (Hex_ipair.oppose dim) z) in 
   Hex_island_t.I(new_anchor,new_z);;     

let print_out (fmt:Format.formatter) nc=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string nc);;     


let reflect (Hex_island_t.I(old_anchor,z)) =
   let new_anchor = Hex_anchor.reflect old_anchor 
   and new_z = Set_of_poly_pairs.safe_set 
      (Set_of_poly_pairs.image Hex_ipair.reflect z) in 
   Hex_island_t.I(new_anchor,new_z);;        

let test_for_neighbor dim island cell =
   Set_of_poly_pairs.mem (Hex_cell.to_int_pair cell) (Private.neighbors dim island);;

let touches_side (Hex_island_t.I(anchor,z)) side = Hex_anchor.touches_side anchor side;; 

let to_readable_string = Private.to_readable_string ;;
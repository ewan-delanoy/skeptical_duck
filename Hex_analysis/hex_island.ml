(* 

#use"Hex_analysis/hex_island.ml";;

*)

exception Disconnected_cells of (int * int) list;;
exception Lonely_side of Hex_cardinal_direction_t.t ;;
exception Missing_side of Hex_cardinal_direction_t.t * (Hex_island_t.t list) ;;
exception Cell_in_corner of Hex_cell_t.t ;;

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

let to_readable_string (Hex_island_t.I(opt,z)) =
   let border_part = (
      match opt with 
      Some(direction)->"<"^(Hex_cardinal_direction.for_ground_description direction)^">"
     |None ->"" 
   )
   and inner_part = inner_to_readable_string z in 
   border_part^inner_part;;

let side_for_cell dim cell =
  let temp1 = List.filter (fun d->
     Hex_cardinal_direction.Border.test dim d cell 
  ) Hex_cardinal_direction.all in 
  let h = List.length temp1 in 
  if h=0 then None else 
  if h>1 then raise(Cell_in_corner(cell)) else 
  Some(List.hd temp1);;
  
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
  
let short_connections dim island1 island2 =
    let neighborhood1 = neighbors dim island1 
    and neighborhood2 = neighbors dim island2 in
    Set_of_poly_pairs.intersect neighborhood1 neighborhood2 ;;

let add_sided_cell_by_casing dim (opt_side,new_cell) l =
   let new_p = Hex_cell.to_int_pair new_cell in 
   let neighbors = Image.image Hex_cell.to_int_pair (Hex_cell.neighbors dim new_cell) in 
   let (connected,unconnected) = List.partition (
    fun (Hex_island_t.I(opt,z))->List.exists (fun q->
      Set_of_poly_pairs.mem q z
    ) neighbors 
   )  l in 
   let old_opts = Image.image (fun (Hex_island_t.I(opt,z))->opt) connected in
   let new_opt = Option.find_and_stop (fun opt->opt) (opt_side::old_opts) in 
   let old_pairs =  Image.image (fun (Hex_island_t.I(opt,z))->z) connected in 
   let new_z = Set_of_poly_pairs.insert new_p (Set_of_poly_pairs.fold_merge old_pairs) in 
   (Hex_island_t.I(new_opt,new_z))::unconnected ;;

let add_cell_by_casing dim new_cell l =
   add_sided_cell_by_casing dim (side_for_cell dim new_cell,new_cell) l ;;

end ;;

(* it is assumed that new_cell touches the side *)
let add_and_forget_the_adding dim (side,new_cell) old_islands =
    let islands1 = Private.add_sided_cell_by_casing dim (Some side,new_cell) old_islands in 
    let (singleton,islands2) = List.partition (fun (Hex_island_t.I(opt,z)) -> opt=Some side) islands1 in 
    let (Hex_island_t.I(opt,z)) = List.hd singleton in 
    let new_z = Set_of_poly_pairs.outsert (Hex_cell.to_int_pair new_cell) z in 
    islands2@[Hex_island_t.I(opt,new_z)] ;; 
   

let add_cell_by_casing = Private.add_cell_by_casing ;;

let add_sided_cell_by_casing = Private.add_sided_cell_by_casing ;;


let common_neighbors 
  dim island1 island2 =
      if island1 = island2 then Set_of_poly_pairs.empty_set else 
      Set_of_poly_pairs.intersect 
        (Private.neighbors dim island1)
           (Private.neighbors dim island2) ;;



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

let eviscerate (Hex_island_t.I(opt,z))= (Hex_island_t.I(opt,Set_of_poly_pairs.empty_set)) ;;

let get_side side l=
   match Option.seek (fun (Hex_island_t.I(opt,_))->opt=Some side) l with 
   Some(island) -> island 
   |None ->raise(Missing_side(side,l));;


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
 

let minimal_connection (Hex_island_t.I(_,z1),Hex_island_t.I(_,z2)) (Hex_island_t.I(_,z)) =
  (* as a first approximation, we de not optimize at all and take 
  everything *)  
  let ipairs = Set_of_poly_pairs.setminus z (Set_of_poly_pairs.merge z1 z2) in 
  Hex_cell_set.safe_set (Set_of_poly_pairs.image Hex_cell.of_int_pair ipairs);;

let neighbors = Private.neighbors ;;






let oppose dim (Hex_island_t.I(old_opt,z)) =
   let new_opt = (match old_opt with 
      None -> None 
      |Some(direction)->Some(Hex_cardinal_direction.oppose direction)
   ) 
   and new_z = Set_of_poly_pairs.safe_set 
      (Set_of_poly_pairs.image (Hex_ipair.oppose dim) z) in 
   Hex_island_t.I(new_opt,new_z);;

let outer_earth (Hex_island_t.I(opt,z))= opt ;;

let print_out (fmt:Format.formatter) nc=
   Format.fprintf fmt "@[%s@]" (Private.to_readable_string nc);;     


let reflect (Hex_island_t.I(old_opt,z)) =
   let new_opt = (match old_opt with 
      None -> None 
      |Some(direction)->Some(Hex_cardinal_direction.reflect direction)
   ) 
   and new_z = Set_of_poly_pairs.safe_set 
      (Set_of_poly_pairs.image Hex_ipair.reflect z) in 
   Hex_island_t.I(new_opt,new_z);;

let short_connections_to_other_islands dim island1 islands= 
   let temp1=Image.image (Private.short_connections dim island1) islands in 
   let temp2=Set_of_poly_pairs.fold_merge temp1 in 
   Set_of_poly_pairs.image Hex_cell.of_int_pair temp2
;;

let short_connections_to_border dim island direction= 
   let temp1=Set_of_poly_pairs.image Hex_cell.of_int_pair (neighbors dim island) in 
   List.filter (Hex_cardinal_direction.Border.test dim direction) temp1
;;

let test_for_neighbor dim island cell =
   Set_of_poly_pairs.mem (Hex_cell.to_int_pair cell) (Private.neighbors dim island);;

let to_readable_string = Private.to_readable_string ;;

   

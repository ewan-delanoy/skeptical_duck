(* 

#use"Hex_analysis/hex_typical_sacrifice.ml";;

*)

module Private = struct 

let prepare_for_journey = 
   [
      Hex_typical_sacrifice_t.Simple_triangle, ("\206\177",[(2,1)],[(1,1);(1,2)])
   ] ;;


let upwards_version tscr = 
    let (descr,actv,pairs) =List.assoc tscr prepare_for_journey in 
    (actv,pairs);;

let oppose dim (actv,pairs) = let tempf = Image.image (Hex_ipair.oppose dim) in 
   (tempf actv,tempf pairs);;

let translate (dx,dy) (actv,pairs) = let tempf = Image.image (fun (x,y)->(x+dx,y+dy)) in 
   (tempf actv,tempf pairs);;   

let reflect (actv,pairs) = let tempf = Image.image Hex_ipair.reflect  in 
   (tempf actv,tempf pairs);;   

let specify_side tscr side = 
     let u = upwards_version tscr in 
     let l = reflect u in 
      match side with 
     Hex_cardinal_direction_t.Down  -> oppose Hex_dimension.eleven u
    |Hex_cardinal_direction_t.Left  -> l
    |Hex_cardinal_direction_t.Right -> oppose Hex_dimension.eleven l
    |Hex_cardinal_direction_t.Up    -> u;;     


let bounds (actv,pairs) = 
     let abscissas = Image.image fst actv 
     and ordinates = Image.image snd pairs in 
     let xmin = Min.list abscissas and xmax = Max.list abscissas 
     and ymin = Min.list ordinates and ymax = Max.list ordinates in   
     (xmin,ymin,xmax,ymax)

let admissible_translations formal_dim sided_scr side= 
   let (Hex_dimension_t.D dim) = formal_dim in 
   let (xmin,ymin,xmax,ymax) = bounds sided_scr in 
   let base = Hex_cardinal_direction.authorized_translations formal_dim (Some side) in  
    List.filter ( 
       fun (dx,dy) ->  (1-xmin <= dx) && (dx <= dim-xmax) 
                    && (1-ymin <= dy) && (dy <= dim-ymax) 
    )  base ;; 

end ;;

let compatible_sacrifices eob = 
   let (side1,side2) = Hex_cardinal_direction.sides_for_player (eob.Hex_end_of_battle_t.winner) 
   and evl = Hex_end_of_battle.assess eob in   
   let base1 = Cartesian.product Private.prepare_for_journey [side1;side2] in  
   List.flatten(Image.image (
      function ((tscr,_),side) ->
        let default_example = Private.specify_side tscr side in 
        let base2 = Private.admissible_translations (eob.Hex_end_of_battle_t.dimension) default_example side in  
        Option.filter_and_unpack (
           fun (dx,dy) -> 
             let (i_actv,i_pairs) = Private.translate (dx,dy) default_example in 
             let actv = Image.image Hex_cell.of_int_pair i_actv 
             and pairs = Image.image Hex_cell.of_int_pair i_pairs  in 
             if (List.for_all (fun cell->evl cell = Hex_eob_result_t.Ally_territory) actv) 
                &&
                 (List.for_all (fun cell->evl cell = Hex_eob_result_t.Unoccupied) pairs)
             then Some(tscr,side,pairs)
             else None    
        ) base2        
   ) base1);;

let data_for_sacrificial_starter end_of_battle pairs =
  let dim = end_of_battle.Hex_end_of_battle_t.dimension in 
  let artificial_eob = {
      end_of_battle with 
      Hex_end_of_battle_t.enemy_territory = 
        (Hex_cell_set.merge (Hex_cell_set.safe_set pairs) 
          end_of_battle.Hex_end_of_battle_t.enemy_territory)
  } in 
  let natural_islands = Hex_island.decompose artificial_eob in 
  let artificial_islands = Hex_island.add_several_and_forget_the_adding dim pairs natural_islands in 
  (artificial_eob,artificial_islands);; 

let reconstruct_sacrificial_solutions pairs mlclr=
    let temp1 = Listennou.extract_successive_pairs_from_even_list pairs in 
    let mlclr2 = Hex_molecular_linker.constructor(Image.image Hex_atomic_linker.pair temp1) in 
    Hex_molecular_linker.fold_merge [mlclr;mlclr2] ;;


let to_readable_string tscr side apex= 
   let (descr,_,_) = List.assoc tscr Private.prepare_for_journey  in 
   let cell = Hex_cell.of_int_pair apex in 
   descr^"("^(Hex_cell.to_string cell)^")";;     
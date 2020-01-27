(* 

#use"Hex_analysis/hex_planar_linker.ml";;

Static constructors for Hex_planar_linker_data_t.t objects 

*)

let check dim pllk cell = match pllk with 
    Hex_planar_linker_t.Eyed_claw(d1,d2) -> Hex_planar_linker_data.check_eyed_claw dim d1 d2 cell  
   |Noneyed_claw(double_hump,d) -> Hex_planar_linker_data.check_noneyed_claw dim double_hump d cell
   |Pyramid(d) -> Hex_planar_linker_data.check_pyramid dim d cell ;;

let ground pllk = 
    match pllk with 
    Hex_planar_linker_t.Eyed_claw(d1,d2) -> d2 
   |Noneyed_claw(double_hump,d) -> d
   |Pyramid(d) -> d ;;


let level = function 
    Hex_planar_linker_t.Eyed_claw(d1,d2) -> 4*((Hex_cardinal_direction.to_int d1)-1)+(Hex_cardinal_direction.to_int d2)  
   |Noneyed_claw(dh,d) ->16+4*((Hex_double_hump_qualifier.to_int dh)-1)+(Hex_cardinal_direction.to_int d)
   |Pyramid(d) -> 24 + (Hex_cardinal_direction.to_int d) ;;


let support pllk cell = match pllk with 
    Hex_planar_linker_t.Eyed_claw(d1,d2) -> Hex_planar_linker_data.support_for_eyed_claw d1 d2 cell  
   |Noneyed_claw(double_hump,d) -> Hex_planar_linker_data.support_for_noneyed_claw double_hump d cell
   |Pyramid(d) -> Hex_planar_linker_data.support_for_pyramid d cell ;;

let to_molecular_linker pllk cell = 
   let ipair = Hex_cell.to_int_pair cell in 
   let (first_trial,helper)=
   (match pllk with 
    Hex_planar_linker_t.Eyed_claw(d1,d2) -> 
      (Some(Hex_molecular_linker_t.M[Hex_atomic_linker_t.Eyed_claw(d1,d2,cell)]),[])   
   |Noneyed_claw(dh,d) -> (None, Hex_planar_linker_data.bridges_in_noneyed_claw dh d ipair )
   |Pyramid(d) -> (None, Hex_planar_linker_data.bridges_in_pyramid d ipair ) ) in 
   if first_trial<>None 
   then Option.unpack first_trial
   else 
        let pairs = Image.image (
            fun (p,q) -> Hex_atomic_linker.pair (Hex_cell.of_int_pair p,Hex_cell.of_int_pair q)
        ) helper in 
        Hex_molecular_linker.constructor pairs
    ;;



let unfold_all_around_cell dim cell=
   let part1=Image.image (
      fun (d1,d2) -> Hex_planar_linker_t.Eyed_claw(d1,d2)
   ) (Hex_planar_linker_data.unfold_eyed_claws_around_cell dim cell)
   and part2=Image.image (
      fun (dh,d) -> Hex_planar_linker_t.Noneyed_claw(dh,d)
   ) (Hex_planar_linker_data.unfold_noneyed_claws_around_cell dim cell)
   and part3=Image.image (
      fun d  -> Hex_planar_linker_t.Pyramid d
   ) (Hex_planar_linker_data.unfold_pyramids_around_cell dim cell) in 
   part1@part2@part3;;

let unfold_all_around_side dim side=
   let part1=Image.image (
      fun ((d1,d2),cell) -> (Hex_planar_linker_t.Eyed_claw(d1,d2),cell)
   ) (Hex_planar_linker_data.unfold_eyed_claws_around_side dim side)
   and part2=Image.image (
      fun ((dh,d),cell) -> (Hex_planar_linker_t.Noneyed_claw(dh,d),cell)
   ) (Hex_planar_linker_data.unfold_noneyed_claws_around_side dim side)
   and part3=Image.image (
      fun (d,cell)  -> (Hex_planar_linker_t.Pyramid d,cell)
   ) (Hex_planar_linker_data.unfold_pyramids_around_side dim side) in 
   part1@part2@part3;;


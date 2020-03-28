(* 

#use"Hex_analysis/hex_border_connector_name.ml";;

*)


exception Bad_apex_for_eyed_claw of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;

module Private = struct 

let downwards_noneyed_claw = function 
    Hex_double_hump_qualifier_t.Big_followed_by_small 
       -> Hex_border_connector_name_t.Bs_D 
   |Hex_double_hump_qualifier_t.Small_followed_by_big
       -> Hex_border_connector_name_t.Sb_D ;;    

let leftwards_noneyed_claw = function 
    Hex_double_hump_qualifier_t.Big_followed_by_small 
       -> Hex_border_connector_name_t.Bs_L 
   |Hex_double_hump_qualifier_t.Small_followed_by_big
       -> Hex_border_connector_name_t.Sb_L ;; 

let rightwards_noneyed_claw = function 
    Hex_double_hump_qualifier_t.Big_followed_by_small 
       -> Hex_border_connector_name_t.Bs_R 
   |Hex_double_hump_qualifier_t.Small_followed_by_big
       -> Hex_border_connector_name_t.Sb_R ;;    

let upwards_noneyed_claw = function 
    Hex_double_hump_qualifier_t.Big_followed_by_small 
       -> Hex_border_connector_name_t.Bs_U 
   |Hex_double_hump_qualifier_t.Small_followed_by_big
       -> Hex_border_connector_name_t.Sb_U ;; 


let noneyed_claw = function
     Hex_cardinal_direction_t.Down  -> downwards_noneyed_claw
    |Hex_cardinal_direction_t.Left  -> leftwards_noneyed_claw
    |Hex_cardinal_direction_t.Right -> rightwards_noneyed_claw
    |Hex_cardinal_direction_t.Up    -> upwards_noneyed_claw ;;   

end ;;

let for_side side =
    let ortho = Hex_cardinal_direction.orthogonal_directions side in 
    (Image.image (fun specifier->
        Hex_border_connector_name_t.Eyed_claw(specifier,side)
    ) ortho)
    @
    (
     Image.image (
       fun qualifier -> 
          Private.noneyed_claw side qualifier
     )   Hex_double_hump_qualifier.all
    )
    @
    [
      Hex_border_connector_name_t.Pyramid(side);
      Hex_border_connector_name_t.Small_pyramid(side) ;
      Hex_border_connector_name_t.Border_bridge(side) ; 
      Hex_border_connector_name_t.Walleye1(side) ; 
    ] ;;

let to_nondefault_molecular_linker nm apex junction = match nm with 
    Hex_border_connector_name_t.Eyed_claw(d1,d2) -> 
         if apex=None then raise (Bad_apex_for_eyed_claw(d1,d2)) else
         Some(Hex_molecular_linker.constructor [
              Hex_atomic_linker_t.Eyed_claw(d1,d2,Hex_cell.of_int_pair (Option.unpack apex))])
   |Bs_D -> None 
   |Bs_L -> None 
   |Bs_R -> None 
   |Bs_U -> None 
   |Sb_D -> None 
   |Sb_L -> None 
   |Sb_R -> None 
   |Sb_U -> None 
   |Pyramid(_) -> None 
   |Small_pyramid(_) -> None 
   |Border_bridge(_) -> None 
   |Walleye1(_) -> None;;
    
    

let to_readable_string = function 
    Hex_border_connector_name_t.Eyed_claw(d1,d2) -> 
         (Hex_cardinal_direction.for_eye_description d1)^"e"^(Hex_cardinal_direction.for_ground_description d2)
   |Bs_D -> "bds" 
   |Bs_L -> "bls" 
   |Bs_R -> "brs"
   |Bs_U -> "bus" 
   |Sb_D -> "sdb" 
   |Sb_L -> "slb"
   |Sb_R -> "srb"
   |Sb_U -> "sub"       
   |Pyramid(d) ->  (Hex_cardinal_direction.for_ground_description d)^"py" 
   |Small_pyramid(d) ->  (Hex_cardinal_direction.for_ground_description d)^"sy" 
   |Border_bridge(d) ->  (Hex_cardinal_direction.for_ground_description d)^"bb"
   |Walleye1(d) ->  (Hex_cardinal_direction.for_ground_description d)^"we1" ;;


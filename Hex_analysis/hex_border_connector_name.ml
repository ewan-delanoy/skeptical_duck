(* 

#use"Hex_analysis/hex_border_connector_name.ml";;

*)


let for_side side =
    let ortho = Hex_cardinal_direction.orthogonal_directions side in 
    (Image.image (fun specifier->
        Hex_border_connector_name_t.Eyed_claw(specifier,side)
    ) ortho)
    @
    (
     Image.image (
       fun qualifier -> Hex_border_connector_name_t.Noneyed_claw(qualifier,side)
     )   Hex_double_hump_qualifier.all
    )
    @
    [
      Hex_border_connector_name_t.Pyramid(side);
      Hex_border_connector_name_t.Small_pyramid(side)  
    ] ;;

let to_nondefault_molecular_linker nm apex junction = match nm with 
    Hex_border_connector_name_t.Eyed_claw(d1,d2) -> 
         Some(Hex_molecular_linker.constructor [
              Hex_atomic_linker_t.Eyed_claw(d1,d2,Hex_cell.of_int_pair (Option.unpack apex))])
   |Noneyed_claw(_,_) -> None
   |Pyramid(_) -> None 
   |Small_pyramid(_) -> None ;;
    

let to_readable_string = function 
    Hex_border_connector_name_t.Eyed_claw(d1,d2) -> 
         (Hex_cardinal_direction.for_eye_description d1)^"e"^(Hex_cardinal_direction.for_ground_description d2)
   |Noneyed_claw(dh,d) -> let s = (Hex_double_hump_qualifier.to_readable_string dh) in 
                          (String.sub s 0 1)^(Hex_cardinal_direction.for_ground_description d)^(String.sub s 1 1)
   |Pyramid(d) ->  (Hex_cardinal_direction.for_ground_description d)^"py" 
   |Small_pyramid(d) ->  (Hex_cardinal_direction.for_ground_description d)^"sy" ;;

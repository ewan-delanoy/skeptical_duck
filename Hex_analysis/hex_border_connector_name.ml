(* 

#use"Hex_analysis/hex_border_connector_name.ml";;

*)

exception Bad_apex_for_eyed_claw of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t ;;


let for_side side =
    let ortho = Hex_cardinal_direction.orthogonal_directions side in 
    (Image.image (fun specifier->
        Hex_border_connector_name_t.Eyed_claw(specifier,side)
    ) ortho)
    @
    (
     Image.image (
       fun (tbc,data_for_tbc) -> Hex_border_connector_name_t.Typical(tbc,side)
     )   Hex_typical_border_connector_name.prepare_for_journey
    )
    ;;

let to_nondefault_molecular_linker nm apex junction = match nm with 
    Hex_border_connector_name_t.Eyed_claw(d1,d2) -> 
         if apex=None then raise (Bad_apex_for_eyed_claw(d1,d2)) else
         Some(Hex_molecular_linker.constructor [
              Hex_atomic_linker_t.Eyed_claw(d1,d2,Hex_cell.of_int_pair (Option.unpack apex))])
   |_ -> None;;
    
exception Excluded_middle_exn ;;     

let to_readable_string = function 
    Hex_border_connector_name_t.Eyed_claw(d1,d2) -> 
         (Hex_cardinal_direction.for_eye_description d1)^"e"^(Hex_cardinal_direction.for_ground_description d2)
   |Typical(tbc,side) -> Hex_typical_border_connector_name.to_readable_string tbc side ;;                    



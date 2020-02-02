(* 

#use"Hex_analysis/hex_connector.ml";;

*)


let check_entry island cnnctr =
    Hex_island.is_included_in cnnctr.Hex_connector_t.entry island;;

let check_exit island cnnctr =
    Hex_island.is_included_in cnnctr.Hex_connector_t.exit island;;

let oppose dim cnnctr = 
  let on_island = Hex_island.oppose dim
  and on_pairs =  Set_of_poly_pairs.image (Hex_ipair.oppose dim) in 
{
    Hex_connector_t.entry =on_island (cnnctr.Hex_connector_t.entry);
    junction = Set_of_poly_pairs.safe_set 
              ( on_pairs (cnnctr.Hex_connector_t.junction) );
    exit = on_island (cnnctr.Hex_connector_t.entry);
};;

let reflect cnnctr = {
    Hex_connector_t.entry = Hex_island.reflect (cnnctr.Hex_connector_t.entry);
    junction = Set_of_poly_pairs.safe_set 
              ( Set_of_poly_pairs.image Hex_ipair.reflect 
                (cnnctr.Hex_connector_t.junction) );
    exit = Hex_island.reflect (cnnctr.Hex_connector_t.entry);
};;

let reverse cnnctr = {
    cnnctr with  
    Hex_connector_t.entry = (cnnctr.Hex_connector_t.exit);
                    exit  = (cnnctr.Hex_connector_t.entry);
};;

(*
let translates (Hex_dimension_t.D dim) cnnctr = 
   let (Hex_island_t.I(opt1,elts1)) = cnnctr.Hex_connector_t.entry 
   and (Hex_island_t.I(opt2,elts2)) = cnnctr.Hex_connector_t.exit 
   and elts3 = cnnctr.Hex_connector_t.junction in 
   let opt = (if opt1<>None then opt1 else opt2) in 
*)
(* 

#use"Hex_analysis/hex_typical_border_connector_name.ml";;

*)

let prepare_for_journey = 
  [
     Hex_typical_border_connector_name_t.Border_bridge , (Some"bb",None) ;
     Hex_typical_border_connector_name_t.Byssus , (None,Some("sdb","slb","brs","bus")) ; 
     Hex_typical_border_connector_name_t.Pyramid , (Some "py",None) ;
     Hex_typical_border_connector_name_t.Small_pyramid , (Some "sy", None) ;
     Hex_typical_border_connector_name_t.Sybil , (None,Some("bds","bls","srb","sub")) ;
     Hex_typical_border_connector_name_t. Walleye1 , (Some "we1",None)
  ];;


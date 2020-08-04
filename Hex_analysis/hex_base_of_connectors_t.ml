(* 

#use"Hex_analysis/hex_base_of_connectors_t.ml";;

The two Hex_cell_set_t.t objects following the Hex_named_connector_t.t are the yet unoccupied 
cells in the entry and exit respectively, of the connector.

*)

type t= B of (Hex_named_connector_t.t * Hex_cell_set_t.t * Hex_cell_set_t.t ) list ;;

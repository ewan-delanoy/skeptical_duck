(* 

#use"Hex_analysis/hex_polychrome_t.ml";;

A Hex_polychrome_t.t object is a generalization of the idea of a decomposition into connected
components. Initially, the two decompositions coindice. Then, components can be merged into bigger
components using Hex_generalized_connector_t.t objects.

*)

type t= {
    classes    : (Hex_polychrome_label_t.t * Hex_cell_set_t.t) list ;
    labels     : (Hex_cell_t.t * Hex_polychrome_label_t.t) list ;
    free_cells : Hex_cell_set_t.t ;
    neighbors_for_free_cells : (Hex_cell_t.t *Hex_cell_set_t.t) list ; 
    history    : (Hex_polychrome_label_t.t * Hex_generalized_connector_t.t * Hex_polychrome_label_t.t) list;
};;


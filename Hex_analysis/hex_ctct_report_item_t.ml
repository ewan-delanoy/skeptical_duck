(* 

#use"Hex_analysis/hex_ctct_report_item_t.ml";;

*)

type t= {
    opt_side : Hex_cardinal_direction_t.t option ;  
    active_dwellers : Hex_cell_set_t.t ;
    passive_neighbors : Hex_cell_set_t.t ;
};;
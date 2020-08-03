(* 

#use"Hex_analysis/hex_connctcmpnt_report_item_t.ml";;

*)

type t= {
    opt_side : Hex_cardinal_direction_t.t option ;  
    members : Hex_cell_set_t.t ;
    active_neighbors : Hex_cell_set_t.t ;
    passive_neighbors : Hex_cell_set_t.t ;
};;
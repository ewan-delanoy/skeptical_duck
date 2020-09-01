(* 

#use"Hex_analysis/hex_polychrome.ml";;


*)

let of_ctct_report (Hex_ctct_report_t.R(l))=
    let all_free_cells = Hex_cell_set.fold_merge 
      (Image.image (fun item->item.Hex_ctct_report_item_t.passive_neighbors) l) 
     in 
    let temp1 = Ennig.index_everything l in 
    let the_classes =   Image.image (
       fun (idx,item) ->
         (Hex_polychrome_label_t.L(idx),item.Hex_ctct_report_item_t.active_dwellers)
    ) temp1 
    and temp2 = List.flatten(Image.image (
       fun (idx,item) ->
         Hex_cell_set.image (fun cell->(cell,Hex_polychrome_label_t.L(idx))) 
          item.Hex_ctct_report_item_t.active_dwellers
    ) temp1) in 
    let all_active_cells = Hex_cell_set.safe_set (Image.image fst temp2) in 
    let the_labels = Hex_cell_set.image (
      fun cell ->  (cell,List.assoc cell temp2)
    ) all_active_cells in 
    {
      Hex_polychrome_t.classes    = the_classes ;
      labels     = the_labels ;
      free_cells = all_free_cells ;
      history    = [];
    };;

(*

type t= {
    classes    : (Hex_polychrome_label_t.t * Hex_cell_set_t.t) list ;
    labels     : (Hex_cell_t.t * Hex_polychrome_label_t.t) list ;
    free_cells : Hex_cell_set_t.t ;
    history    : (Hex_polychrome_label_t.t * Hex_generalized_connector_t.t * Hex_polychrome_label_t.t) list;
};;

*)
(* 

#use"Hex_analysis/hex_ctct_report_item.ml";;

*)

let common_neighbors item1 item2 =
   Hex_cell_set.intersect 
     (item1.Hex_ctct_report_item_t.passive_neighbors) 
     (item2.Hex_ctct_report_item_t.passive_neighbors);;

let to_island item =
  let anchor =  (match item.Hex_ctct_report_item_t.opt_side with 
    Some(side) -> Hex_anchor_t.Single_anchor(side) 
    |None -> Hex_anchor_t.No_anchor ) 
  and  dwellers =  item.Hex_ctct_report_item_t.active_dwellers in 
  let dwellers2 = Hex_cell_set.image Hex_cell.to_int_pair dwellers in 
  Hex_island_t.I(anchor,Set_of_poly_pairs.safe_set dwellers2);;
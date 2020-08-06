(* 

#use"Hex_analysis/hex_ctct_report_item.ml";;

*)

let adjusted_common_neighbors formal_dim item1 item2 =
   let temp1 = Hex_cell_set.intersect 
     (item1.Hex_ctct_report_item_t.passive_neighbors) 
     (item2.Hex_ctct_report_item_t.passive_neighbors) in 
   if Hex_cell_set.length(temp1)<=2 then temp1 else  
   let opt1 = item1.Hex_ctct_report_item_t.opt_side 
   and opt2 = item2.Hex_ctct_report_item_t.opt_side in 
   let opt = (if opt1=None then opt2 else opt1) in 
   match opt with 
   None -> temp1 
   |Some(side) ->
      let corners = Hex_cardinal_direction.Border.corners formal_dim side in 
      match Option.seek (fun corner -> Hex_cell_set.is_included_in corner temp1) corners with 
      Some(corner)->corner 
      |None -> temp1;;


let to_island item =
  let anchor =  (match item.Hex_ctct_report_item_t.opt_side with 
    Some(side) -> Hex_anchor_t.Single_anchor(side) 
    |None -> Hex_anchor_t.No_anchor ) 
  and  dwellers =  item.Hex_ctct_report_item_t.active_dwellers in 
  let dwellers2 = Hex_cell_set.image Hex_cell.to_int_pair dwellers in 
  Hex_island_t.I(anchor,Set_of_poly_pairs.safe_set dwellers2);;
(* 

#use"Hex_analysis/hex_coconnector_report.ml";;

*)




let first_draft fgame = 
   let formal_dim = fgame.Hex_finished_game_t.dimension  
   and eob = Hex_end_of_battle.of_finished_game fgame in 
   let report = Hex_ctct_report.about_end_of_battle eob 
   and base = Hex_base_of_connectors.from_end_of_battle eob in 
   let (Hex_ctct_report_t.R(l)) = report in 
   let indexed_l = Ennig.index_everything l in 
   let temp1 = Uple.list_of_pairs indexed_l in 
   (Image.image (
     fun ((i,item1),(j,item2))->
        let common = Hex_ctct_report_item.adjusted_common_neighbors formal_dim item1 item2 
        and connectors1 = Hex_base_of_connectors.select_coconnectors base item1 item2 in 
        let connectors = List.filter (
            fun nc->(Hex_named_connector.inner_sea nc)<> common 
        ) connectors1 in  
       ((i,j),
        (common,
         connectors))
   ) temp1);;

let verify_item_in_draft item = 
    let ((i,j),(neighbors,connectors)) = item in 
    let n1 = Hex_cell_set.length neighbors 
    and n2 = List.length connectors in 
    if (n1 > 0) && (n2 > 0)
              then Some("Both neighbors and connectors",item) else
    if n1 > 2 then Some("More than two neighbors",item) else 
    if n2 > 1 then Some("More than one connector",item) else 
    None ;;

let verify_indexed_game (game_idx,fgame) = 
   let draft = first_draft fgame in 
   let problems = Option.filter_and_unpack verify_item_in_draft draft in 
   if problems = []
   then None 
   else Some (Image.image (fun (msg,item)->(game_idx,msg,item)) problems) ;;

let verify_games fgames=
   let indexed_games = Ennig.index_everything fgames in 
   List.flatten(Option.filter_and_unpack verify_indexed_game indexed_games);;

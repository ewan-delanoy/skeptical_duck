(* 

#use"Hex_analysis/hex_check_report.ml";;

*) 

module Private = struct


let ref_for_games = ref [];;
let ref_for_eobs = ref [];;
let ref_for_ctct_reports = ref [];;
let ref_for_bases = ref [];;
let ref_for_drafts = ref [];;

let reset_all () = (
    ref_for_games :=[];
    ref_for_eobs :=[];
    ref_for_ctct_reports :=[];
    ref_for_bases :=[];
    ref_for_drafts :=[];
);;

let analize_game fgame = 
   let eob = Hex_end_of_battle.of_finished_game fgame in 
   let report = Hex_ctct_report.about_end_of_battle eob 
   and base = Hex_base_of_connectors.from_end_of_battle eob in 
   let (Hex_ccnn_report_t.R draft) = Hex_ccnn_report.draft_from_previous_items eob base report in 
   (
    ref_for_games :=fgame::(!ref_for_games);
    ref_for_eobs :=eob::(!ref_for_eobs);
    ref_for_ctct_reports := report::(!ref_for_ctct_reports);
    ref_for_bases := base :: (!ref_for_bases);
    ref_for_drafts := draft :: (!ref_for_drafts);
   );;

let analize_games games = List.iter analize_game (List.rev games);;

let verify_item_in_draft item = 
    let ((i,j),gc) = item in 
    let (Hex_generalized_connector_t.G(neighbors,connectors))=gc in 
    let n1 = Hex_cell_set.length neighbors 
    and n2 = List.length connectors in 
    if (n1 > 0) && (n2 > 0)
              then Some("Both neighbors and connectors",item) else
    if n1 > 2 then Some("More than two neighbors",item) else 
    None ;;

let verify_indexed_draft (draft_idx,draft) = 
   
   let problems = Option.filter_and_unpack verify_item_in_draft draft in 
   if problems = []
   then None 
   else Some (Image.image (fun (msg,item)->(draft_idx,msg,item)) problems) ;;

let verify_all_drafts ()=
   let indexed_drafts = Ennig.index_everything (!ref_for_drafts) in 
   List.flatten(Option.filter_and_unpack verify_indexed_draft indexed_drafts);;

let expand triple = 
   let (pair,(b,ncs)) = triple in 
   if (Hex_cell_set.length b) >1 then Some(triple,b) else 
   if ncs = [] then None else
   Some(triple,Hex_named_connector.inner_sea (List.hd ncs));;

let check_disjointness_on_draft l=
   let temp1 = Option.filter_and_unpack expand l in 
   let temp2 = Uple.list_of_pairs temp1 in 
   List.filter (fun ((triple1,z1),(triple2,z2))->Hex_cell_set.intersects z1 z2) temp2;; 

let check_disjointness_on_indexed_draft (draft_idx,draft) = 
   let problems = check_disjointness_on_draft draft in 
   if problems = []
   then None 
   else Some (Image.image (fun (a,b)->(draft_idx,a,b)) problems) ;;

let check_disjointness_on_all_drafts ()=
   let indexed_drafts = Ennig.index_everything (!ref_for_drafts) in 
   List.flatten(Option.filter_and_unpack check_disjointness_on_indexed_draft indexed_drafts);;

end ;;


let analize_games = Private.analize_games;;
let check_disjointness_on_all_drafts = Private.check_disjointness_on_all_drafts ;; 
let reset_all = Private.reset_all ;; 
let verify_all_drafts = Private.verify_all_drafts;;
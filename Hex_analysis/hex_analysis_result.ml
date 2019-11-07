(* 

#use"Hex_analysis/hex_analysis_result.ml";;

*)

let empty_result = 
{
     Hex_analysis_result_t.mandatory_set = None;
     involved_end_strategies = [] ;
     easy_advancer =  None ;
     strong_moves =  Hex_cell_set_t.S [];
     already_used_moves = Hex_cell_set_t.S [] ;
     usual_move = Hex_cell.of_string "a1"; (* arbitrary, will never be used *)
     sizes = (0,0,0,0,0);
  } ;;

let report_on_danger res=
    match res.Hex_analysis_result_t.mandatory_set with 
    None->""
    |Some(set)->"Danger, because of "^
                (Strung.of_intlist res.Hex_analysis_result_t.involved_end_strategies)^
                ": play in "^(Hex_cell_set.to_string(set))^"\n";; 

   
let report_on_possible_advances res=
     match res.Hex_analysis_result_t.easy_advancer with 
     None->""
     |Some(cell,remaining)->
        let temp1 = Image.image Hex_cell.to_string remaining in 
        let s_remaining = String.concat "," temp1 in 
        "Suggested : "^(Hex_cell.to_string cell)^", from "^s_remaining^"\n";;  


let full_report res = (report_on_danger res)^(report_on_possible_advances res)^"\n";;
  
  
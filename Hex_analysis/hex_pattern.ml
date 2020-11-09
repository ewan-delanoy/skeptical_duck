(* 

#use"Hex_analysis/hex_pattern.ml";;

*)

exception Extract_pattern_exn of Hex_cell_t.t ;;

let extract_pattern eob cells =
    let temp1 = Image.image (
      fun cell -> 
        let lbl=(match Hex_end_of_battle.assess eob cell with 
        Hex_eob_result_t.Ally_territory -> raise (Extract_pattern_exn(cell))
        |Enemy_territory -> true 
        |Unoccupied -> false) in 
        (Hex_cell.to_int_pair cell,lbl)
    ) cells in 
    Hex_pattern_t.Pat temp1 ;;


let oppose dim (Hex_pattern_t.Pat l) = 
   Hex_pattern_t.Pat (Image.image (fun (pair,lbl)->(Hex_ipair.oppose dim pair,lbl)) l) ;;

let reflect (Hex_pattern_t.Pat l) = 
    Hex_pattern_t.Pat (Image.image (fun (pair,lbl)->(Hex_ipair.reflect pair,lbl)) l) ;;

let translate (dx,dy) (Hex_pattern_t.Pat l) =  
  Hex_pattern_t.Pat (Image.image (fun ((x,y),lbl)->((x+dx,y+dy),lbl)) l) ;;


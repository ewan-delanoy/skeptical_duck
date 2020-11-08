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



(* 

#use"Hex_analysis/hex_appendage.ml";;

*)

let opt_of_string text =
   match Hex_eyed_claw.opt_of_string text with 
    None -> None 
   |Some(claw)->Some(Hex_appendage_t.Eyed_claw(claw));;

let powder = function 
   (Hex_appendage_t.Eyed_claw(claw)) -> Hex_eyed_claw.powder claw;;  
(* 

#use"Hex_analysis/hex_appendage.ml";;

*)

let of_concrete_object crobj=
    let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
    Hex_appendage_t.Eyed_claw(Hex_eyed_claw.of_concrete_object arg1);;

let opt_of_string text =
   match Hex_eyed_claw.opt_of_string text with 
    None -> None 
   |Some(claw)->Some(Hex_appendage_t.Eyed_claw(claw));;

let powder = function 
   (Hex_appendage_t.Eyed_claw(claw)) -> Hex_eyed_claw.powder claw;;  

let to_concrete_object (Hex_appendage_t.Eyed_claw(claw))=
   Concrete_object_t.Variant("Hex_"^"appendage_t.Eyed_claw",[
      Hex_eyed_claw.to_concrete_object claw
   ]);;   
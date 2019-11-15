(* 

#use"Hex_analysis/hex_octopus.ml";;

*)

module Private = struct
let elt_of_concrete_object crobj= 
   let (arg1,arg2,_,_,_,_,_)=Concrete_object_field.unwrap_bounded_uple crobj in 
   (
    Hex_appendage.of_concrete_object arg1,
    Concrete_object_field.to_int_pair(arg2)
   );;

let elt_to_concrete_object (adg,p) =
   Concrete_object_t.Uple [
      Hex_appendage.to_concrete_object adg;
      Concrete_object_field.of_int_pair(p)
   ]  ;;

let list_of_concrete_object = 
   Concrete_object_field.to_list elt_of_concrete_object;;
let list_to_concrete_object = 
   Concrete_object_field.of_list elt_to_concrete_object;;

end;;

let actives_and_passives (Hex_octopus_t.O l)=
   let actives=Hex_cell_set.safe_set(Image.image (fun (adg,p)->Hex_ipair.to_cell p) l) in 
   let temp1=List.flatten(Image.image (fun (adg,p)->Hex_appendage.powder adg p) l) in 
   let passives=Hex_cell_set.safe_set(Image.image Hex_ipair.to_cell temp1) in
   (actives,passives);;

let empty_one = Hex_octopus_t.O [];;

let of_concrete_object crobj= 
    let (_,(arg1,_,_,_,_,_,_))=Concrete_object_field.unwrap_bounded_variant crobj in 
    Hex_octopus_t.O(Private.list_of_concrete_object arg1);;

let to_concrete_object (Hex_octopus_t.O l) =
   Concrete_object_t.Variant("Hex_"^"octopus_t.O",[
      Private.list_to_concrete_object l
   ]);;   

(* 

#use"Hex_analysis/hex_extended_molecular.ml";;

*)

module Private = struct  

let passive_part extmol =
     Hex_cell_set.merge 
       (Hex_molecular_linker.support extmol.Hex_extended_molecular_t.molecular_part)
         extmol.Hex_extended_molecular_t.nonmolecular_passive_part;;


let use_ally_move_to_simplify_one cell extmol =
   {
      Hex_extended_molecular_t.molecular_part = Hex_molecular_linker.use_ally_move_to_simplify_one cell (extmol.Hex_extended_molecular_t.molecular_part);
      nonmolecular_passive_part = Hex_cell_set.outsert cell (extmol.Hex_extended_molecular_t.nonmolecular_passive_part);
      active_part = Hex_cell_set.outsert cell (extmol.Hex_extended_molecular_t.active_part);
   };;
   
let withstands_enemy_move cell extmol = 
    (Hex_molecular_linker.withstands_enemy_move cell extmol.Hex_extended_molecular_t.molecular_part)
    &&
    (not(Hex_cell_set.mem cell extmol.Hex_extended_molecular_t.nonmolecular_passive_part))
    &&
    (not(Hex_cell_set.mem cell extmol.Hex_extended_molecular_t.active_part));;    

let of_molecular_and_active_ones (mlclr,active_ones) = 
   {
      Hex_extended_molecular_t.molecular_part = mlclr;
      nonmolecular_passive_part = Hex_cell_set_t.S[];
      active_part = Hex_cell_set.merge (Hex_molecular_linker.active_complement mlclr) active_ones;
   };;

let full_support extmol =
   Hex_cell_set.fold_merge 
    [
        Hex_molecular_linker.support extmol.Hex_extended_molecular_t.molecular_part;
        extmol.Hex_extended_molecular_t.nonmolecular_passive_part;
        extmol.Hex_extended_molecular_t.active_part
    ];; 


let disjunction l =
    let mols = Image.image (fun extmol->extmol.Hex_extended_molecular_t.molecular_part) l in 
    let whole = Hex_molecular_linker.fold_merge mols in 
    let tester1_for_commonality =(fun atm extmol ->
        (Hex_molecular_linker.mem atm extmol.Hex_extended_molecular_t.molecular_part)
        ||
        (Hex_cell_set.does_not_intersect (Hex_atomic_linker.support atm) (full_support extmol))
    ) in 
    let is_common = (fun atm->List.for_all (tester1_for_commonality atm) l) in 
    let common_part = Hex_molecular_linker.filter is_common whole in 
    (* all members differ from the full intersection by 1 elt, which justifies the following *)
    let final_active_part = Hex_cell_set.fold_intersect 
      (Image.image (fun extmol->extmol.Hex_extended_molecular_t.active_part) l) in 
    let total_passive_part = Hex_cell_set.fold_merge(Image.image passive_part l) in 
    let final_passive_part = Hex_cell_set.setminus total_passive_part 
                                   (Hex_molecular_linker.support common_part) in  
    {
      Hex_extended_molecular_t.molecular_part = common_part;
      nonmolecular_passive_part = final_passive_part;
      active_part = final_active_part;
   };;



let salt = "Hex_"^"extended_molecular_t.";;

let molecular_part_label            = salt ^ "molecular_part";;
let nonmolecular_passive_part_label = salt ^ "nonmolecular_passive_part";;
let active_part_label               = salt ^ "active_part";;


let of_concrete_object  crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
      Hex_extended_molecular_t.molecular_part = Hex_molecular_linker.of_concrete_object (g molecular_part_label);
      nonmolecular_passive_part = Hex_cell_set.of_concrete_object (g nonmolecular_passive_part_label);
      active_part = Hex_cell_set.of_concrete_object (g active_part_label);
   };;

let to_concrete_object extmol =
 
   Concrete_object_t.Record([
     molecular_part_label,Hex_molecular_linker.to_concrete_object(extmol.Hex_extended_molecular_t.molecular_part);
     nonmolecular_passive_part_label, Hex_cell_set.to_concrete_object(extmol.Hex_extended_molecular_t.nonmolecular_passive_part);
     active_part_label, Hex_cell_set.to_concrete_object(extmol.Hex_extended_molecular_t.active_part);
   ]);;

end ;;

let active_part extmol = extmol.Hex_extended_molecular_t.active_part;;
let disjunction = Private.disjunction;;
let of_concrete_object = Private.of_concrete_object;;
let of_molecular_and_active_ones = Private.of_molecular_and_active_ones;;
let passive_part = Private.passive_part ;; 
let to_concrete_object = Private.to_concrete_object;;
let use_ally_move_to_simplify_one = Private.use_ally_move_to_simplify_one;;
let withstands_enemy_move = Private.withstands_enemy_move;;

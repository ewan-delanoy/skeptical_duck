(* 

#use"Hex_analysis/hex_extended_molecular.ml";;

*)

module Private = struct  

let passive_part extmol =
     Hex_cell_set.merge 
       (Hex_molecular_linker.support extmol.Hex_extended_molecular_t.molecular_part)
         extmol.Hex_extended_molecular_t.nonmolecular_passive_part;;


let use_ally_move_to_simplify_one_in_usual_case cell extmol =
   {
      Hex_extended_molecular_t.molecular_part = Hex_molecular_linker.use_ally_move_to_simplify_one cell (extmol.Hex_extended_molecular_t.molecular_part);
      nonmolecular_passive_part = Hex_cell_set.outsert cell (extmol.Hex_extended_molecular_t.nonmolecular_passive_part);
      active_part = Hex_cell_set.outsert cell (extmol.Hex_extended_molecular_t.active_part);
   };;
   
let use_ally_move_to_simplify_one cell extmol =
   let old_mlclr = extmol.Hex_extended_molecular_t.molecular_part in 
   match Hex_molecular_linker.test_for_passive_to_active_conversion cell old_mlclr with
   None ->  use_ally_move_to_simplify_one_in_usual_case cell extmol
   |Some(_,new_mlclr) -> 
        {
         extmol with 
         Hex_extended_molecular_t.molecular_part = new_mlclr ;
        };;

let use_enemy_move_to_simplify_one_in_usual_case cell extmol =
   if ( (Hex_cell_set.mem cell (passive_part extmol) )
        ||
        (Hex_cell_set.mem cell extmol.Hex_extended_molecular_t.active_part )
   )  
   then None 
   else Some extmol ;;

let use_enemy_move_to_simplify_one cell extmol =
   let old_mlclr = extmol.Hex_extended_molecular_t.molecular_part in 
   match Hex_molecular_linker.test_for_passive_to_active_conversion cell old_mlclr with
   None ->  use_enemy_move_to_simplify_one_in_usual_case cell extmol
   |Some(other_cell,new_mlclr) -> 
      Some( 
        {
         extmol with 
         Hex_extended_molecular_t.molecular_part = new_mlclr ;
         active_part = Hex_cell_set.insert other_cell (extmol.Hex_extended_molecular_t.active_part)
        });;   

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

let empty_one = 
   {
      Hex_extended_molecular_t.molecular_part = Hex_molecular_linker_t.M [];
      nonmolecular_passive_part = Hex_cell_set_t.S[];
      active_part = Hex_cell_set_t.S[];
   };;

let common_molecular_part l =
     if l=[] then Hex_molecular_linker_t.M[] else 
    let mols = Image.vorstellung (fun extmol->extmol.Hex_extended_molecular_t.molecular_part) l in 
    let whole = Hex_molecular_linker.fold_merge mols in 
    let tester1_for_commonality =(fun atm extmol ->
        (Hex_molecular_linker.mem atm extmol.Hex_extended_molecular_t.molecular_part)
        ||
        (Hex_cell_set.does_not_intersect (Hex_atomic_linker.support atm) (full_support extmol))
    ) in 
    let is_common = (fun atm->List.for_all (tester1_for_commonality atm) l) in 
    Hex_molecular_linker.filter is_common whole ;;

exception Leaky_disjunction of Hex_cell_t.t * Hex_extended_molecular_t.t ;;

let check_active_part_in_disjunction l_pairs=
   let local_active_parts=Image.vorstellung (fun (cell,extmol)->
      Hex_cell_set.outsert cell (extmol.Hex_extended_molecular_t.active_part)
   ) l_pairs  in 
   let global_active_part = Hex_cell_set.fold_merge local_active_parts in 
   match Option.seek (fun (cell,extmol)->Hex_cell_set.mem cell global_active_part ) l_pairs with 
   Some(cell0,extmol0)-> raise(Leaky_disjunction(cell0,extmol0))
  |None -> global_active_part ;;

let disjunction cells older_extmols =
    if older_extmols=[] then empty_one else 
    let common_part = common_molecular_part older_extmols in 
    let final_active_part = check_active_part_in_disjunction (List.combine cells older_extmols) in 
    let total_passive_part = Hex_cell_set.fold_merge((Hex_cell_set.safe_set cells)
                           ::(Image.vorstellung passive_part older_extmols)) in 
    let removable_passive_part = Hex_cell_set.setminus final_active_part 
                                   (Hex_molecular_linker.support common_part) in                     
    let final_passive_part = Hex_cell_set.setminus total_passive_part removable_passive_part in  
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
let common_molecular_part = Private.common_molecular_part;;
let disjunction = Private.disjunction;;
let of_concrete_object = Private.of_concrete_object;;
let of_molecular_and_active_ones = Private.of_molecular_and_active_ones;;
let passive_part = Private.passive_part ;; 
let to_concrete_object = Private.to_concrete_object;;
let use_ally_move_to_simplify_one = Private.use_ally_move_to_simplify_one;;
let use_enemy_move_to_simplify_one = Private.use_enemy_move_to_simplify_one;;

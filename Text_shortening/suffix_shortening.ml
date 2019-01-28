(*

#use"Text_shortening/suffix_shortening.ml";;

*)



module Private = struct
 
let itch x=x.Suffix_shortening_t.initial_touches;;
let atsh x=x.Suffix_shortening_t.atomic_shortenings;;
let mols x=x.Suffix_shortening_t.molecules;;
let molsh x=x.Suffix_shortening_t.molecular_shortenings;;

let compute_molecular_shortenings 
   atm_shrtngs molecules =   
    let unordered=Image.image 
      (
        fun l_atm->
          let short_bridge=String.concat "" l_atm in
          let expanded_atoms=Image.image (
            Shortening_common.go_from_snd atm_shrtngs
          ) l_atm in   
          let long_bridge=String.concat "" l_atm in
          (long_bridge,short_bridge)
      ) molecules in 
    Ordered.diforchan_plaen
      Shortening_common.hash_order
      unordered;;  


end;;


let empty_one = {
    Suffix_shortening_t.initial_touches =[];
    Suffix_shortening_t.atomic_shortenings = [];
    Suffix_shortening_t.molecules = [];
    Suffix_shortening_t.molecular_shortenings = [];
};;    

let add_initial_touch x new_pair =
  let old_itches=Private.itch x in 
  let new_itches=Shortening_common.add_single_pair old_itches new_pair in
  {
    Suffix_shortening_t.initial_touches = new_itches;
    Suffix_shortening_t.atomic_shortenings = (Private.atsh x);
    Suffix_shortening_t.molecules = (Private.mols x);
    Suffix_shortening_t.molecular_shortenings = (Private.molsh x);
  };;

let add_atomic_pair x new_pair =
  let old_atoms=Private.atsh x in 
  let new_atoms=Shortening_common.add_single_pair old_atoms new_pair in
  let mols= Private.mols x in 
  let new_molsh=Private.compute_molecular_shortenings old_atoms mols in 
  {
    Suffix_shortening_t.initial_touches = Private.itch x;
    Suffix_shortening_t.atomic_shortenings = new_atoms;
    Suffix_shortening_t.molecules = mols;
    Suffix_shortening_t.molecular_shortenings = new_molsh;
  };;
    
let add_molecule x new_molecule =
  let atoms=Private.atsh x in 
  let new_mols=new_molecule::(Private.mols x) in
  let new_molsh=Private.compute_molecular_shortenings atoms new_mols in 
  {
    Suffix_shortening_t.initial_touches =Private.itch x;
    Suffix_shortening_t.atomic_shortenings = atoms;
    Suffix_shortening_t.molecules = new_mols;
    Suffix_shortening_t.molecular_shortenings = new_molsh;
  };;

       

let apply x word=
   let word1 = Shortening_common.go_from_fst (Private.itch x) word in 
   match Option.seek 
   (fun (prefix,_)->Substring.begins_with key prefix) 
    () with 
   None->key
   |Some(old_prefix,new_prefix)->
     let culled_form = Cull_string.cobeginning (String.length old_prefix) key in  
     new_prefix^culled_form;;

let reverse (Suffix_shortening_t.S pairs) key=
   match Option.seek (fun (x,y)->y=key) pairs with 
   None->key
   |Some(old_key,key)->old_key;;

let ocaml_notation (Suffix_shortening_t.S pairs)=
  let temp1=Image.image (
      fun (s,t)->
        "   (\""^s^"\",\""^t^"\")"
  ) pairs in 
  let temp2=String.concat ";\n" temp1 in 
  "Pre"^"fix_shortening_t.S(\n[\n\n"^temp2^"\n\n])";;






    


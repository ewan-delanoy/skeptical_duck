(*

#use"Van_der_Waerden/Fixed_width/vdw_string.ml";;

*)



module Private = struct

let (Vdw_max_width_t.MW mw) = Vdw_chosen.max_width ;;

let cardinality (Vdw_string_t.S s) =
    let n = String.length s in 
    List.length(List.filter(fun j->
      (String.get s j)='1')(Ennig.ennig 0 (n-1))) ;; 

let check_at_index (Vdw_string_t.S s) k=
    if (Strung.get s k)<>'1' then true else
    let bound = min mw ((k-1)/2) in 
    List.for_all (fun t->
      (Strung.get s (k-t),Strung.get s (k-2*t)) <> ('1','1')  
    ) (Ennig.ennig 1 bound) ;;

let check_on_interval vdw_s a b =
    List.for_all (check_at_index vdw_s) (Ennig.ennig a b) ;;
    
let is_admissible (Vdw_string_t.S vdw_s) = 
   check_on_interval (Vdw_string_t.S vdw_s)  1 (String.length vdw_s) ;;

let admissible_parts_are_joinable
   (Vdw_string_t.S vdw_part1) (Vdw_string_t.S vdw_part2) =
    let n1 = String.length vdw_part1 
    and n2 = String.length vdw_part2 in  
    check_on_interval (Vdw_string_t.S(vdw_part1^vdw_part2)) (n1+1) (n1+n2) ;;

let join (Vdw_string_t.S vdw_part1) (Vdw_string_t.S vdw_part2) = 
  (Vdw_string_t.S(vdw_part1^vdw_part2)) ;;

end ;;

let admissible_parts_are_joinable = Private.admissible_parts_are_joinable ;;
let cardinality = Private.cardinality ;;
let is_admissible = Private.is_admissible ;;
let join = Private.join ;;
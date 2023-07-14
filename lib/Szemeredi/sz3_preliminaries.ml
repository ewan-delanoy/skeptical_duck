(*

#use"lib/Szemeredi/sz3_preliminaries.ml";;

We make an exception to the rule of not having numbers in module names.
Sz3 is short for "third stab at Szemeredi problem".

*)

type width = Sz3_types.width = W of int ;; 

type breadth = Sz3_types.breadth = B of int ;; 

type finite_int_set = Sz3_types.finite_int_set = FIS of int * (int list) ;; 

type constraint_t = Sz3_types.constraint_t = C of int list;; 

type extension_data = Sz3_types.extension_data  ;; 

type solution = Sz3_types.solution ;; 

type fan = Sz3_types.fan = F of extension_data list ;; 

type mold = Sz3_types.mold = M of (solution list) * extension_data ;;


let i_order = Total_ordering.for_integers ;;
let i_insert = Ordered.insert i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_intersect = Ordered.intersect i_order ;;
let i_intersects = Ordered.intersects i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_length_preserving_sort = Ordered.length_preserving_sort i_order ;;
let i_outsert = Ordered.outsert i_order ;;
let i_setminus = Ordered.setminus i_order ;;


let il_order = Total_ordering.silex_for_intlists ;;
let il_fold_merge = Ordered.fold_merge il_order ;;
let il_insert = Ordered.insert il_order ;;
let il_is_included_in = Ordered.is_included_in il_order ;;
let il_min= Ordered.min il_order ;;
let il_merge = Ordered.merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let t_order = Total_ordering.triple_product 
   i_order i_order (Total_ordering.silex_for_intlists) ;;


module Constraint = struct 

let width (C l) = W((List.nth l 1)-(List.nth l 0)) ;;

end ;;  

module Fan = struct 

   module Private = struct

  let constructor ll =
     let sorted_ll = il_sort ll in 
     F (Ordered_misc.minimal_elts_wrt_inclusion(sorted_ll));;

  end ;;  
     
  let combine_two_conditions (F ll1) (F ll2) =
     let temp1 = Cartesian.product ll1 ll2 in 
     Private.constructor( Image.image (fun (x,y)->i_merge x y) temp1 );; 

  let combine_conditions = function 
      [] -> F[]
     |first_fan :: other_fans ->
        List.fold_left combine_two_conditions first_fan other_fans ;; 

  (*
  let canonical_container_in_hard_case initial_competing_fans =
    let measure = (fun (F rays)->
      i_length_preserving_sort (Image.image List.length rays)
    ) in 
    let temp1 = Image.image measure initial_competing_fans in 
    let smallest_measure = il_min temp1 in 
    let competing_fans = 
        List.filter(fun mz->measure(mz)=smallest_measure)  
            initial_competing_fans in 
    combine_conditions competing_fans ;; 

  let canonical_container sample (F rays) =
     let indexed_rays = Int_range.index_everything rays in 
     let covering_indices = (fun x->
        List.filter_map (fun (idx,ray)->
           if i_is_included_in ray x 
           then Some idx 
          else None   
        ) indexed_rays
      ) in
      let temp1 = Image.image covering_indices sample in 
      let temp2 = Ordered_misc.minimal_transversals temp1 in 
      let (_,temp3) = Min.minimize_it_with_care List.length temp2 in 
      let return_to_original = (fun l->F(Image.image(fun idx->List.assoc idx indexed_rays) l)) in 
      if List.length temp3 = 1 
      then return_to_original (List.hd temp3) 
      else      
      let temp4 = Image.image return_to_original temp3 in
      canonical_container_in_hard_case temp4 ;;
    *)  
      
    let canonical_container sample (F rays) = 
        let appears_at_least_once = (fun 
           ray -> List.exists (i_is_included_in ray) sample) in 
        F(List.filter appears_at_least_once rays) ;;    

    let insert ray (F rays) =  
        Private.constructor(il_insert ray rays);;

    let insert_several more_rays (F rays) =  
        Private.constructor(il_merge (il_sort more_rays) rays);;

    let is_stronger_than (F rays1) (F rays2) =
      List.for_all (fun ray1->List.exists (fun ray2->i_is_included_in ray2 ray1) rays2) rays1 ;;  

    let remove_vertex pivot (F rays) =
        Private.constructor (Image.image (i_outsert pivot) rays) ;;  

end ;;   

module Mold = struct 

let translate d (M(sols, ext)) =
    let tr = (fun x->Image.image(fun t->t+d) x) in 
    M(Image.image tr sols,tr ext) ;; 

end ;;


module Finite_int_set = struct 

  module Private = struct

  let to_usual_int_list (FIS(n,scrappers)) = i_setminus (Int_range.range 1 n) scrappers ;; 
  
  let of_usual_int_list domain =
       if domain = [] then FIS(0,[]) else 
       let n = List.hd(List.rev domain) in 
       FIS(n,i_setminus (Int_range.range 1 n) domain) ;;   

  end ;;

  let decompose_wrt_translation fis_domain = 
    let domain = Private.to_usual_int_list fis_domain in 
    let (d,core_domain) = (match domain with 
      [] -> (0,[])
      | h :: _ -> (h-1, if h=1 then domain else 
                    Image.image (fun x->x-(h-1)) domain 
                   )
    ) in 
    (d,Private.of_usual_int_list core_domain) ;; 

  let max (FIS(n,_)) = n ;; 

  let of_usual_int_list = Private.of_usual_int_list ;; 

  let remove_one_element (FIS(n,scrappers)) k=
       let new_scrappers = i_insert k scrappers in 
       if k <> n then FIS(n,new_scrappers) else 
       let new_z =  Private.to_usual_int_list (FIS(n-1,new_scrappers)) in 
       let new_max = List.hd(List.rev new_z) in 
       FIS(new_max,List.filter (fun t->t<new_max) scrappers) ;;     
  
  
  (*
  
  remove_one_element (FIS(10,[3;7;8;9])) 10 ;;
  remove_one_element (FIS(3,[])) 3 ;;
  
  *)

  let to_usual_int_list = Private.to_usual_int_list ;; 


end ;;    



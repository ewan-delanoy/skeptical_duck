(* 

#use"Hex_analysis/hex_ipair.ml";;

*)


module Private = struct 


let is_valid  (Hex_dimension_t.D dim) (i,j) = 
     (1<=i) && (i<=dim) && (1<=j) && (j<=dim)   ;;   

let neighbors_for_one dim (i,j) =
   let temp1 = List.filter (is_valid dim) 
   [
        j,(i-1);(j+1),(i-1);
       (j-1),i               ;(j+1),i    ;
       (j-1),(i+1); j,(i+1);
   ] in 
   Set_of_poly_pairs.safe_set temp1;;

let neighbors_for_several dim l=
    let temp1 = Image.image (neighbors_for_one dim) l in 
    Set_of_poly_pairs.setminus 
     (Set_of_poly_pairs.fold_merge temp1) 
      (Set_of_poly_pairs.safe_set l) ;;
    
let rec helper1_for_cc_computing 
   (dim,old_whole,news_from_whole,to_be_exhausted) =
    if (Set_of_poly_pairs.length news_from_whole = 0)
    then (old_whole,to_be_exhausted)
    else 
    let temp1 = neighbors_for_several dim 
            (Set_of_poly_pairs.forget_order news_from_whole) in 
    let temp2 = Set_of_poly_pairs.intersect temp1 to_be_exhausted in       
    let new_ones = Set_of_poly_pairs.setminus temp2 old_whole in 
    if (Set_of_poly_pairs.length new_ones = 0)
    then (old_whole,to_be_exhausted)
    else let new_whole = Set_of_poly_pairs.merge old_whole new_ones in 
         let remaining_ones = Set_of_poly_pairs.setminus 
                   to_be_exhausted new_whole in 
         helper1_for_cc_computing 
         (dim,new_whole,new_ones,remaining_ones);;

let rec helper2_for_cc_computing 
   (dim,already_treated,to_be_treated) =
   if (Set_of_poly_pairs.length to_be_treated = 0)
   then List.rev already_treated
   else 
   let      p = Set_of_poly_pairs.hd  to_be_treated
   and others = Set_of_poly_pairs.tl  to_be_treated in
   let (component,remaining_ones) = helper1_for_cc_computing 
   (dim,Set_of_poly_pairs.empty_set,
        Set_of_poly_pairs_t.S[p],others)  in   
   helper2_for_cc_computing 
   (dim,component::already_treated,remaining_ones);;     

let rec helper_for_cc_extraction (dim,already_treated,to_be_treated,bounding_set) =
   if (Set_of_poly_pairs.length to_be_treated = 0)
   then already_treated 
   else 
   let      p = Set_of_poly_pairs.hd  to_be_treated
   and others = Set_of_poly_pairs.tl  to_be_treated in
   let temp1 = neighbors_for_one dim p in 
   let temp2 = Set_of_poly_pairs.intersect temp1 bounding_set in 
   let new_ones = Set_of_poly_pairs.setminus temp2 already_treated in 
   helper_for_cc_extraction(dim,Set_of_poly_pairs.insert p already_treated,
        Set_of_poly_pairs.merge others new_ones,bounding_set);;

let extract_connected_component dim bounding_set x =
   helper_for_cc_extraction (dim,
     Set_of_poly_pairs.empty_set,Set_of_poly_pairs.safe_set [x],bounding_set) ;;
  

end ;;


let compute_connected_components dim l=
   let components = Private.helper2_for_cc_computing 
   (dim,[],Set_of_poly_pairs.safe_set l) in 
   Image.image Set_of_poly_pairs.forget_order components;; 

  
let is_valid  = Private.is_valid  ;;   

let is_connected dim l =
  (List.length(compute_connected_components dim l)=1);;

let neighbors_for_several = Private.neighbors_for_several ;;

let oppose (Hex_dimension_t.D dim) (x,y) = (dim+1-x,dim+1-y);;

let reflect (x,y) = (y,x);;
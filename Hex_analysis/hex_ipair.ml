(* 

#use"Hex_analysis/hex_ipair.ml";;

*)


module Private = struct 


let is_valid  (Hex_dimension_t.D dim) (i,j) = 
     (1<=i) && (i<=dim) && (1<=j) && (j<=dim)   ;;   

let neighbors_for_one dim (j,i) =
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
  
let rec helper_for_cc_enumeration
   (dim,previous_results,previous_whole,to_be_treated,bounding_set) =
   if (Set_of_poly_pairs.length to_be_treated = 0)
   then List.rev previous_results
   else 
   let      p = Set_of_poly_pairs.hd  to_be_treated
   and others = Set_of_poly_pairs.tl  to_be_treated in
   let component = extract_connected_component dim bounding_set p in 
   let l_component = Set_of_poly_pairs.forget_order component in 
   let current_results = l_component :: previous_results in 
   let current_whole = Set_of_poly_pairs.merge component previous_whole in 
   let remaining_ones = Set_of_poly_pairs.setminus others current_whole in 
   helper_for_cc_enumeration(dim,current_results,current_whole,
        remaining_ones,bounding_set);;  

let enumerate_components dim bounding_set = 
   helper_for_cc_enumeration(dim,[],Set_of_poly_pairs.empty_set,
        bounding_set,bounding_set);;  

end ;;


let bounds_for_authorized_translations formal_dim l = 
   let (Hex_dimension_t.D dim) = formal_dim in 
   let elts = Set_of_poly_pairs.sort l in 
   let abscissas = Set_of_poly_pairs.image fst elts 
   and ordinates = Set_of_poly_pairs.image snd elts in 
   let xmin = Min.list abscissas and xmax = Max.list abscissas 
   and ymin = Min.list ordinates and ymax = Max.list ordinates in    
   (1-xmin,dim-xmax,1-ymin,dim-ymax) ;;


let compute_connected_components dim l=
   Private.enumerate_components dim (Set_of_poly_pairs.safe_set l);; 

  
let is_valid  = Private.is_valid  ;;   

let is_connected dim l =
  (List.length(compute_connected_components dim l)=1);;

let neighbors_for_several = Private.neighbors_for_several ;;

let oppose (Hex_dimension_t.D dim) (x,y) = (dim+1-x,dim+1-y);;

let reflect (x,y) = (y,x);;
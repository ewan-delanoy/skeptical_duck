(* 

#use"Hex_analysis/hex_common.ml";;

*)



module Private = struct 

let joiner = " - ";;

let cell_list_to_string l=
  String.concat joiner (Image.image Hex_cell.to_string l);;


let cell_pair_to_string (cell1,cell2)=cell_list_to_string [cell1;cell2];;

let ipair_is_valid  (Hex_dimension_t.D dim) (i,j) = 
     (1<=i) && (i<=dim) && (1<=j) && (j<=dim)   ;;   

let neighbors_for_one dim (i,j) =
   let temp1 = List.filter (ipair_is_valid dim) 
   [
        j,(i-1);(j+1),(i-1);
       (j-1),i               ;(j+1),i    ;
       (j-1),(i+1); j,(i+1);
   ] in 
   Set_of_poly_pairs.safe_set temp1;;

let neighbors_for_several dim z=
    let temp1 = Set_of_poly_pairs.image (neighbors_for_one dim) z in 
    Set_of_poly_pairs.setminus 
     (Set_of_poly_pairs.fold_merge temp1) z ;;
    
let rec helper1_for_cc_computing 
   (dim,old_whole,news_from_whole,to_be_exhausted) =
    if (Set_of_poly_pairs.length news_from_whole = 0)
    then (old_whole,to_be_exhausted)
    else 
    let temp1 = neighbors_for_several dim news_from_whole in 
    let new_ones = Set_of_poly_pairs.setminus temp1 old_whole in 
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

end ;;

let all_cells (Hex_dimension_t.D dimension)=
   let temp1=Ennig.doyle (fun j->String.make 1 (char_of_int(j+96))) 1 dimension 
   and temp2=Ennig.doyle (fun i->string_of_int i) 1 dimension in 
   let temp3=Cartesian.product temp1 temp2 in 
   let temp4=Image.image (fun (sj,si)->Hex_cell.of_string(sj^si)) temp3 in 
   Hex_cell_set.safe_set temp4;;



let cell_list_of_string s =
  let temp1=Str.split (Str.regexp_string Private.joiner) s in 
    Image.image Hex_cell.of_string temp1;;

let cell_list_to_pretty_string l=
  let (pairs,optional_last)=Listennou.divide_by_two l in 
  let part1= String.concat " | " (Image.image Private.cell_pair_to_string pairs) in 
  let part2=(match optional_last with 
     None->""
     |Some(cell)->Hex_cell.to_string cell
  ) in 
  if (part1="")||(part2="")
  then part1^part2
  else part1^Private.joiner^part2;;

let cell_list_to_string = Private.cell_list_to_string ;;

let cell_pair_of_string text =
   let l=cell_list_of_string text in 
   (List.nth l 0,List.nth l 1);;

let cell_pair_to_string = Private.cell_pair_to_string;;

let compute_connected_components dim l=
   let components = Private.helper2_for_cc_computing 
   (dim,[],Set_of_poly_pairs.safe_set l) in 
   Image.image Set_of_poly_pairs.forget_order components;; 

let has_just_played preceding_moves=
   if ((List.length preceding_moves) mod 2=1)  
   then Hex_player_t.First_player
   else Hex_player_t.Second_player;;
  
let ipair_is_valid  = Private.ipair_is_valid  ;;   

let is_connected dim l =
  (List.length(compute_connected_components dim l)=1);;


let next_one_to_play preceding_moves=
   if ((List.length preceding_moves) mod 2=0)  
   then Hex_player_t.First_player
   else Hex_player_t.Second_player;;

let parse_list_of_moves s=
   let temp1=Cull_string.extract_intervals_in_wrt_separator s "," in 
   Image.image Hex_cell.of_string temp1;;


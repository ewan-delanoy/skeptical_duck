(*

#use"Van_der_Waerden/vdw_preliminaries.ml";;

*)

   
let oint = Total_ordering.for_integers ;;   
let ointlist = Total_ordering.silex_compare Total_ordering.for_integers ;;
  
let extract_core_and_simplify ll = 
  if ll = [] then ([],[]) else 
  let core = Ordered.fold_intersect oint ll in 
  (core,Image.image (fun l->Ordered.setminus oint l core) ll) ;;          
  
let diameter soi =
  if List.length(soi)<2 then 0 else 
  (List.hd(List.rev soi)) - (List.hd soi) + 1  ;;  
      
let look_for_arithmetic_progressions_in_with_width_equal_to
         soi width=
  if List.length(soi)<3 then [] else 
  let temp1 = Image.image (fun x->[x;x+width;x+2*width]) soi in 
  List.filter (fun obstruction ->
    Ordered.is_included_in oint obstruction soi) temp1 ;;  

let level_two_translate translation ll=
    Image.image (Ordered.merge oint translation) ll ;;

let max_easy_length = 15 ;;

exception Set_too_large_to_be_solved_naively ;; 
    
let naive_power_set soi =
  if (List.length soi) > max_easy_length 
  then raise Set_too_large_to_be_solved_naively
  else Ordered.sort ointlist (Listennou.power_set soi) ;;  
      

  let obstructions_passing_through_point_above width x =
    Ennig.doyle (fun t->[x-2*t;x-t]) 1 width ;;
 
 let obstructions_passing_through_one_of_points_above 
    (width,bound) soi =
   let l = Set_of_integers.forget_order soi in  
   let temp1 = List.flatten 
     (Image.image 
     (obstructions_passing_through_point_above width) l)  in 
   List.filter (List.for_all(fun x->x<bound)) temp1  ;;

 let obstructions_passing_through_two_points_above
   (width,bound) soi = 
   let l = Set_of_integers.forget_order soi in  
   let temp1 = Option.filter_and_unpack (
     fun (x,y)->
       let z = 2*x -y in 
       if (y-x<=width)&&(z<bound)
       then Some z
     else None  
   ) (Uple.list_of_pairs l) in 
   Ordered.sort oint temp1 ;;

 let minimal_obstructions_corresponding_to_above 
   (Vdw_max_width_t.MW width) bound soi =
   let part1 = obstructions_passing_through_one_of_points_above (width,bound) soi
   and pre_part2 = obstructions_passing_through_two_points_above (width,bound) soi  in 
   let part2 = Image.image (fun x->[x]) pre_part2 in 
   let temp1 = Ordered.select_minimal_elements_for_inclusion oint (part1 @ part2) in 
   Ordered.sort Total_ordering.cardinality_then_diameter temp1 ;;
     

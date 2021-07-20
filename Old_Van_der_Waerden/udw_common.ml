(*

#use"Old_Van_der_Waerden/udw_common.ml";;

*)

exception Unknown_width_in_measure_exn of int ;;
exception Unknown_width_in_lower_measure_exn of int ;;

module Private = struct 

  let oord = Total_ordering.silex_compare Total_ordering.for_integers ;;   
  let oint = Total_ordering.for_integers ;;     
  
  let extract_core_and_simplify ll = 
      if ll = [] then ([],[]) else 
      let core = Ordered.fold_intersect oint ll in 
      (core,Image.image (fun l->Ordered.setminus oint l core) ll) ;;          
  
  let diameter soi =
        if Set_of_integers.length(soi)<2 then 0 else 
        (Set_of_integers.max soi) - (Set_of_integers.min soi) + 1  ;;  
      
  let look_for_arithmetic_progressions_in_with_width_equal_to
         soi width=
        if Set_of_integers.length(soi)<3 then [] else 
        let temp1 = Set_of_integers.image (fun x->Set_of_integers.safe_set [x;x+width;x+2*width]) soi in 
        List.filter (fun obstruction ->Set_of_integers.is_included_in obstruction soi) temp1 ;;  

  let look_for_arithmetic_progressions_in_with_width_up_to width soi=
      let max_width = (if width<1 then ((diameter soi)-1)/2 else width) in 
      List.rev(List.flatten(Ennig.doyle 
       (look_for_arithmetic_progressions_in_with_width_equal_to soi) 1 max_width));;
  
  

  let test_for_admissibility constraints soi = match constraints with 
      Udw_list_of_constraints_t.Defined_by_max_width(max_width) ->
        ((look_for_arithmetic_progressions_in_with_width_up_to max_width soi) = [])
     |General_case obstructions ->
      List.for_all (fun obs->not(Set_of_integers.is_included_in obs soi )) obstructions ;;
          
  let test_joinability criterion l1 l2 =
    test_for_admissibility criterion 
    (Set_of_integers.safe_set (l1@l2)) ;;

  let level_two_translate translation ll=
    Image.image (Ordered.merge oint translation) ll ;;
  
  let max_easy_length = 15 ;;

  exception Set_too_large_to_be_solved_naively ;; 
  
  let naive_power_set soi =
      if (Set_of_integers.length soi) > max_easy_length 
      then raise Set_too_large_to_be_solved_naively
      else Listennou.power_set (Set_of_integers.forget_order soi) ;;  
    
  let naive_restricted_power_set constraints soi =
        let temp1 = naive_power_set soi in 
        List.filter (fun l-> test_for_admissibility constraints (Set_of_integers.safe_set l)) temp1 ;;
  
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
    (Udw_max_width_t.MW width) bound soi =
    let part1 = obstructions_passing_through_one_of_points_above (width,bound) soi
    and pre_part2 = obstructions_passing_through_two_points_above (width,bound) soi  in 
    let part2 = Image.image (fun x->[x]) pre_part2 in 
    let temp1 = Ordered.select_minimal_elements_for_inclusion oint (part1 @ part2) in 
    Ordered.sort Total_ordering.cardinality_then_diameter temp1 ;;
    

  let measure_in_width_four n =
      if n<1 then 0 else 
      let q=(n/9) in 
      match n mod 9 with
        0 -> 4*q+1 
      |1 -> 4*q+1
      |2 -> 4*q+2  
      |3 -> 4*q+2
      |4 -> 4*q+3
      |5 -> 4*q+4
      |6 -> 4*q+4  
      |7 -> 4*q+4
      |8 -> 4*q+4 
      | _ -> failwith("unforeseen");;     

  let lower_measure_in_width_four n =
        if n<1 then 0 else 
        let q=(n/9) in 
        match n mod 9 with
          0 -> 4*q
        |1 -> 4*q
        |2 -> 4*q 
        |3 -> 4*q
        |4 -> 4*q+1
        |5 -> 4*q+1
        |6 -> 4*q+2  
        |7 -> 4*q+2
        |8 -> 4*q+3 
        | _ -> failwith("unforeseen");;  

let measure (Udw_max_width_t.MW mw) n=
  match mw with 
  4 -> measure_in_width_four n 
  | _ -> raise( Unknown_width_in_measure_exn mw);;

let lower_measure (Udw_max_width_t.MW mw) n=
  match mw with 
  4 -> lower_measure_in_width_four n 
  | _ -> raise( Unknown_width_in_lower_measure_exn mw);;  

end ;;  

module Music = struct 

  let prod a b =
    let temp1 = Cartesian.product a b in
    let temp2 = Image.image (fun (x,y)->
       Set_of_integers.safe_set (x@y)
     ) temp1  in 
    let temp3 = Ordered_misc.minimal_elts_wrt_inclusion temp2 in 
    let temp4 = Image.image Set_of_integers.forget_order temp3 in 
    Ordered.sort Private.oord temp4 ;;

    let shadow ll obstructions = 
      let indexed_obstructions = Ennig.index_everything obstructions in 
      let tempf1 = (fun l->
           Set_of_integers.safe_set(Option.filter_and_unpack (
               fun (idx,obs) ->
                if Ordered.is_included_in Private.oint obs l 
                then Some idx 
                else None      
           ) indexed_obstructions)) in 
      let temp1 = Image.image tempf1 ll in     
      let temp2 = Ordered_misc.minimal_transversals temp1 in 
      let tempf2 = (fun k->List.nth obstructions (k-1)) in 
      Image.image (Set_of_integers.image tempf2) temp2 ;;
      
    let effective_blowup n m ll=
      let rightmost_side = Ennig.ennig m n in 
      let temp1 = Image.image (Ordered.intersect Private.oint rightmost_side) ll in 
      let temp2 = Ordered.sort Private.oord temp1 in 
      let tempf1 = ( fun x y->
        if (Ordered.intersect Private.oint rightmost_side y)<> x
        then None 
        else Some(x,Ordered.setminus Private.oint y rightmost_side)
      ) in 
      let tempf2 = (fun x->Option.unpack(Option.find_and_stop (tempf1 x) ll)) in 
      Image.image tempf2 temp2;;       

end ;;  


let decompose max_width n d=
    let delta = (Private.measure max_width n) -(Private.measure max_width  (n-1)) in 
    let draft = [ (n-1,d-delta+1),[n] ; (n-1,d-delta),[]] in 
    List.filter ( fun ((n1,d1),l) -> d1>=0 ) draft ;;



let extended_partition selector  ll= 
    let (temp1,temp2) = List.partition 
    (Ordered.is_included_in Private.oint selector ) ll in 
     (Private.extract_core_and_simplify temp1,
      Private.extract_core_and_simplify temp2) ;;

let generic_computer (Udw_max_width_t.MW max_width) n =   
        let unordered_base = 
          Private.naive_restricted_power_set
         ( Udw_list_of_constraints_t.Defined_by_max_width max_width) 
           (Set_of_integers.safe_set(Ennig.ennig 1 n))
        in 
        Ordered.sort Private.oord unordered_base ;;

exception Homogeneous_translation_exn of (int list) * ( (int list) * (int list) );;

let homogeneous_translation criterion ll translation =
   match ll with 
   [] -> Udw_homogeneous_translation_result_t.Nothing_taken
   | head :: others ->
     let tester = Private.test_joinability criterion translation in 
     let is_joinable = tester head in 
     match Option.seek (fun l1->(tester l1)<>is_joinable) others with 
      (Some l1) -> raise (Homogeneous_translation_exn(translation,(head,l1)))
     | None -> 
       if is_joinable
       then Udw_homogeneous_translation_result_t.All_taken(Image.image (fun l->
        Ordered.safe_set Private.oint (l@translation)) ll)
       else Udw_homogeneous_translation_result_t.Nothing_taken;;


let lower_measure = Private.lower_measure ;; 
let measure = Private.measure ;;        
let minimal_obstructions_corresponding_to_above = Private.minimal_obstructions_corresponding_to_above ;;

let reconstruct parts =
  let temp1 = Image.image (fun (a,b)->
    Private.level_two_translate a b) parts in 
  Ordered.fold_merge Private.oord temp1 ;;

let rightmost_blowup max_width n r =
   let big_m = measure max_width n 
   and small_m = measure max_width (n-r) in   
   let temp1 = generic_computer max_width r in 
   Option.filter_and_unpack (fun small_sol->
    let remaining_length = big_m - (List.length small_sol) 
    and translated_sol = Image.image (fun t->n-r+t) small_sol in 
    let d = small_m-remaining_length in 
    if d<0 then None else Some((n-r,d),translated_sol) 
   ) temp1 ;;

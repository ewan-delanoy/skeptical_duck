(*

#use"lib/Szemeredi/sz_preliminaries.ml";;

*)

module Private = struct

  let i_mem = Ordered.mem Total_ordering.for_integers  ;;
  let i_outsert = Ordered.outsert Total_ordering.for_integers  ;;
  let i_setminus = Ordered.setminus Total_ordering.for_integers  ;;  
  let i_sort = Ordered.safe_set Total_ordering.for_integers  ;;
  let i_is_included_in = Ordered.is_included_in Total_ordering.for_integers ;;
  let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
  let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;
  
  let diameter soi =
    if List.length(soi)<2 then 0 else 
    (List.hd(List.rev soi)) - (List.hd soi) + 1  ;; 
  
  let look_for_arithmetic_progressions_in_with_width_equal_to
           soi width=
    if List.length(soi)<3 then [] else 
    let temp1 = Image.image (fun x->[x;x+width;x+2*width]) soi in 
    List.filter (fun obstruction -> i_is_included_in obstruction soi) temp1 ;;  
  
  let look_for_arithmetic_progressions_in_with_width_up_to 
    (Sz_max_width_t.MW width) soi=
    let max_width = (if width<1 then ((diameter soi)-1)/2 else width) in 
    List.rev(List.flatten(Int_range.scale 
    (look_for_arithmetic_progressions_in_with_width_equal_to soi) 1 max_width));;
    

  let test_for_admissibility max_width soi = 
      ((look_for_arithmetic_progressions_in_with_width_up_to max_width soi) = [])
  
  let extender max_width ll x=
    let temp1 = Image.image (fun y->i_sort (y@[x])) ll in 
    let temp2 = List.filter (test_for_admissibility max_width) temp1 in
    il_merge ll temp2 ;;
  
  let restricted_power_set =Memoized.recursive (fun old_f (max_width,soi) ->
    if soi = [] 
    then [[]]  
    else 
    let temp1 = List.rev soi in 
    let (last_elt,temp2) = Listennou.ht temp1 in 
    let soi2 = List.rev temp2 in 
    extender max_width (old_f (max_width,soi2)) last_elt  
  );;
  
  let arithmetic_progressions_in_interval_with_width_equal_to (a,b) width  =
    Int_range.scale (fun x->[x;x+width;x+2*width]) a (b-2*width)  ;; 
  
  let arithmetic_progressions_with_width_up_to_in_interval 
    interval (Sz_max_width_t.MW width)  = 
    let (a,b) = interval in 
    let max_width = (if width<1 then (b-a)/2 else width) in 
    List.rev(List.flatten(Int_range.scale 
    (arithmetic_progressions_in_interval_with_width_equal_to interval) 1 max_width));;  ;;   
  
  let force_subset_in_arbitrary_set (Sz_max_width_t.MW width) subset soi =
      let old_obses = look_for_arithmetic_progressions_in_with_width_up_to 
      (Sz_max_width_t.MW width) soi in 
      let new_obses = Option.filter_and_unpack (
        fun old_obstruction ->
          let new_obstruction = i_setminus old_obstruction subset in
          if (new_obstruction = old_obstruction) || (new_obstruction = []) 
          then None 
          else Some new_obstruction  
      ) old_obses in 
      Ordered_misc.minimal_elts_wrt_inclusion (il_sort new_obses);;
  
  
  let force_subset_in_interval (Sz_max_width_t.MW width) subset interval =
      let old_obses = arithmetic_progressions_with_width_up_to_in_interval interval (Sz_max_width_t.MW width) in 
      let new_obses = Option.filter_and_unpack (
        fun old_obstruction ->
          let new_obstruction = i_setminus old_obstruction subset in
          if (new_obstruction = old_obstruction) || (new_obstruction = []) 
          then None 
          else Some new_obstruction  
      ) old_obses in 
      Ordered_misc.minimal_elts_wrt_inclusion (il_sort new_obses);;
 

let careful_translate d x = if d=0 then x else Image.image (fun t->t+d) x ;;

let translation_decomposition x = match x with 
    [] -> (0, [])
    | first_elt::_others -> 
        let d = (first_elt-1) in 
        (d,careful_translate (-d) x) ;;

let transdist_decomposition max_dist x =
  let parts = Arithmetic_list.decompose_into_far_apart_components   
  ~max_inner_distance:max_dist x in 
  Image.image translation_decomposition  parts ;;

let transdist_components max_dist x =
    Image.image snd (transdist_decomposition max_dist x) ;;  

let evaluate_using_translation_and_distancing max_dist f_opt x=
  let temp0 = transdist_decomposition max_dist x in 
  let temp1 = Image.image (fun (d,y)->
    match f_opt y with 
     None -> (None,Some (d,y))
    |Some sy ->(Some(careful_translate d sy),None)
    ) temp0 in 
  let (_good_temp1,bad_temp1) = List.partition (fun (_opt_good,opt_bad)->opt_bad=None) temp1 in 
  if bad_temp1 = []
  then let full_solution = List.flatten(Image.image (fun (opt_good,_)->Option.unpack opt_good) temp1) in 
        (Some full_solution,None) 
  else (None,Some (temp1,bad_temp1));;

  end ;;
  

  let contained_arithmetic_progressions = Private.look_for_arithmetic_progressions_in_with_width_up_to ;;
  let decompose_using_translation_and_distancing = Private.transdist_components ;;
  let evaluate_using_translation_and_distancing = Private.evaluate_using_translation_and_distancing ;;
  let force_subset_in_arbitrary_set = Private.force_subset_in_arbitrary_set ;;
  let force_subset_in_interval = Private.force_subset_in_interval ;;
  let restricted_power_set = Private.restricted_power_set ;;
  let test_for_admissibility = Private.test_for_admissibility ;;
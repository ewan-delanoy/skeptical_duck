(*

#use"lib/preserve_initial_ordering.ml";;

*)

let preserve_initial_ordering l=
    let rec tempf=(fun
    (treated_part,ordered_treated_part,yet_untreated)->
      match yet_untreated with
      []->List.flatten(List.rev(treated_part))
      |x::remains->
        let better_x=List.filter 
        (fun y->Set_of_polys.nmem y ordered_treated_part) x in
        if better_x=[]
        then tempf(treated_part,ordered_treated_part,remains)
        else
        let temp1=Set_of_polys.merge(Set_of_polys.sort x) ordered_treated_part in
        tempf(better_x::treated_part,temp1,remains)
    ) in
   tempf([],Set_of_polys.empty_set,l);;

let and_mark_endings l=
	 let rec tempf=(fun
    (treated_part,ordered_treated_part,yet_untreated)->
      match yet_untreated with
      []->List.flatten(List.rev(treated_part))
      |x::remains->
        let better_x=List.filter 
        (fun y->Set_of_polys.nmem y ordered_treated_part) x in
        if better_x=[]
        then tempf(treated_part,ordered_treated_part,remains)
        else
        let temp1=Set_of_polys.merge(Set_of_polys.sort x) ordered_treated_part in
        let temp2=List.rev(better_x) in
        let temp3=(List.hd temp2,Is_an_ending_or_not.Yes)::
        (Image.image (fun t->(t,Is_an_ending_or_not.No)) (List.tl temp2)) in
        tempf((List.rev temp3)::treated_part,temp1,remains)
    ) in
   tempf([],Set_of_polys.empty_set,l);;

(*

preserve_initial_ordering
  [[18; 4; 14]; [17; 10; 16]; [16; 19]; [13; 19]; [13; 18; 3]];;

and_mark_endings
  [[18; 4; 14]; [17; 10; 16]; [16; 19]; [13; 19]; [13; 18; 3]];;

*)

               
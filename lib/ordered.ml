(*
 
#use"lib/ordered.ml";;

*)

exception The_empty_set_has_no_min ;; 

module Private = struct 

  let intersect (cmpr:'a Total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v,accu)->
        if u=[] then (List.rev(accu)) else
        if v=[] then (List.rev(accu)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
        match cmpr(xu)(xv) with
         Total_ordering_result_t.Lower->tempf(yu,v,accu)
        |Total_ordering_result_t.Equal->tempf(yu,yv,xu::accu)
        |Total_ordering_result_t.Greater->tempf(u,yv,accu)
      ) in
      tempf(ox,oy,[]);;
  
  let is_increasing (cmpr:'a Total_ordering_t.t) l=
    if List.length(l)<2 then true else
    let rec tempf=(function
    (a,to_be_treated)->match to_be_treated with
     []->true
     |b::others->if (cmpr(a)(b)=Total_ordering_result_t.Lower)
                   then tempf(b,others)
                   else false
    ) in
    tempf(List.hd l,List.tl l);;
    
  
  let merge (cmpr:'a Total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v,accu)->
        if u=[] then (List.rev_append(accu)(v)) else
        if v=[] then (List.rev_append(accu)(u)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
        Total_ordering_result_t.Lower->tempf(yu,v,xu::accu)
      |Total_ordering_result_t.Equal->tempf(yu,yv,xu::accu)
      |Total_ordering_result_t.Greater->tempf(u,yv,xv::accu)
      ) in
      tempf(ox,oy,[]);;
  
  let length_preserving_merge (cmpr:'a Total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v,accu)->
        if u=[] then (List.rev_append(accu)(v)) else
        if v=[] then (List.rev_append(accu)(u)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
        Total_ordering_result_t.Lower->tempf(yu,v,xu::accu)
      |Total_ordering_result_t.Equal->tempf(yu,yv,xu::xv::accu)
      |Total_ordering_result_t.Greater->tempf(u,yv,xv::accu)
      ) in
      tempf(ox,oy,[]);;    
  
  let setminus (cmpr:'a Total_ordering_t.t) ox oy=
      let rec tempf=
      (function (u,v,accu)->
        if u=[] then (List.rev(accu)) else
        if v=[] then (List.rev_append(accu)(u)) else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
        match cmpr(xu)(xv) with
           Total_ordering_result_t.Lower->tempf(yu,v,xu::accu)
          |Total_ordering_result_t.Equal->tempf(yu,yv,accu)
          |Total_ordering_result_t.Greater->tempf(u,yv,accu)
     ) in
     tempf(ox,oy,[]);;
  
  let rec sort (cmpr:'a Total_ordering_t.t) x=
    if List.length(x)<2
    then x
    else let temp1=Partition_list.split_in_half(x) in
         let y1=sort(cmpr)(fst temp1)
         and y2=sort(cmpr)(snd temp1) in
         merge cmpr y1 y2;;      

  let rec length_preserving_sort (cmpr:'a Total_ordering_t.t) x=
         if List.length(x)<2
         then x
         else let temp1=Partition_list.split_in_half(x) in
              let y1=length_preserving_sort(cmpr)(fst temp1)
              and y2=length_preserving_sort(cmpr)(snd temp1) in
              length_preserving_merge cmpr y1 y2;;       
  
  let rec helper_for_min (cmpr:'a Total_ordering_t.t) (current_min,to_be_treated) =
    match to_be_treated with 
     [] -> current_min
    | elt :: others ->
    match cmpr current_min elt with
    Total_ordering_result_t.Lower
   |Total_ordering_result_t.Equal->helper_for_min cmpr (current_min,others)
   |Total_ordering_result_t.Greater->helper_for_min cmpr (elt,others) ;; 


  let min (cmpr:'a Total_ordering_t.t) = function 
       [] -> raise(The_empty_set_has_no_min)
      |elt :: others -> helper_for_min cmpr (elt,others) ;; 

  let is_included_in (cmpr:'a Total_ordering_t.t) ox oy=
         let rec tempf=(function (u,v)->
           if u=[] then true else
           if v=[] then false else
           let xu=List.hd(u) and yu=List.tl(u) 
           and xv=List.hd(v) and yv=List.tl(v) in
           match cmpr(xu)(xv) with
             Total_ordering_result_t.Lower->false
           |Total_ordering_result_t.Equal->tempf(yu,yv)
           |Total_ordering_result_t.Greater->tempf(u,yv)
         ) in
         tempf(ox,oy);;       
  
  let helper1_for_minimal_elements_selection (cmpr:'a Total_ordering_t.t)  
         comparator =
         let rec tempf = (fun
         (treated,to_be_treated) -> match to_be_treated with 
         [] -> (None,List.rev treated) 
        |new_item :: others ->
           if new_item = comparator 
           then (* ignore and continue *) tempf (treated,others)
           else       
           if is_included_in cmpr new_item comparator
           then (* finish *)  (Some new_item,[])
           else 
           if is_included_in cmpr comparator new_item
           then tempf (treated,others)
           else tempf (new_item::treated,others)  
         ) in tempf ;;    
       
  let helper2_for_minimal_elements_selection (cmpr:'a Total_ordering_t.t)  =
          let rec tempf = (fun 
         (treated,to_be_treated) -> match to_be_treated with 
          [] -> List.rev treated 
         |new_item :: others ->
           let (opt,checked_subset) = 
             helper1_for_minimal_elements_selection cmpr new_item ([],others) in 
           if opt<>None 
           then tempf(treated,others) 
           else tempf(new_item::treated,checked_subset)) in   
         tempf ;;
           
  let select_minimal_elements_for_inclusion tr ll=
         helper2_for_minimal_elements_selection tr ([],ll) ;;
    
  
  end;;
  
  
  let diff (cmpr: 'a Total_ordering_t.t) =
            let rec tempf=(fun
              (treated_bc,treated_b,treated_c,to_be_treated1,to_be_treated2)->
                match to_be_treated1 with
                []->(treated_bc,treated_b,List.rev_append treated_c to_be_treated2)
                |(a1,b1)::others1->
                (
                  match to_be_treated2 with
                []->(treated_bc,List.rev_append treated_b to_be_treated1,treated_c)     
                |(a2,c2)::others2->
                  (
                    match cmpr a1 a2 with
                    Total_ordering_result_t.Lower->
                      tempf(treated_bc,(a1,b1)::treated_b,treated_c,others1,to_be_treated2)
                    |Total_ordering_result_t.Greater->
                    tempf(treated_bc,treated_b,(a2,c2)::treated_c,to_be_treated1,others2)
                    |Total_ordering_result_t.Equal->
                    tempf((a1,b1,c2)::treated_bc,treated_b,treated_c,others1,others2)  
                  )
                )      
            ) in
            tempf;;   
  
  let does_not_intersect (cmpr:'a Total_ordering_t.t) ox oy=
      let rec tempf=(function (u,v)->
          if (u=[])||(v=[]) then true else
          let xu=List.hd(u) and yu=List.tl(u) 
          and xv=List.hd(v) and yv=List.tl(v) in
          match cmpr(xu)(xv) with
            Total_ordering_result_t.Lower->tempf(yu,v)
          |Total_ordering_result_t.Equal->false
          |Total_ordering_result_t.Greater->tempf(u,yv)
      ) in
      tempf(ox,oy);;
  
  exception Empty_intersection_undefined;;    
  
  let fold_intersect cmpr=function
     []->raise(Empty_intersection_undefined)
    |a::b->List.fold_left(Private.intersect cmpr)(a)(b);;
  
  let fold_merge cmpr l=
     let rec tempf=(function
        (already_treated,to_be_treated)->match to_be_treated with 
        []->already_treated
        |a::b->tempf(Private.merge cmpr a already_treated,b)
     ) in 
     tempf([],l);;    
  
  let insert cmpr x oy=Private.merge cmpr [x] oy;; 
  
  let intersect = Private.intersect;;
  
  let intersects cmpr ox oy = not(does_not_intersect cmpr ox oy);;
  
  let is_included_in = Private.is_included_in ;;
  
  let length_preserving_sort = Private.length_preserving_sort ;;

  let mem (cmpr:'a Total_ordering_t.t) x ol=
     let rec tempf=(function
      []->false
      |a::others->match cmpr(x)(a) with
         Total_ordering_result_t.Lower->false
         |Total_ordering_result_t.Equal->true
         |Total_ordering_result_t.Greater->tempf others
     )  in
     tempf ol;;   

  let merge = Private.merge;;
  
  let min = Private.min ;; 

  let outsert cmpr x oy=Private.setminus cmpr oy [x];;
  
  let safe_set cmpr ox=if Private.is_increasing(cmpr)(ox) 
                       then ox 
                       else Private.sort cmpr ox;;
  
  let select_minimal_elements_for_inclusion = Private.select_minimal_elements_for_inclusion;;
  
  let setminus = Private.setminus;;
  
  let sort = Private.sort;;

  let symmetric_difference cmpr x y =
    merge cmpr (setminus cmpr x y) (setminus cmpr y x) ;;
  
  
  
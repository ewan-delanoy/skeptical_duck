(*
 
#use"lib/tordered.ml";;

*)

exception Empty_intersection_undefined;;    
exception The_empty_set_has_no_min ;; 
exception The_empty_set_has_no_max ;; 

type 'a ordered_list = Se of 'a list ;;

module Private = struct 

   let to_list (Se l)= l ;;
   
   let unsafe_of_list l = Se l ;;


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
  
  let rec helper_for_max (cmpr:'a Total_ordering_t.t) (current_max,to_be_treated) =
    match to_be_treated with 
     [] -> current_max
    | elt :: others ->
    match cmpr current_max elt with
    Total_ordering_result_t.Greater
   |Total_ordering_result_t.Equal->helper_for_max cmpr (current_max,others)
   |Total_ordering_result_t.Lower->helper_for_max cmpr (elt,others) ;; 


  let max (cmpr:'a Total_ordering_t.t) = function 
       [] -> raise(The_empty_set_has_no_max)
      |elt :: others -> helper_for_max cmpr (elt,others) ;; 


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
      
  let fold_intersect cmpr=function
     []->raise(Empty_intersection_undefined)
    |a::b->List.fold_left(intersect cmpr)(a)(b);;

  let fold_merge cmpr l=
     let rec tempf=(function
        (already_treated,to_be_treated)->match to_be_treated with 
        []->already_treated
        |a::b->tempf(merge cmpr a already_treated,b)
     ) in 
     tempf([],l);;  

  let insert cmpr x oy=merge cmpr [x] oy;; 
  
  let intersects cmpr ox oy = not(does_not_intersect cmpr ox oy);;

  let mem (cmpr:'a Total_ordering_t.t) x ol=
     let rec tempf=(function
      []->false
      |a::others->match cmpr(x)(a) with
         Total_ordering_result_t.Lower->false
         |Total_ordering_result_t.Equal->true
         |Total_ordering_result_t.Greater->tempf others
     )  in
     tempf ol;;   

  let outsert cmpr x oy=setminus cmpr oy [x];;
  
  let safe_set cmpr ox=if is_increasing(cmpr)(ox) 
                       then ox 
                       else sort cmpr ox;;   

  let symmetric_difference cmpr x y =
    merge cmpr (setminus cmpr x y) (setminus cmpr y x) ;;                     

  end;;
  
  module Forget_order_After = struct 

  let sort cmpr x= Private.sort cmpr x;;

  end ;;  
  
  let does_not_intersect cmpr ox oy = 
     Private.does_not_intersect cmpr (Private.to_list ox) (Private.to_list oy);;
  
  let empty_set = Private.unsafe_of_list [] ;;

  
  let filter f ox = Private.unsafe_of_list(List.filter f (Private.to_list ox));;

  let filter_map f ox = List.filter_map f (Private.to_list ox);;

  let fold_intersect cmpr oll = 
     Private.unsafe_of_list(Private.fold_intersect cmpr (Image.image Private.to_list oll));;   
  
  let fold_merge cmpr oll = 
     Private.unsafe_of_list(Private.fold_merge cmpr (Image.image Private.to_list oll));;     
  
  let image f ox = Image.image f (Private.to_list ox) ;;   

  let insert cmpr x oy=Private.unsafe_of_list(Private.insert cmpr x (Private.to_list oy));; 
  
  let intersect cmpr ox oy= 
  Private.unsafe_of_list(Private.intersect cmpr (Private.to_list ox) (Private.to_list oy));;
  
  let intersects cmpr ox oy = 
    Private.intersects cmpr (Private.to_list ox) (Private.to_list oy);;
  let is_included_in cmpr ox oy = 
    Private.is_included_in cmpr (Private.to_list ox) (Private.to_list oy);;

  let length ox = List.length (Private.to_list ox) ;;

  let length_preserving_sort 
     cmpr ox = Private.unsafe_of_list(Private.length_preserving_sort cmpr (Private.to_list ox) );;

  let max cmpr ox = Private.max cmpr (Private.to_list ox);; 
  let mem cmpr x oy= Private.mem cmpr x (Private.to_list oy);;   

  let merge cmpr ox oy= 
  Private.unsafe_of_list(Private.merge cmpr (Private.to_list ox) (Private.to_list oy));;
  
  
  let min cmpr ox = Private.min cmpr (Private.to_list ox);; 

  let outsert cmpr x oy=Private.unsafe_of_list(Private.outsert cmpr x (Private.to_list oy));; 
  
  let safe_set cmpr x=Private.unsafe_of_list(Private.safe_set cmpr x);;
  
  let select_minimal_elements_for_inclusion cmpr oll= 
   let temp = Image.image Private.to_list oll in 
   Image.image Private.unsafe_of_list (Private.select_minimal_elements_for_inclusion cmpr temp);;
  
  let setminus cmpr ox oy= 
  Private.unsafe_of_list(Private.setminus cmpr (Private.to_list ox) (Private.to_list oy));;
  
  let singleton x = Private.unsafe_of_list [x] ;;

  let sort cmpr x= Private.unsafe_of_list(Private.sort cmpr x);;

  let symmetric_difference cmpr ox oy =
    Private.unsafe_of_list(Private.symmetric_difference cmpr (Private.to_list ox) (Private.to_list oy));;

  let tl ox = Private.unsafe_of_list(List.tl (Private.to_list ox));;   

  let to_list = Private.to_list ;; 
  
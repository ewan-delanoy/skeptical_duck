(*
 
#use"erdurod.ml";;

*)

module Private = struct 

let intersect (cmpr:'a Total_ordering.t) ox oy=
    let rec tempf=(function (u,v,accu)->
      if u=[] then (List.rev(accu)) else
      if v=[] then (List.rev(accu)) else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
       Total_ordering.Lower->tempf(yu,v,accu)
      |Total_ordering.Equal->tempf(yu,yv,xu::accu)
      |Total_ordering.Greater->tempf(u,yv,accu)
    ) in
    tempf(ox,oy,[]);;

let is_nondecreasing (cmpr:'a Total_ordering.t) l=
  if List.length(l)<2 then true else
  let rec tempf=(function
  (a,to_be_treated)->match to_be_treated with
   []->true
   |b::others->if (cmpr(a)(b)<>Total_ordering.Greater)
                 then tempf(b,others)
                 else false
  ) in
  tempf(List.hd l,List.tl l);;
  

let merge (cmpr:'a Total_ordering.t) ox oy=
    let rec tempf=(function (u,v,accu)->
      if u=[] then (List.rev_append(accu)(v)) else
      if v=[] then (List.rev_append(accu)(u)) else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
    match cmpr(xu)(xv) with
      Total_ordering.Lower->tempf(yu,v,xu::accu)
    |Total_ordering.Equal->tempf(yu,yv,xu::accu)
    |Total_ordering.Greater->tempf(u,yv,xv::accu)
    ) in
    tempf(ox,oy,[]);;


let setminus (cmpr:'a Total_ordering.t) ox oy=
    let rec tempf=
    (function (u,v,accu)->
      if u=[] then (List.rev(accu)) else
      if v=[] then (List.rev_append(accu)(u)) else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
         Total_ordering.Lower->tempf(yu,v,xu::accu)
        |Total_ordering.Equal->tempf(yu,yv,accu)
        |Total_ordering.Greater->tempf(u,yv,accu)
   ) in
   tempf(ox,oy,[]);;

let rec sort (cmpr:'a Total_ordering.t) x=
  if List.length(x)<2
  then x
  else let temp1=Listennou.split_list_in_half(x) in
       let y1=sort(cmpr)(fst temp1)
       and y2=sort(cmpr)(snd temp1) in
       merge cmpr y1 y2;;


end;;


let diff (cmpr: 'a Total_ordering.t) =
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
                  Total_ordering.Lower->
                    tempf(treated_bc,(a1,b1)::treated_b,treated_c,others1,to_be_treated2)
                  |Total_ordering.Greater->
                  tempf(treated_bc,treated_b,(a2,c2)::treated_c,to_be_treated1,others2)
                  |Total_ordering.Equal->
                  tempf((a1,b1,c2)::treated_bc,treated_b,treated_c,others1,others2)  
                )
              )      
          ) in
          tempf;;   

let does_not_intersect (cmpr:'a Total_ordering.t) ox oy=
    let rec tempf=(function (u,v)->
        if (u=[])||(v=[]) then true else
        let xu=List.hd(u) and yu=List.tl(u) 
        and xv=List.hd(v) and yv=List.tl(v) in
        match cmpr(xu)(xv) with
          Total_ordering.Lower->tempf(yu,v)
        |Total_ordering.Equal->false
        |Total_ordering.Greater->tempf(u,yv)
    ) in
    tempf(ox,oy);;

let fold_intersect cmpr=function
   []->failwith("empty intersection undefined")
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

let is_included_in (cmpr:'a Total_ordering.t) ox oy=
    let rec tempf=(function (u,v)->
      if u=[] then true else
      if v=[] then false else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
        Total_ordering.Lower->false
      |Total_ordering.Equal->tempf(yu,yv)
      |Total_ordering.Greater->tempf(u,yv)
    ) in
    tempf(ox,oy);;


let rec mem (cmpr:'a Total_ordering.t) x ol=
   let rec tempf=(function
    []->false
    |a::others->match cmpr(x)(a) with
       Total_ordering.Lower->false
       |Total_ordering.Equal->true
       |Total_ordering.Greater->tempf others
   )  in
   tempf ol;;    

let merge = Private.merge;;

let outsert cmpr x oy=Private.setminus cmpr oy [x];;

let safe_set cmpr ox=if Private.is_nondecreasing(cmpr)(ox) 
                     then ox 
                     else Private.sort cmpr ox;;

let setminus = Private.setminus;;

let sort = Private.sort;;


(*
type 'a old_set=S of 'a list;;

let unsafe_set x=S(x);;
let forget_order (S x)=x;;

let eq (S x) (S y)=(x=y);;
let neq (S x) (S y)=(x<>y);;

let is_nondecreasing (cmpr:'a Total_ordering.t) l=
  if List.length(l)<2 then true else
  let rec tempf=(function
  (a,to_be_treated)->match to_be_treated with
   []->true
   |b::others->if (cmpr(a)(b)<>Total_ordering.Greater)
                 then tempf(b,others)
                 else false
  ) in
  tempf(List.hd l,List.tl l);;
  
let is_increasing (cmpr:'a Total_ordering.t) l=
  if List.length(l)<2 then true else
  let rec tempf=(function
  (a,to_be_treated)->match to_be_treated with
   []->true
   |b::others->if (cmpr(a)(b)=Total_ordering.Lower)
                 then tempf(b,others)
                 else false
  ) in
  tempf(List.hd l,List.tl l);;

let rec mem (cmpr:'a Total_ordering.t) x ol=
   let rec mem0=(function
    []->false
    |a::others->match cmpr(x)(a) with
       Total_ordering.Lower->false
       |Total_ordering.Equal->true
       |Total_ordering.Greater->mem0 others
   )  in
   mem0 (forget_order ol);;
		
            
let rec sort (cmpr:'a Total_ordering.t) x=
  if List.length(x)<2
  then unsafe_set(x)
  else let temp1=Listennou.split_list_in_half(x) in
       let y1=sort(cmpr)(fst temp1)
       and y2=sort(cmpr)(snd temp1) in
       merge cmpr y1 y2;;
  
  
let setminus (cmpr:'a Total_ordering.t) ox oy=
    let rec tempf=
    (function (u,v,accu)->
      if u=[] then (List.rev(accu)) else
      if v=[] then (List.rev_append(accu)(u)) else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
         Total_ordering.Lower->tempf(yu,v,xu::accu)
        |Total_ordering.Equal->tempf(yu,yv,accu)
        |Total_ordering.Greater->tempf(u,yv,accu)
   ) in
   unsafe_set(tempf(forget_order ox,forget_order oy,[]));;

let intersect (cmpr:'a Total_ordering.t) ox oy=
    let rec tempf=(function (u,v,accu)->
      if u=[] then (List.rev(accu)) else
      if v=[] then (List.rev(accu)) else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
       Total_ordering.Lower->tempf(yu,v,accu)
      |Total_ordering.Equal->tempf(yu,yv,xu::accu)
      |Total_ordering.Greater->tempf(u,yv,accu)
    ) in
    unsafe_set(tempf(forget_order ox,forget_order oy,[]));;



let is_included_in (cmpr:'a Total_ordering.t) ox oy=
    let rec tempf=(function (u,v)->
      if u=[] then true else
      if v=[] then false else
      let xu=List.hd(u) and yu=List.tl(u) 
      and xv=List.hd(v) and yv=List.tl(v) in
      match cmpr(xu)(xv) with
        Total_ordering.Lower->false
      |Total_ordering.Equal->tempf(yu,yv)
      |Total_ordering.Greater->tempf(u,yv)
    ) in
    tempf(forget_order ox,forget_order oy);;

let min=((fun cmpr x->match x with
  []->failwith("The empty set has no min")
  |a::b->
    let rec tempf=(fun 
     (candidate,to_be_treated)->match to_be_treated with
      []->candidate
      |c::others->
        if cmpr(c)(candidate)=Total_ordering.Lower
        then tempf(c,others)
        else tempf(candidate,others)
    ) in
    tempf(a,b)):> ('a Total_ordering.t -> 'a list->'a));;

let max=((fun cmpr x->match x with
  []->failwith("The empty set has no max")
  |a::b->
    let rec tempf=(fun 
     (candidate,to_be_treated)->match to_be_treated with
      []->candidate
      |c::others->
        if cmpr(c)(candidate)=Total_ordering.Greater
        then tempf(c,others)
        else tempf(candidate,others)
    ) in
    tempf(a,b)):> ('a Total_ordering.t -> 'a list->'a));;

let cooperation_for_two cmpr x y=
   (intersect cmpr x y,setminus cmpr x y,setminus cmpr y x);;
  
let expand_boolean_algebra cmpr l=
  if List.length(l)<2 then l else
  let rec tempf=(fun 
    (graet,y0,between,to_be_treated)->
      if between=[] 
      then if to_be_treated=[]
           then y0::graet
           else let z0=List.hd(to_be_treated) and others=List.tl(to_be_treated) in
                tempf([],z0,y0::graet,others)
      else 
      let x0=List.hd(between) and others_between=List.tl(between) in
      let t1=intersect cmpr x0 y0 and t2=setminus cmpr x0 y0 in
      let y1=setminus cmpr y0 t1 in
      let temp1=List.filter (fun ox->forget_order ox<>[]) [t1;t2] in
      tempf(List.rev_append temp1 graet,y1,others_between,to_be_treated)
  )  in
  let x1=List.hd(l) and r1=List.tl(l) in
  tempf([],x1,[],r1);;

let length ox=List.length(forget_order ox);;
let image f ox=Image.image(f)(forget_order ox);;
let insert cmpr x oy=merge cmpr (unsafe_set [x])(oy);;
let exists f ox=List.exists f (forget_order ox);;
let safe_set cmpr ox=if is_nondecreasing(cmpr)(ox) 
                       then unsafe_set ox 
                       else sort cmpr ox;;
let rev_map f ox=List.rev_map(f)(forget_order ox);;
let filter f ox=unsafe_set(List.filter(f)(forget_order ox));;
let for_all f ox=List.for_all(f)(forget_order ox);;
let singleton x=unsafe_set [x];;
let empty_set=unsafe_set [];;


let nmem cmpr a ox=not(mem cmpr a ox);;
let eq ox oy=(forget_order ox)=(forget_order oy);;
  
let sort_silently cmpr x=
  forget_order(sort cmpr  x);;
let mem_silently cmpr e x=
    mem cmpr e (unsafe_set  x);;  
let setminus_silently cmpr x y=
      forget_order(setminus cmpr  (unsafe_set x) (unsafe_set y) );;
let insert_silently cmpr x l=
        forget_order(insert cmpr x (unsafe_set l));;     
let diff_silently (cmpr: 'a Total_ordering.t) =
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
                  Total_ordering.Lower->
                    tempf(treated_bc,(a1,b1)::treated_b,treated_c,others1,to_be_treated2)
                  |Total_ordering.Greater->
                  tempf(treated_bc,treated_b,(a2,c2)::treated_c,to_be_treated1,others2)
                  |Total_ordering.Equal->
                  tempf((a1,b1,c2)::treated_bc,treated_b,treated_c,others1,others2)  
                )
              )      
          ) in
          tempf;;                   
*)          
(*
    hardcore version :
    
type 'a old_set=S of 'a list;;
let unsafe_set x=S(x);;
let forget_order (S x)=x;;


*)
(*
    soft version :
    
type 'a old_set='a list;;
let unsafe_set=((fun x->x):>('a list->'a old_set));;
let forget_order=((fun x->x):>('a old_set->'a list));;


*)

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
		
            
let merge (cmpr:'a Total_ordering.t) ox oy=
let rec merge0=
(function (u,v,accu)->
if u=[] then (List.rev_append(accu)(v)) else
if v=[] then (List.rev_append(accu)(u)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match cmpr(xu)(xv) with
   Total_ordering.Lower->merge0(yu,v,xu::accu)
   |Total_ordering.Equal->merge0(yu,yv,xu::accu)
   |Total_ordering.Greater->merge0(u,yv,xv::accu)

) in
unsafe_set(merge0(forget_order ox,forget_order oy,[]));;

let rec sort (cmpr:'a Total_ordering.t) x=
  if List.length(x)<2
  then unsafe_set(x)
  else let temp1=Listennou.split_list_in_half(x) in
       let y1=sort(cmpr)(fst temp1)
       and y2=sort(cmpr)(snd temp1) in
       merge cmpr y1 y2;;
  
  
let lemel (cmpr:'a Total_ordering.t) ox oy=
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

let kengeij (cmpr:'a Total_ordering.t) ox oy=
let rec kengeij0=
(function (u,v,accu)->
if u=[] then (List.rev(accu)) else
if v=[] then (List.rev(accu)) else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match cmpr(xu)(xv) with
   Total_ordering.Lower->kengeij0(yu,v,accu)
   |Total_ordering.Equal->kengeij0(yu,yv,xu::accu)
   |Total_ordering.Greater->kengeij0(u,yv,accu)

) in
unsafe_set(kengeij0(forget_order ox,forget_order oy,[]));;

let kengeij_goullo (cmpr:'a Total_ordering.t) ox oy=
let rec kengeij_goullo0=
(function (u,v)->
if (u=[])||(v=[]) then true else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match cmpr(xu)(xv) with
   Total_ordering.Lower->kengeij_goullo0(yu,v)
   |Total_ordering.Equal->false
   |Total_ordering.Greater->kengeij_goullo0(u,yv)
) in
kengeij_goullo0(forget_order ox,forget_order oy);;


let ental (cmpr:'a Total_ordering.t) ox oy=
let rec ental0=
(function (u,v)->
if u=[] then true else
if v=[] then false else
let xu=List.hd(u) and yu=List.tl(u) 
and xv=List.hd(v) and yv=List.tl(v) in
match cmpr(xu)(xv) with
   Total_ordering.Lower->false
   |Total_ordering.Equal->ental0(yu,yv)
   |Total_ordering.Greater->ental0(u,yv)
) in
ental0(forget_order ox,forget_order oy);;

let min=((fun cmpr x->match x with
  []->failwith("The empty set has no min")
  |a::b->
    let rec tempf=(fun 
     (trecher,to_be_treated)->match to_be_treated with
      []->trecher
      |c::others->
        if cmpr(c)(trecher)=Total_ordering.Lower
        then tempf(c,others)
        else tempf(trecher,others)
    ) in
    tempf(a,b)):> ('a Total_ordering.t -> 'a list->'a));;

let max=((fun cmpr x->match x with
  []->failwith("The empty set has no max")
  |a::b->
    let rec tempf=(fun 
     (trecher,to_be_treated)->match to_be_treated with
      []->trecher
      |c::others->
        if cmpr(c)(trecher)=Total_ordering.Greater
        then tempf(c,others)
        else tempf(trecher,others)
    ) in
    tempf(a,b)):> ('a Total_ordering.t -> 'a list->'a));;

let cooperation_for_two cmpr x y=
   (kengeij cmpr x y,lemel cmpr x y,lemel cmpr y x);;
  
let expand_boolean_algebra cmpr l=
  if List.length(l)<2 then l else
  let rec tempf=(fun 
    (graet,y0,etre,to_be_treated)->
      if etre=[] 
      then if to_be_treated=[]
           then y0::graet
           else let z0=List.hd(to_be_treated) and others=List.tl(to_be_treated) in
                tempf([],z0,y0::graet,others)
      else 
      let x0=List.hd(etre) and others_etre=List.tl(etre) in
      let t1=kengeij cmpr x0 y0 and t2=lemel cmpr x0 y0 in
      let y1=lemel cmpr y0 t1 in
      let temp1=List.filter (fun ox->forget_order ox<>[]) [t1;t2] in
      tempf(List.rev_append temp1 graet,y1,others_etre,to_be_treated)
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
let big_teuzin cmpr l=
   let rec tempf=(function
      (already_treated,to_be_treated)->match to_be_treated with 
      []->already_treated
      |a::b->tempf(merge cmpr a already_treated,b)
   ) in 
   tempf(empty_set,l);;
let big_kengeij cmpr=function
   []->failwith("empty intersection undefined")
  |a::b->List.fold_left(kengeij cmpr)(a)(b);;
let nelfenn cmpr a ox=not(mem cmpr a ox);;
let nental cmpr a ox=not(ental cmpr a ox);;
let eq ox oy=(forget_order ox)=(forget_order oy);;
  
let sort_silently cmpr x=
  forget_order(sort cmpr  x);;
let mem_silently cmpr e x=
    mem cmpr e (unsafe_set  x);;  
let kengeij_plaen cmpr x y=
    forget_order(kengeij cmpr  (unsafe_set x) (unsafe_set y) );;
let setminus_silently cmpr x y=
      forget_order(lemel cmpr  (unsafe_set x) (unsafe_set y) );;
let insert_plaen cmpr x l=
        forget_order(insert cmpr x (unsafe_set l));;     
let diff_plaen (cmpr: 'a Total_ordering.t) =
          let rec tempf=(fun
            (graet_bc,graet_b,graet_c,to_be_treated1,to_be_treated2)->
              match to_be_treated1 with
              []->(graet_bc,graet_b,List.rev_append graet_c to_be_treated2)
              |(a1,b1)::others1->
              (
                match to_be_treated2 with
              []->(graet_bc,List.rev_append graet_b to_be_treated1,graet_c)     
              |(a2,c2)::others2->
                (
                  match cmpr a1 a2 with
                  Total_ordering.Lower->
                    tempf(graet_bc,(a1,b1)::graet_b,graet_c,others1,to_be_treated2)
                  |Total_ordering.Greater->
                  tempf(graet_bc,graet_b,(a2,c2)::graet_c,to_be_treated1,others2)
                  |Total_ordering.Equal->
                  tempf((a1,b1,c2)::graet_bc,graet_b,graet_c,others1,others2)  
                )
              )      
          ) in
          tempf;;                   
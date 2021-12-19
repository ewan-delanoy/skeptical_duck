(*

#use"Ordered_Lists/total_ordering.ml";;

*)


module Private = struct
let leq (computer:'a Total_ordering_t.t) x y=
   let v=computer(x)(y) in
   (v=Total_ordering_result_t.Lower)||(v=Total_ordering_result_t.Equal);;
   
 let lt (computer:'a Total_ordering_t.t) x y=(computer(x)(y)=Total_ordering_result_t.Lower);;   
 
 let geq (computer:'a Total_ordering_t.t) x y=
   let v=computer(x)(y) in
   (v=Total_ordering_result_t.Lower)||(v=Total_ordering_result_t.Equal);;
   
 let gt (computer:'a Total_ordering_t.t) x y=(computer(x)(y)=Total_ordering_result_t.Greater);;   
 
 let from_lt f=
   let temp1=(fun x y->
     if f(x)(y)
     then Total_ordering_result_t.Lower
     else if f(y)(x)
          then Total_ordering_result_t.Greater
          else Total_ordering_result_t.Equal
   ) in
   (temp1:'a Total_ordering_t.t);;
 
 let standard_completion f g=
  let answer=(fun x y->
   if f(y)(x)
   then Total_ordering_result_t.Greater
   else if f(x)(y)
        then Total_ordering_result_t.Lower
        else if g(x)(y)
             then Total_ordering_result_t.Equal
             else if x<y
                  then Total_ordering_result_t.Lower
                  else Total_ordering_result_t.Greater
  ) in
  (answer: 'a Total_ordering_t.t);;
 
 let standard=((fun x y->
    if x=y
    then Total_ordering_result_t.Equal
    else if x<y
         then Total_ordering_result_t.Lower
         else Total_ordering_result_t.Greater
 ): 'a Total_ordering_t.t);;
 
let standard2=((fun (x1,y1) (x2,y2)->
    let t1=standard x1 x2 in 
    if t1<> Total_ordering_result_t.Equal 
    then t1
    else standard y1 y2
 ): ('a * 'b) Total_ordering_t.t);;

 let completion f (g:'a Total_ordering_t.t)=
  let answer=(fun x y->
   if f(y)(x)
   then Total_ordering_result_t.Greater
   else if f(x)(y)
        then Total_ordering_result_t.Lower
         else g(x)(y)
  ) in
  (answer: 'a Total_ordering_t.t);;
 
let combine=((fun ~tried_first ~tried_second->
  (fun x y->
   let first_trial = tried_first x y in 
   if first_trial <> Total_ordering_result_t.Equal 
   then first_trial
   else tried_second x y
  ) ): 
    tried_first:('a Total_ordering_t.t) -> tried_second:('a Total_ordering_t.t) -> ('a Total_ordering_t.t)
  );;

 let product (f:'a Total_ordering_t.t) (g:'b Total_ordering_t.t)=
  ((fun (x1,y1) (x2,y2)->
     let t=f(x1)(x2) in
     if t<>Total_ordering_result_t.Equal 
     then t
     else g y1 y2
 ): ('a*'b) Total_ordering_t.t);;
 
 let triple_product (f:'a Total_ordering_t.t) (g:'b Total_ordering_t.t) (h:'c Total_ordering_t.t)=
  ((fun (x1,y1,z1) (x2,y2,z2)->
     let tx=f(x1)(x2) in
     if tx<>Total_ordering_result_t.Equal 
     then tx
     else let ty=g(y1)(y2) in
          if ty<>Total_ordering_result_t.Equal 
          then ty
          else h z1 z2
 ): ('a*'b*'c) Total_ordering_t.t);;
 
 let rec lex_compare (f:'a Total_ordering_t.t)=
  let rec tempf=(
    fun l1 l2->
     match l1 with 
     []->(if l2=[] then Total_ordering_result_t.Equal else Total_ordering_result_t.Lower)
     |a1::b1->
      (
        match l2 with 
        []->Total_ordering_result_t.Greater
        |a2::b2->
          let t=f(a1)(a2) in
           if t<>Total_ordering_result_t.Equal then t else
           tempf b1 b2
      )) in
     (tempf:>( ('a list) Total_ordering_t.t));;
 


let silex_compare (f:'a Total_ordering_t.t)=
  let tempf=(
    fun l1 l2->
     let t=standard(List.length l1)(List.length l2) in
     if t<>Total_ordering_result_t.Equal then t else
     lex_compare f l1 l2
  ) in
   (tempf:>( ('a list) Total_ordering_t.t));;
 

let from_list (l:'a list)=
  let tempc=(fun x y->
  let rec tempf=(function
   []->(x<y)
   |u::peurrest->if u=x then List.mem(y)(peurrest)
                 else if u=y then false
                 else tempf(peurrest)
  ) in
  tempf l) in
  from_lt tempc;;

let min (f:'a Total_ordering_t.t)=function
 []->failwith("Min of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Total_ordering_result_t.Lower
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;

let max (f:'a Total_ordering_t.t)=function
 []->failwith("Max of the empty set is undefined")
 |a::b->
   let rec tempf=(fun
    (candidate,l)->match l with
      []->candidate
      |c::peurrest->if f(c)(candidate)=Total_ordering_result_t.Greater
                    then tempf(c,peurrest)
                    else tempf(candidate,peurrest)
   ) in
   tempf(a,b);;
   
let minimize_it_with_care (cf:'a Total_ordering_t.t) 
   f=function
[]->failwith("careful min on empty set undefined")
|x::y->
 let rec minimize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Total_ordering_result_t.Lower
				then minimize_it_with_care0([a],va,peurrest)
				else if howl=Total_ordering_result_t.Equal
				     then minimize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else minimize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 minimize_it_with_care0([x],f(x),y);;


let maximize_it_with_care (cf:'a Total_ordering_t.t) 
   f=function
[]->failwith("careful max on empty set undefined")
|x::y->
 let rec maximize_it_with_care0=(function
  (current_candidates,current_value,da_ober)->match da_ober with
  []->(current_value,List.rev(current_candidates))
  |a::peurrest->let va=f(a) in
                let howl=cf(va)(current_value) in
                if howl=Total_ordering_result_t.Greater
				then maximize_it_with_care0([a],va,peurrest)
				else if howl=Total_ordering_result_t.Equal
				     then maximize_it_with_care0(a::current_candidates,current_value,peurrest)
					 else maximize_it_with_care0(current_candidates,current_value,peurrest)
 ) 
in
 maximize_it_with_care0([x],f(x),y);;

let modify_locally (f:'a Total_ordering_t.t) l=
  let big_m=max(f)(l) in
  let tempf=(fun x y->
    if List.mem(x)(l)
    then if List.mem(y)(l)
         then if x=y
              then Total_ordering_result_t.Equal
              else (from_list l x y)
         else f big_m y
    else if List.mem(y)(l)
         then f x big_m
         else f x y
  
  ) in
  (tempf:>( 'a Total_ordering_t.t));;

let list_for_dictionary_order=
  [97; 65; 98; 66; 99; 67; 100; 68; 101; 69; 102; 70; 103; 71; 104; 72; 105;
  73; 106; 74; 107; 75; 108; 76; 109; 77; 110; 78; 111; 79; 112; 80; 113; 81;
  114; 82; 115; 83; 116; 84; 117; 85; 118; 86; 119; 87; 120; 88; 121; 89;
  122; 90; 91; 92; 93; 94; 95; 96];;  

let reindexer_for_dictionary_order i=
    if (i<65)||(i>122) 
    then i 
    else 64+(Listennou.find_index i list_for_dictionary_order);;


let for_characters=let tempf=(fun x y->
  standard 
        (reindexer_for_dictionary_order(int_of_char x))
        (reindexer_for_dictionary_order(int_of_char y))
  ) in (tempf:>char Total_ordering_t.t);;

let for_integers=let tempf=(fun (x:int) (y:int)-> standard x y 
    ) in (tempf:>int Total_ordering_t.t);;  

let lex_for_strings=
    ((fun s1 s2->
      let m1=String.length s1
      and m2=String.length s2
      in
      let m=Stdlib.min(m1)(m2) in
      match Option.seek (fun j->(String.get s1 j)<>(String.get s2 j)) (Ennig.ennig 0 (m-1)) with
      None->standard m1 m2
      |Some(j)->for_characters (String.get s1 j) (String.get s2 j) 
    ) : string Total_ordering_t.t);;

let silex_for_strings=
      ((fun s1 s2->
        let m1=String.length s1
        and m2=String.length s2
        in
        let first_try=standard(m1)(m2) in
        if first_try<>Total_ordering_result_t.Equal
        then first_try
        else lex_for_strings s1 s2
      ) : string Total_ordering_t.t);;    

let lex_for_string_lists=
  ((fun l1 l2->
      let (_,left_part,right_part)=Listennou.factor (l1,l2) in
      if left_part=[] 
      then (if right_part=[] 
           then Total_ordering_result_t.Equal 
           else Total_ordering_result_t.Lower)
      else if right_part=[] 
           then Total_ordering_result_t.Greater 
           else lex_for_strings (List.hd left_part) (List.hd right_part)  
  ) : (string list) Total_ordering_t.t);;

let for_longest_match=  
    ((fun s1 s2->
      let m1=String.length s1
      and m2=String.length s2 in
      if (
          if m1>m2 then false else
          (String.sub s2 0 m1)=s1
      ) then Total_ordering_result_t.Greater else
      if (
          if m2>m1 then false else
          (String.sub s1 0 m2)=s2
      ) then Total_ordering_result_t.Lower else
      lex_for_strings s1 s2
     ): string Total_ordering_t.t);;


let for_longest_match_pairs=  
((fun (s1,v1) (s2,v2)->
  let first_try=silex_for_strings(s2)(s1) in
  if first_try<>Total_ordering_result_t.Equal 
  then first_try
  else standard v1 v2
 ): (string*'b) Total_ordering_t.t);;
 
let from_snd (f:'b Total_ordering_t.t)=((fun (x1,y1) (x2,y2)->
  let first_try=f y1 y2 in
  if first_try<>Total_ordering_result_t.Equal 
  then first_try
  else standard x1 x2
): ('a*'b) Total_ordering_t.t );;

let cardinality_then_diameter =((fun l1 l2->
  let first_try=standard (List.length l1) (List.length l2) in
  if first_try<>Total_ordering_result_t.Equal 
  then first_try
  else 
  let diam1 = List.hd(List.rev l1) - (List.hd l1)
  and diam2 = List.hd(List.rev l2) - (List.hd l2) in 
  let second_try=standard diam1 diam2 in
  if second_try<>Total_ordering_result_t.Equal 
  then second_try
  else lex_compare for_integers l1 l2
): (int list) Total_ordering_t.t );;

end ;;

let cardinality_then_diameter = Private.cardinality_then_diameter ;;
let for_integers = Private.for_integers ;;
let lex_compare = Private.lex_compare ;;
let lex_for_strings = Private.lex_for_strings ;;
let product = Private.product ;;
let silex_compare = Private.silex_compare ;;
let silex_for_strings = Private.silex_for_strings ;;
let silex_for_intlists = Private.silex_compare for_integers ;;
let standard = Private.standard ;;
let standard2 = Private.standard2 ;;

 
           
(*

#use"Text_editing/french_capitalization.ml";;

*)



let optional_for_one s=
  match Option.seek (fun (a,_)->Supstring.begins_with s a) French_data.paired_data with 
  None->None
  |Some(a0,b0)->Some(b0^(Cull_string.cobeginning (String.length a0) s));;

let optional_for_two (s1,s2)=
   let os1=optional_for_one s1
   and os2=optional_for_one s2 in 
   if List.mem None [os1;os2]
   then None 
   else Some(Option.unpack os1,Option.unpack os2);;

let optional_for_false_three (s1,s2,s3)=
   let os1=optional_for_one s1
   and os3=optional_for_one s3 in 
   if List.mem None [os1;os3]
   then None 
   else Some(Option.unpack os1,s2,Option.unpack os3);;

let generalize_for_one l=l@(Option.filter_and_unpack optional_for_one l);;
let generalize_for_two l=l@(Option.filter_and_unpack optional_for_two l);;
let generalize_for_false_three l=
   Image.image(
      fun (x,y,li)->
         let new_li=li@(Option.filter_and_unpack optional_for_false_three li) in 
         (x,y,new_li)
   ) l;; 
  


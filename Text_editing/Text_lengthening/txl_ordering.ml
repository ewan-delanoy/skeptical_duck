(*

#use "Text_editing/Text_lengthening/ordering_for_text_lengthener.ml";;

*)

module Private=struct

let s = French_order.cmp;;

let for_adjustments=Total_ordering.triple_product s s s;;
let for_decompressions l=
   let temp1=Image.image (fun (x,y,z)->
    (x,y,Ordered.sort_silently for_adjustments z) ) l in 
   let t = Total_ordering.triple_product s s Total_ordering.standard in 
   Ordered.sort_silently t temp1;;


let for_expansions=
   let flatten=String.concat "" in 
   let pre1 = (fun x y ->Total_ordering.standard (String.length y) (String.length x)) in 
   let pre2 = Total_ordering.combine ~tried_first:pre1 ~tried_second:s in 
   (fun x y ->pre2 (flatten x) (flatten y));;
let for_inert_words=s;;
let for_abbreviations=Total_ordering.product s s;;

end;;

let order_abbreviations = Ordered.sort_silently Private.for_abbreviations;;
let order_adjustments = Ordered.sort_silently Private.for_adjustments;;
let order_decompressions = Private.for_decompressions;;
let order_expansions = Ordered.sort_silently Private.for_expansions;;
let order_inert_words = Ordered.sort_silently Private.for_inert_words;;


let insert_abbreviation = Ordered.insert_silently Private.for_abbreviations;;
let insert_adjustment = Ordered.insert_silently Private.for_adjustments;;
let insert_expansion = Ordered.insert_silently Private.for_expansions;;
let insert_inert_word = Ordered.insert_silently Private.for_inert_words;;


(*

#use "Text_editing/Text_lengthening/txl_ordering.ml";;

*)

module Private=struct

let s = French_order.cmp;;

let for_adjustments=Total_ordering.triple_product s s s;;
let for_decompressions l=
   let temp1=Image.image (fun (x,y,z)->
    (x,y,Erdurod.sort for_adjustments z) ) l in 
   let t = Total_ordering.triple_product s s Total_ordering.standard in 
   Erdurod.sort t temp1;;


let for_expansions=
   let flatten=String.concat "" in 
   let pre1 = (fun x y ->Total_ordering.standard (String.length y) (String.length x)) in 
   let pre2 = Total_ordering.combine ~tried_first:pre1 ~tried_second:s in 
   (fun x y ->pre2 (flatten x) (flatten y));;
let for_inert_words=s;;
let for_abbreviations=Total_ordering.product s s;;

end;;

let order_abbreviations = Erdurod.sort Private.for_abbreviations;;
let order_adjustments = Erdurod.sort Private.for_adjustments;;
let order_decompressions = Private.for_decompressions;;
let order_expansions = Erdurod.sort Private.for_expansions;;
let order_inert_words = Erdurod.sort Private.for_inert_words;;


let insert_abbreviation = Erdurod.insert Private.for_abbreviations;;
let insert_adjustment = Erdurod.insert Private.for_adjustments;;
let insert_expansion = Erdurod.insert Private.for_expansions;;
let insert_inert_word = Erdurod.insert Private.for_inert_words;;


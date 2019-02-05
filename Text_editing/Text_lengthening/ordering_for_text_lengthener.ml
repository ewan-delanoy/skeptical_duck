(*

#use "Text_editing/Text_lengthening/ordering_for_text_lengthener.ml";;

*)

module Private=struct

let s = French_order.cmp;;

let for_adjustments=Total_ordering.triple_product s s s;;
let for_decompressions l=
   let temp1=Image.image (fun (x,y,z)->
    (x,y,Ordered.diforchan_plaen for_adjustments z) ) l in 
   let t = Total_ordering.triple_product s s Total_ordering.standard in 
   Ordered.diforchan_plaen t temp1;;


let for_expansions=Total_ordering.lex_for_string_lists;;
let for_inert_words=s;;
let for_abbreviations=Total_ordering.product s s;;

end;;

let order_abbreviations = Ordered.diforchan_plaen Private.for_abbreviations;;
let order_adjustments = Ordered.diforchan_plaen Private.for_adjustments;;
let order_decompressions = Private.for_decompressions;;
let order_expansions = Ordered.diforchan_plaen Private.for_expansions;;
let order_inert_words = Ordered.diforchan_plaen Private.for_inert_words;;


let insert_abbreviation = Ordered.insert_plaen Private.for_abbreviations;;
let insert_adjustment = Ordered.insert_plaen Private.for_adjustments;;
let insert_expansion = Ordered.insert_plaen Private.for_expansions;;
let insert_inert_word = Ordered.insert_plaen Private.for_inert_words;;


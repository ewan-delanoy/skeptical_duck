(*

#use "Text_editing/Text_lengthening/ordering_for_text_lengthener.ml";;

*)

module Private=struct

let for_adjustments=
   let t = Total_ordering.lex_for_strings in
   Total_ordering.triple_product t t t;;

let for_expansions=Total_ordering.lex_for_string_lists;;
let for_inert_words=Total_ordering.lex_for_strings;;
let for_left_core_abbreviations=Total_ordering.product Total_ordering.lex_for_strings Total_ordering.lex_for_strings;;
let for_prefix_abbreviations=Total_ordering.product Total_ordering.lex_for_strings Total_ordering.lex_for_strings;;

end;;
(*
let order_decompressions = Ordered.diforchan_plaen Private.for_decompressions;;
let order_expansions = Ordered.diforchan_plaen Private.for_expansions;;
let order_inert_words = Ordered.diforchan_plaen Private.for_inert_words;;
let order_left_core_abbreviations = Ordered.diforchan_plaen Private.for_left_core_abbreviations;;
let order_prefix_abbreviations = Ordered.diforchan_plaen Private.for_left_core_abbreviations;;
*)

let insert_adjustment = Ordered.insert_plaen Private.for_adjustments;;
let insert_expansion = Ordered.insert_plaen Private.for_expansions;;
let insert_inert_word = Ordered.insert_plaen Private.for_inert_words;;
let insert_left_core_abbreviation = Ordered.insert_plaen Private.for_left_core_abbreviations;;
let insert_prefix_abbreviation = Ordered.insert_plaen Private.for_left_core_abbreviations;;
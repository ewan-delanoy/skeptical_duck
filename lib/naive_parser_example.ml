(*

#use"lib/naive_parser_example.ml";;

*)

let fixed_string substr =
   Naive_parser_t.NP(
    fun text idx ->
      if Substring.is_a_substring_located_at substr text idx 
      then Some((),idx+(String.length substr)) 
      else None 
   ) ;;
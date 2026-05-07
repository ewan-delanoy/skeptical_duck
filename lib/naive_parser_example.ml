(*

#use"lib/naive_parser_example.ml";;

*)


let digit =
   Naive_parser_t.NP(
    fun text idx ->
      if idx > (String.length text)
      then None 
      else let c = String.get text (idx-1) in 
           let i = int_of_char c in 
           if (48<=i) &&(i<=57)
           then Some(c,idx+1)
           else None
   ) ;;

let fixed_string substr =
   Naive_parser_t.NP(
    fun text idx ->
      if Substring.is_a_substring_located_at substr text idx 
      then Some((),idx+(String.length substr)) 
      else None 
   ) ;;
(*

#use"phpbb_text_with_quotes_t.ml";;


*)

type t = 
 Atom of string  
|Concatenated of t list 
|Quoted of string * t ;;

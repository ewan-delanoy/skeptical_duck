(*

#use"phpbb_text_with_quotes_t.ml";;


*)

type t = Text of element list 
and element = Atom of string 
            |Quote of (string option) * t ;;

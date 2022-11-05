(* 

#use"Simplified_yacc/sy_token_info_t.ml";;

*)


type t = {
      str : string;
      charpos : int;
      line : int;
      column : int;
      file : string;
} ;;

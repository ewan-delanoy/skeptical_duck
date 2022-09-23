(*

#use"Padioleau/yp_token_info.ml";;

*)


let tokinfo_str_pos str pos =
    {
    Yp_token_info_t.charpos = pos;
    str     = str;

    (* info filled in a post-lexing phase, see complete_token_location_large*)
    line = -1;
    column = -1;
    file = "NO FILE INFO YET";
  } ;; 

let tokinfo lexbuf  =
  tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf) ;;
           


exception NoTokenLocation of string ;;

let str_of_info x = x.Yp_token_info_t.str ;;

let token_location_of_info pinfo = Ok pinfo ;;

let unsafe_token_location_of_info ii =
    match token_location_of_info ii with
    | Ok pinfo -> pinfo
    | Error msg -> raise (NoTokenLocation msg) ;;

let pos_of_info  ii = (unsafe_token_location_of_info ii).Yp_token_info_t.charpos ;;  
let charpos ii = ii.Yp_token_info_t.charpos ;; 
(*

#use"Padioleau/yp_parse_info.ml";;

*)

type token_location = {
  str: string;
  charpos: int;
  line: int; column: int;
  file: string;
} ;;

type token_origin =
  (* Present both in the AST and list of tokens *)
  | OriginTok  of token_location

  (* Present only in the AST and generated after parsing. Can be used
   * when building some extra AST elements. *)
  | FakeTokStr of string (* to help the generic pretty printer *) *
                  (* Sometimes we generate fake tokens close to existing
                   * origin tokens. This can be useful when have to give an error
                   * message that involves a fakeToken. The int is a kind of
                   * virtual position, an offset. See compare_pos below.
                  *)
                  (token_location * int) option

  (* In the case of a XHP file, we could preprocess it and incorporate
   * the tokens of the preprocessed code with the tokens from
   * the original file. We want to mark those "expanded" tokens
   * with a special tag so that if someone do some transformation on
   * those expanded tokens they will get a warning (because we may have
   * trouble back-propagating the transformation back to the original file).
  *)
  | ExpandedTok of
      (* refers to the preprocessed file, e.g. /tmp/pp-xxxx.pphp *)
      token_location  *
      (* kind of virtual position. This info refers to the last token
       * before a serie of expanded tokens and the int is an offset.
       * The goal is to be able to compare the position of tokens
       * between then, even for expanded tokens. See compare_pos
       * below.
      *)
      token_location * int

  (* The Ab constructor is (ab)used to call '=' to compare
   * big AST portions. Indeed as we keep the token information in the AST,
   * if we have an expression in the code like "1+1" and want to test if
   * it's equal to another code like "1+1" located elsewhere, then
   * the Pervasives.'=' of OCaml will not return true because
   * when it recursively goes down to compare the leaf of the AST, that is
   * the token_location, there will be some differences of positions. If instead
   * all leaves use Ab, then there is no position information and we can
   * use '='. See also the 'al_info' function below.
   *
   * Ab means AbstractLineTok. I Use a short name to not
   * polluate in debug mode.
  *)
  | Ab ;;



type t = token_origin ;;

let mk_info_of_loc loc = OriginTok loc ;; 

let tokinfo_str_pos str pos =
   let loc = {
    charpos = pos;
    str     = str;

    (* info filled in a post-lexing phase, see complete_token_location_large*)
    line = -1;
    column = -1;
    file = "NO FILE INFO YET";
  } in 
  mk_info_of_loc loc;; 

let tokinfo lexbuf  =
  tokinfo_str_pos (Lexing.lexeme lexbuf) (Lexing.lexeme_start lexbuf) ;;
           


exception NoTokenLocation of string ;;

let str_of_info  ii =
    match ii  with
    | OriginTok x -> x.str
    | FakeTokStr (s, _) -> s
    | ExpandedTok _ | Ab ->
        raise (NoTokenLocation "str_of_info: Expanded or Ab") ;;


let token_location_of_info ii =
    match ii with
    | OriginTok pinfo -> Ok pinfo
    (* TODO ? dangerous ? *)
    | ExpandedTok (pinfo_pp, _pinfo_orig, _offset) -> Ok pinfo_pp
    | FakeTokStr (_, (Some (pi, _))) -> Ok pi
  
    | FakeTokStr (_, None) -> Error "FakeTokStr"
    | Ab -> Error "Ab" ;;

let unsafe_token_location_of_info ii =
    match token_location_of_info ii with
    | Ok pinfo -> pinfo
    | Error msg -> raise (NoTokenLocation msg) ;;

let pos_of_info  ii = (unsafe_token_location_of_info ii).charpos ;;  
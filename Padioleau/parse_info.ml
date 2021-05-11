(*

#use"Padioleau/parse_info.ml";;

*)
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
(* open Common *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Some helpers for the different lexers and parsers in pfff.
 * The main types are:
 * ('token_location' < 'token_origin' < 'token_mutable') * token_kind
 * 
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Currently core/lexing.ml does not handle the line number position.
 * Even if there are certain fields in the lexing structure, they are not
 * maintained by the lexing engine so the following code does not work:
 *
 *   let pos = Lexing.lexeme_end_p lexbuf in
 *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
 *      (pos.pos_cnum - pos.pos_bol) in
 *
 * Hence those types and functions below to overcome the previous limitation,
 * (see especially complete_token_location_large()).
 *)
type token_location = {
    str: string;
    charpos: int;

    line: int;
    column: int;

    file: Padioleau_common.filename;
  }
  (* with tarzan *)

let fake_token_location = {
  charpos = -1; str = ""; line = -1; column = -1; file = "";
}

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
    | Ab

   (* with tarzan *)

type token_mutable = {
  (* contains among other things the position of the token through
   * the token_location embedded inside the token_origin type.
   *)
  token : token_origin;
  mutable transfo: transformation;
  (* less: mutable comments: ...; *)
}

(* poor's man refactoring *)
and transformation =
  | NoTransfo
  | Remove
  | AddBefore of add
  | AddAfter of add
  | Replace of add
  | AddArgsBefore of string list

  and add =
    | AddStr of string
    | AddNewlineAndIdent

 (* with tarzan *)

type token_kind =
  (* for the fuzzy parser and sgrep/spatch fuzzy AST *)
  | LPar
  | RPar
  | LBrace
  | RBrace
  (* for the unparser helpers in spatch, and to filter
   * irrelevant tokens in the fuzzy parser
   *)
  | Esthet of esthet
  (* mostly for the lexer helpers, and for fuzzy parser *)
  (* less: want to factorize all those TH.is_eof to use that?
   * but extra cost? same for TH.is_comment?
   * todo: could maybe get rid of that now that we don't really use
   * berkeley DB and prefer Prolog, and so we don't need a sentinel
   * ast elements to associate the comments with it
   *)
  | Eof

  | Other

  and esthet =
   | Comment
   | Newline
   | Space

(* shortcut *)
type info = token_mutable


type parsing_stat = {
  filename: Padioleau_common.filename;
  mutable correct: int;
  mutable bad: int;
  (* used only for cpp for now *)
  mutable have_timeout: bool;
 (* by our cpp commentizer *)
  mutable commentized: int;
  (* if want to know exactly what was passed through, uncomment:
   *  
   * mutable passing_through_lines: int;
   * 
   * it differs from bad by starting from the error to
   * the synchro point instead of starting from start of
   * function to end of function.
   *)

  (* for instance to report most problematic macros when parse c/c++ *)
  mutable problematic_lines:
    (string list (* ident in error line *) * int (* line_error *)) list;
}
let default_stat file =  {
  filename = file;
  have_timeout = false;
  correct = 0; bad = 0;
  commentized = 0;
  problematic_lines = [];
}


(* Many parsers need to interact with the lexer, or use tricks around
 * the stream of tokens, or do some error recovery, or just need to
 * pass certain tokens (like the comments token) which requires
 * to have access to this stream of remaining tokens.
 * The token_state type helps.
 *)
type 'tok tokens_state = {
  mutable rest:         'tok list;
  mutable current:      'tok;
  (* it's passed since last "checkpoint", not passed from the beginning *)
  mutable passed:       'tok list;
  (* if want to do some lalr(k) hacking ... cf yacfe.
   * mutable passed_clean : 'tok list;
   * mutable rest_clean :   'tok list;
   *)
}
let mk_tokens_state toks = {
    rest       = toks;
    current    = (List.hd toks);
    passed = [];
    (* passed_clean = [];
     * rest_clean = (toks |> List.filter TH.is_not_comment);
     *)
  }

(*****************************************************************************)
(* Lexer helpers *)
(*****************************************************************************)

let lexbuf_to_strpos lexbuf     =
  (Lexing.lexeme lexbuf, Lexing.lexeme_start lexbuf)

let tokinfo_str_pos str pos =
  {
    token = OriginTok {
      charpos = pos;
      str     = str;

      (* info filled in a post-lexing phase, see complete_token_location_large*)
      line = -1;
      column = -1;
      file = "";
    };
    transfo = NoTransfo;
  }

(*
val rewrap_token_location : token_location.token_location -> info -> info
let rewrap_token_location pi ii =
  {ii with pinfo =
    (match ii.pinfo with
    | OriginTok _oldpi -> OriginTok pi
    | FakeTokStr _  | Ab | ExpandedTok _ ->
        failwith "rewrap_parseinfo: no OriginTok"
    )
  }
*)
let token_location_of_info ii =
  match ii.token with
  | OriginTok pinfo -> pinfo
  (* TODO ? dangerous ? *)
  | ExpandedTok (pinfo_pp, _pinfo_orig, _offset) -> pinfo_pp
  | FakeTokStr (_, (Some (pi, _))) -> pi

  | FakeTokStr (_, None)
  | Ab
    -> failwith "token_location_of_info: no OriginTok"

(* for error reporting *)
(*
let string_of_token_location x =
  spf "%s at %s:%d:%d" x.str x.file x.line x.column
*)
let string_of_token_location x =
  Padioleau_common.spf "%s:%d:%d" x.file x.line x.column

let string_of_info x =
  string_of_token_location (token_location_of_info x)

let str_of_info  ii = (token_location_of_info ii).str
let file_of_info ii = (token_location_of_info ii).file
let line_of_info ii = (token_location_of_info ii).line
let col_of_info  ii = (token_location_of_info ii).column

(* todo: return a Real | Virt position ? *)
let pos_of_info  ii = (token_location_of_info ii).charpos

let pinfo_of_info ii = ii.token

let is_origintok ii =
  match ii.token with
  | OriginTok _ -> true
  | _ -> false

(*
let opos_of_info ii = 
  PI.get_orig_info (function x -> x.PI.charpos) ii

val pos_of_tok     : Parser_cpp.token -> int
val str_of_tok     : Parser_cpp.token -> string
val file_of_tok    : Parser_cpp.token -> Common.filename

let pos_of_tok x =  Ast.opos_of_info (info_of_tok x)
let str_of_tok x =  Ast.str_of_info (info_of_tok x)
let file_of_tok x = Ast.file_of_info (info_of_tok x)
let pinfo_of_tok x = Ast.pinfo_of_info (info_of_tok x)

val is_origin : Parser_cpp.token -> bool
val is_expanded : Parser_cpp.token -> bool
val is_fake : Parser_cpp.token -> bool
val is_abstract : Parser_cpp.token -> bool


let is_origin x =
  match pinfo_of_tok x with Parse_info.OriginTok _ -> true | _ -> false
let is_expanded x =
  match pinfo_of_tok x with Parse_info.ExpandedTok _ -> true | _ -> false
let is_fake x =
  match pinfo_of_tok x with Parse_info.FakeTokStr _ -> true | _ -> false
let is_abstract x =
  match pinfo_of_tok x with Parse_info.Ab -> true | _ -> false
*)

(* info about the current location *)
(*
let get_pi = function
  | OriginTok pi -> pi
  | ExpandedTok (_,pi,_) -> pi
  | FakeTokStr (_,(Some (pi,_))) -> pi
  | FakeTokStr (_,None) ->
      failwith "FakeTokStr None"
  | Ab ->
      failwith "Ab"
*)

(* original info *)
let get_original_token_location = function
  | OriginTok pi -> pi
  | ExpandedTok (pi,_, _) -> pi
  | FakeTokStr (_,_) -> failwith "no position information"
  | Ab -> failwith "Ab"

(* used by token_helpers *)
(*
let get_info f ii =
  match ii.token with
  | OriginTok pi -> f pi
  | ExpandedTok (_,pi,_) -> f pi
  | FakeTokStr (_,Some (pi,_)) -> f pi
  | FakeTokStr (_,None) ->
      failwith "FakeTokStr None"
  | Ab ->
      failwith "Ab"
*)
(*
let get_orig_info f ii =
  match ii.token with
  | OriginTok pi -> f pi
  | ExpandedTok (pi,_, _) -> f pi
  | FakeTokStr (_,Some (pi,_)) -> f pi
  | FakeTokStr (_,None ) ->
      failwith "FakeTokStr None"
  | Ab ->
      failwith "Ab"
*)

(* not used but used to be useful in coccinelle *)
type posrv =
  | Real of token_location
  | Virt of
      token_location (* last real info before expanded tok *) *
      int (* virtual offset *)

let compare_pos ii1 ii2 =
  let get_pos = function
    | OriginTok pi -> Real pi
(* todo? I have this for lang_php/
    | FakeTokStr (s, Some (pi_orig, offset)) ->
        Virt (pi_orig, offset)
*)
    | FakeTokStr _
    | Ab
      -> failwith "get_pos: Ab or FakeTok"
    | ExpandedTok (_pi_pp, pi_orig, offset) ->
        Virt (pi_orig, offset)
  in
  let pos1 = get_pos (pinfo_of_info ii1) in
  let pos2 = get_pos (pinfo_of_info ii2) in
  match (pos1,pos2) with
  | (Real p1, Real p2) ->
      compare p1.charpos p2.charpos
  | (Virt (p1,_), Real p2) ->
      if Padioleau_common.(=|=) (compare p1.charpos p2.charpos) (-1)
      then (-1)
      else 1
  | (Real p1, Virt (p2,_)) ->
      if Padioleau_common.(=|=) (compare p1.charpos p2.charpos)  1
      then 1
      else (-1)
  | (Virt (p1,o1), Virt (p2,o2)) ->
      let poi1 = p1.charpos in
      let poi2 = p2.charpos in
      match compare poi1 poi2 with
      |	-1 -> -1
      |	0 -> compare o1 o2
      |	1 -> 1
      | _ -> raise Padioleau_common.Impossible


let min_max_ii_by_pos xs =
  match xs with
  | [] -> failwith "empty list, max_min_ii_by_pos"
  | [x] -> (x, x)
  | x::xs ->
      let pos_leq p1 p2 = Padioleau_common.(=|=) (compare_pos p1 p2) (-1) in
      xs |> List.fold_left (fun (minii,maxii) e ->
        let maxii' = if pos_leq maxii e then e else maxii in
        let minii' = if pos_leq e minii then e else minii in
        minii', maxii'
      ) (x,x)


let rewrap_str s ii =
  {ii with token =
    (match ii.token with
    | OriginTok pi -> OriginTok { pi with str = s;}
    | FakeTokStr (s, info) -> FakeTokStr (s, info)
    | Ab -> Ab
    | ExpandedTok _ ->
        (* ExpandedTok ({ pi with Common.str = s;},vpi) *)
        failwith "rewrap_str: ExpandedTok not allowed here"
    )
  }

let tok_add_s s ii  =
  rewrap_str ((str_of_info ii) ^ s) ii

(*****************************************************************************)
(* vtoken -> ocaml *)
(*****************************************************************************)
let vof_filename v = Ocaml.vof_string v

let vof_token_location {
                     str = v_str;
                     charpos = v_charpos;
                     line = v_line;
                     column = v_column;
                     file = v_file
                   } =
  let bnds = [] in
  let arg = vof_filename v_file in
  let bnd = ("file", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_column in
  let bnd = ("column", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_line in
  let bnd = ("line", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_int v_charpos in
  let bnd = ("charpos", arg) in
  let bnds = bnd :: bnds in
  let arg = Ocaml.vof_string v_str in
  let bnd = ("str", arg) in let bnds = bnd :: bnds in Ocaml.VDict bnds


let vof_token_origin =
  function
  | OriginTok v1 ->
      let v1 = vof_token_location v1 in Ocaml.VSum (("OriginTok", [ v1 ]))
  | FakeTokStr (v1, opt) ->
      let v1 = Ocaml.vof_string v1 in
      let opt = Ocaml.vof_option (fun (p1, i) ->
        Ocaml.VTuple [vof_token_location p1; Ocaml.vof_int i]
      ) opt
      in
      Ocaml.VSum (("FakeTokStr", [ v1; opt ]))
  | Ab -> Ocaml.VSum (("Ab", []))
  | ExpandedTok (v1, v2, v3) ->
      let v1 = vof_token_location v1 in
      let v2 = vof_token_location v2 in
      let v3 = Ocaml.vof_int v3 in
      Ocaml.VSum (("ExpandedTok", [ v1; v2; v3 ]))


let rec vof_transformation =
  function
  | NoTransfo -> Ocaml.VSum (("NoTransfo", []))
  | Remove -> Ocaml.VSum (("Remove", []))
  | AddBefore v1 -> let v1 = vof_add v1 in Ocaml.VSum (("AddBefore", [ v1 ]))
  | AddAfter v1 -> let v1 = vof_add v1 in Ocaml.VSum (("AddAfter", [ v1 ]))
  | Replace v1 -> let v1 = vof_add v1 in Ocaml.VSum (("Replace", [ v1 ]))
  | AddArgsBefore v1 -> let v1 = Ocaml.vof_list Ocaml.vof_string v1 in Ocaml.VSum
  (("AddArgsBefore", [ v1 ]))

and vof_add =
  function
  | AddStr v1 ->
      let v1 = Ocaml.vof_string v1 in Ocaml.VSum (("AddStr", [ v1 ]))
  | AddNewlineAndIdent -> Ocaml.VSum (("AddNewlineAndIdent", []))

let vof_info
 { token = v_token; transfo = v_transfo } =
  let bnds = [] in
  let arg = vof_transformation v_transfo in
  let bnd = ("transfo", arg) in
  let bnds = bnd :: bnds in
  let arg = vof_token_origin v_token in
  let bnd = ("token", arg) in 
  let bnds = bnd :: bnds in 
  Ocaml.VDict bnds


(*****************************************************************************)
(* Error location report *)
(*****************************************************************************)

(* A changen is a stand-in for a file for the underlying code.  We use
 * channels in the underlying parsing code as this avoids loading
 * potentially very large source files directly into memory before we
 * even parse them, but this makes it difficult to parse small chunks of
 * code.  The changen works around this problem by providing a channel,
 * size and source for underlying data.  This allows us to wrap a string
 * in a channel, or pass a file, depending on our needs. 
 *)
type changen = unit -> (in_channel * int * Padioleau_common.filename)

(* Many functions in parse_php were implemented in terms of files and
 * are now adapted to work in terms of changens.  However, we wish to
 * provide the original API to users.  This wraps changen-based functions
 * and makes them operate on filenames again. 
 *)
let file_wrap_changen : (changen -> 'a) -> (Padioleau_common.filename -> 'a) = fun f ->
  (fun file ->
    f (fun () -> (open_in file, Common2.filesize file, file)))




let full_charpos_to_pos_large_from_changen = fun changen ->
  let (chan, chansize, _) = changen () in

  let size = (chansize + 2) in

    (* old: let arr = Array.create size  (0,0) in *)
    let arr1 = Bigarray.Array1.create
      Bigarray.int Bigarray.c_layout size in
    let arr2 = Bigarray.Array1.create
      Bigarray.int Bigarray.c_layout size in
    Bigarray.Array1.fill arr1 0;
    Bigarray.Array1.fill arr2 0;

    let charpos   = ref 0 in
    let line  = ref 0 in

    let full_charpos_to_pos_aux () =
      try
        while true do begin
          let s = (input_line chan) in
          incr line;

          (* '... +1 do'  cos input_line dont return the trailing \n *)
          for i = 0 to (String.length s - 1) + 1 do
            (* old: arr.(!charpos + i) <- (!line, i); *)
            arr1.{!charpos + i} <- (!line);
            arr2.{!charpos + i} <- i;
          done;
          charpos := !charpos + String.length s + 1;
        end done
     with End_of_file ->
       for i = !charpos to (* old: Array.length arr *)
         Bigarray.Array1.dim arr1 - 1 do
         (* old: arr.(i) <- (!line, 0); *)
         arr1.{i} <- !line;
         arr2.{i} <- 0;
       done;
       ();
    in
    begin
      full_charpos_to_pos_aux ();
      close_in chan;
      (fun i -> arr1.{i}, arr2.{i})
    end

let full_charpos_to_pos_large2 =
  file_wrap_changen full_charpos_to_pos_large_from_changen

let full_charpos_to_pos_large a =
  Padioleau_common.profile_code "Common.full_charpos_to_pos_large"
    (fun () -> full_charpos_to_pos_large2 a)

let complete_token_location_large filename table x =
  { x with
    file = filename;
    line   = fst (table (x.charpos));
    column = snd (table (x.charpos));
  }

(*---------------------------------------------------------------------------*)
(* return line x col x str_line  from a charpos. This function is quite
 * expensive so don't use it to get the line x col from every token in
 * a file. Instead use full_charpos_to_pos.
 *)
let (info_from_charpos2: int -> Padioleau_common.filename -> (int * int * string)) =
  fun charpos filename ->

  (* Currently lexing.ml does not handle the line number position.
   * Even if there is some fields in the lexing structure, they are not
   * maintained by the lexing engine :( So the following code does not work:
   *   let pos = Lexing.lexeme_end_p lexbuf in
   *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
   *      (pos.pos_cnum - pos.pos_bol) in
   * Hence this function to overcome the previous limitation.
   *)
  let chan = open_in filename in
  let linen  = ref 0 in
  let posl   = ref 0 in
  let rec charpos_to_pos_aux last_valid =
    let s =
      try Some (input_line chan)
      with End_of_file when Padioleau_common.(=|=) charpos  last_valid -> None in
    incr linen;
    match s with
      Some s ->
        let s = s ^ "\n" in
        if (!posl + String.length s > charpos)
        then begin
          close_in chan;
          (!linen, charpos - !posl, s)
        end
        else begin
          posl := !posl + String.length s;
          charpos_to_pos_aux !posl;
        end
    | None -> (!linen, charpos - !posl, "\n")
  in
  let res = charpos_to_pos_aux 0 in
  close_in chan;
  res

let info_from_charpos a b =
  Padioleau_common.profile_code "Common.info_from_charpos" (fun () -> info_from_charpos2 a b)


(* Decalage is here to handle stuff such as cpp which include file and who
 * can make shift.
 *)
let (error_messagebis: Padioleau_common.filename -> (string * int) -> int -> string)=
 fun filename (lexeme, lexstart) decalage ->

  let charpos = lexstart      + decalage in
  let tok = lexeme in
  let (line, pos, linecontent) =  info_from_charpos charpos filename in
  Padioleau_common.spf "File \"%s\", line %d, column %d,  charpos = %d
    around = '%s', whole content = %s"
    filename line pos charpos tok (Common2.chop linecontent)

let error_message = fun filename (lexeme, lexstart) ->
  try error_messagebis filename (lexeme, lexstart) 0
  with
    End_of_file ->
      ("PB in Common.error_message, position " ^ Padioleau_common.i_to_s lexstart ^
       " given out of file:" ^ filename)

let error_message_token_location = fun info ->
  let filename = info.file in
  let lexeme = info.str in
  let lexstart = info.charpos in
  try error_messagebis filename (lexeme, lexstart) 0
  with
    End_of_file ->
      ("PB in Common.error_message, position " ^ Padioleau_common.i_to_s lexstart ^
       " given out of file:" ^ filename)

let error_message_info info =
  let pinfo = token_location_of_info info in
  error_message_token_location pinfo


let print_bad line_error (start_line, end_line) filelines  =
  begin
    Padioleau_common.pr2 ("badcount: " ^ Padioleau_common.i_to_s (end_line - start_line));

    for i = start_line to end_line do
      let line = filelines.(i) in

      if Padioleau_common.(=|=) i line_error
      then  Padioleau_common.pr2 ("BAD:!!!!!" ^ " " ^ line)
      else  Padioleau_common.pr2 ("bad:" ^ " " ^      line)
    done
  end


(*****************************************************************************)
(* Parsing statistics *)
(*****************************************************************************)

(* todo: stat per dir ?  give in terms of func_or_decl numbers:   
 * nbfunc_or_decl pbs / nbfunc_or_decl total ?/ 
 *
 * note: cela dit si y'a des fichiers avec des #ifdef dont on connait pas les 
 * valeurs alors on parsera correctement tout le fichier et pourtant y'aura 
 * aucune def  et donc aucune couverture en fait.   
 * ==> TODO evaluer les parties non parsé ? 
 *)

let print_parsing_stat_list ?(verbose=false)statxs =

  let total = (List.length statxs) in
  let perfect = 
    statxs 
      |> List.filter (function 
          {have_timeout = false; bad = 0; _} -> true | _ -> false)
      |> List.length 
  in

  if verbose then begin 
  Padioleau_common.pr "\n\n\n---------------------------------------------------------------";
  Padioleau_common.pr "pbs with files:";
  statxs 
    |> List.filter (function 
      | {have_timeout = true; _} -> true 
      | {bad = n; _} when n > 0 -> true 
      | _ -> false)
    |> List.iter (function 
        {filename = file; have_timeout = timeout; bad = n; _} -> 
        Padioleau_common.pr (file ^ "  " ^ (if timeout then "TIMEOUT" else Padioleau_common.i_to_s n));
        );

  Padioleau_common.pr "\n\n\n";
  Padioleau_common.pr "files with lots of tokens passed/commentized:";
  let threshold_passed = 100 in
  statxs 
    |> List.filter (function 
      | {commentized = n; _} when n > threshold_passed -> true
      | _ -> false)
    |> List.iter (function 
        {filename = file; commentized = n; _} -> 
        Padioleau_common.pr (file ^ "  " ^ (Padioleau_common.i_to_s n));
        );

        Padioleau_common.pr "\n\n\n";
  end;

  let good = statxs |> List.fold_left (fun acc {correct = x; _} -> acc+x) 0 in
  let bad  = statxs |> List.fold_left (fun acc {bad = x; _} -> acc+x) 0  in
  let passed = statxs |> List.fold_left (fun acc {commentized = x; _} -> acc+x) 0
  in
  let total_lines = good + bad in

  Padioleau_common.pr "---------------------------------------------------------------";
  Padioleau_common.pr (
  (Padioleau_common.spf "NB total files = %d; " total) ^
  (Padioleau_common.spf "NB total lines = %d; " total_lines) ^
  (Padioleau_common.spf "perfect = %d; " perfect) ^
  (Padioleau_common.spf "pbs = %d; "     (statxs |> List.filter (function 
      {bad = n; _} when n > 0 -> true | _ -> false) 
                               |> List.length)) ^
  (Padioleau_common.spf "timeout = %d; " (statxs |> List.filter (function 
      {have_timeout = true; _} -> true | _ -> false) 
                               |> List.length)) ^
  (Padioleau_common.spf "=========> %d" ((100 * perfect) / total)) ^ "%"
                                                          
 );
  let gf, badf = float_of_int good, float_of_int bad in
  let passedf = float_of_int passed in
  Padioleau_common.pr (
  (Padioleau_common.spf "nb good = %d,  nb passed = %d " good passed) ^
  (Padioleau_common.spf "=========> %f"  (100.0 *. (passedf /. gf)) ^ "%")
   );
   Padioleau_common.pr (
  (Padioleau_common.spf "nb good = %d,  nb bad = %d " good bad) ^
  (Padioleau_common.spf "=========> %f"  (100.0 *. (gf /. (gf +. badf))) ^ "%"
   )
  )

(*****************************************************************************)
(* Most problematic tokens *)
(*****************************************************************************)

(* inspired by a comment by a reviewer of my CC'09 paper *)
let lines_around_error_line ~context (file, line) =
  let arr = Common2.cat_array file in

  let startl = max 0 (line - context) in
  let endl   = min (Array.length arr) (line + context) in
  let res = ref [] in

  for i = startl to endl -1 do
    Padioleau_common.push arr.(i) res
  done;
  List.rev !res

let print_recurring_problematic_tokens xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun x ->
    let file = x.filename in
    x.problematic_lines |> List.iter (fun (xs, line_error) ->
      xs |> List.iter (fun s ->
        Common2.hupdate_default s
          (fun (old, example)  -> old + 1, example)
          (fun() -> 0, (file, line_error)) h;
      )));
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  Padioleau_common.pr2 ("maybe 10 most problematic tokens");
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  Padioleau_common.hash_to_list h
  |> List.sort (fun (_k1,(v1,_)) (_k2,(v2,_)) -> compare v2 v1)
  |> Padioleau_common.take_safe 10
  |> List.iter (fun (k,(i, (file_ex, line_ex))) ->
    Padioleau_common.pr2 (Padioleau_common.spf "%s: present in %d parsing errors" k i);
    Padioleau_common.pr2 ("example: ");
    let lines = lines_around_error_line ~context:2 (file_ex, line_ex) in
    lines |> List.iter (fun s -> Padioleau_common.pr2 ("       " ^ s));
  );
  Common2.pr2_xxxxxxxxxxxxxxxxx();
  ()

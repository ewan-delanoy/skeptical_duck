(* Joust: a Java lexer, parser, and pretty-printer written in OCaml
   Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
   Released under the GNU Lesser General Public License *)

(* ocamllex lexer for Java

   Attempts to conform to:

   The Java Language Specification
   Second Edition

   James Gosling, Bill Joy, Guy Steele, Gilad Bracha *)

{

open Jvsp_types

module Store = struct 

let main_ref = ref([]: positioned_token list);;

let add postok = (main_ref:=postok::(!main_ref)) ;;

let usual tokn buf =
  let token_content = Lexing.lexeme buf in 
  let token_start = (Lexing.lexeme_start buf)+1
  and token_length = String.length(token_content) in 
  let token_end = (token_start-1) + token_length in 
  let _ =add ({
    tok = tokn;
    start = token_start;
    endd = token_end;  
  }) in 
  tokn;;

let linebreak buf = usual (LINEBREAK(Lexing.lexeme buf)) buf ;;  

let whitespace buf = usual (WHITESPACE(Lexing.lexeme buf)) buf ;;

end ;;

module Comment = struct 

type t = { mutable buffer : string; mutable comment_start : int } ;;

let current = {buffer = "";comment_start=(-1);} ;; 

let start_traditional_comment buf =
  (
    current.buffer <- Lexing.lexeme buf;
    current.comment_start <- (Lexing.lexeme_start buf)+1;
  ) ;;

let store_current () =
  let len = String.length(current.buffer) in 
  let comment_end = (current.comment_start-1) + len in 
  Store.add ({
    tok = COMMENT(current.buffer);
    start = current.comment_start;
    endd = comment_end;  
  }) ;;


let continue_traditional_comment buf =
  let c = Lexing.lexeme_char buf 0 in
  current.buffer <- current.buffer ^ (String.make 1 c);;

let finish_and_store_traditional_comment buf =
  let _ = current.buffer <- current.buffer ^ (Lexing.lexeme buf) in 
  store_current();;

let store_double_slash_line_comment buf =
  let _ =(current.comment_start <- (Lexing.lexeme_start buf)+1;
  current.buffer <- Lexing.lexeme buf) in 
  store_current () ;;

end ;;

module Text_block = struct 

type t = { mutable tb_buffer : string; mutable tb_start : int } ;;

let current = {tb_buffer = "";tb_start=(-1);} ;; 

let start buf =
  (
    current.tb_buffer <- Lexing.lexeme buf;
    current.tb_start <- (Lexing.lexeme_start buf)+1;
  ) ;;

let store_current () =
  let len = String.length(current.tb_buffer) in 
  let block_end = (current.tb_start-1) + len in 
  Store.add ({
    tok = TEXT_BLOCK(current.tb_buffer);
    start = current.tb_start;
    endd = block_end;  
  }) ;;


let continue buf =
  let c = Lexing.lexeme_char buf 0 in
  current.tb_buffer <- current.tb_buffer ^ (String.make 1 c);;

let finish_and_store buf =
  let _ = current.tb_buffer <- current.tb_buffer ^ (Lexing.lexeme buf) in 
  store_current();;

end ;;

module Reserved = struct 

let hash_table_of_list list =
  let tbl = Hashtbl.create (List.length list)
  in
  List.iter (fun (s, t) -> Hashtbl.add tbl s t) list;
  tbl ;;

let list_for_literals = [
    ("false",BOOLEAN_LITERAL false);
    ("null",NULL_LITERAL);
    ("true",BOOLEAN_LITERAL true)
] ;;    
  
let list_for_lowlevel_types = Image.image (
    fun v->(v,LOWLEVEL_TYPE v)
) [
    "boolean";
    "byte";
    "char";
    "double";
    "float";
    "int";
    "long";
    "short";
] ;;


let words = hash_table_of_list ([
  "abstract", ABSTRACT;
  "assert", ASSERT;
  "break", BREAK;
  "case", CASE;
  "catch", CATCH;
  "class", CLASS;
  "const", CONST;
  "continue", CONTINUE;
  "default", DEFAULT;
  "do", DO;
  "else", ELSE;
  "enum", ENUM;
  "exports", EXPORTS;
  "extends", EXTENDS;
  "final", FINAL;
  "finally", FINALLY;
  "for", FOR;
  "goto", GOTO;
  "if", IF;
  "implements", IMPLEMENTS;
  "import", IMPORT;
  "instanceof", INSTANCEOF;
  "interface", INTERFACE;
  "module", MODULE;
  "native", NATIVE;
  "new", NEW;
  "nonsealed", NONSEALED;
  "open", OPEN;
  "opens", OPENS;
  "package", PACKAGE;
  "permits", PERMITS;
  "private", PRIVATE;
  "protected", PROTECTED;
  "provides", PROVIDES;
  "public", PUBLIC;
  "record", RECORD;
  "requires", REQUIRES;
  "return", RETURN;
  "sealed", SEALED;
  "static", STATIC;
  "strictfp", STRICTFP;
  "super", SUPER;
  "switch", SWITCH;
  "synchronized", SYNCHRONIZED;
  "this", THIS;
  "throw", THROW;
  "throws", THROWS;
  "transient", TRANSIENT;
  "transitive", TRANSITIVE;
  "try", TRY;
  "to", TO;
  "uses", USES;
  "var", VAR;
  "void", VOID;
  "volatile", VOLATILE;
  "while", WHILE;
  "with", WITH;
  "yield", YIELD
] @ list_for_literals 
  @list_for_lowlevel_types

 ) ;;

let lookup name =
  try Some (Hashtbl.find words name) with Not_found -> None ;;


end ;; 

let identifier buf =
  let s = Lexing.lexeme buf in
  match Reserved.lookup s with
  | Some t -> t
  | None -> IDENTIFIER s ;;

let assign_op buf =
  OPERATOR_EQ (Lexing.lexeme buf) ;;

exception Unterminated_comment ;;
exception Unterminated_text_block ;;
exception Illegal_escape_in_text_block of int;;
exception Unknown_char of char * int ;;

}

(* CHAPTER 3: Lexical Structure *)

(* 3.4 Line Terminators *)

let LF = '\n'  (* newline *)
let CR = '\r'  (* return *)

let LineTerminator = LF | CR | CR LF
let InputCharacter = [^ '\r' '\n']

(* 3.5 Input Elements and Tokens *)

let SUB = '\026' (* control-Z *) (* decimal *)

(* 3.6 White Space *)

let SP = ' '     (* space *)
let HT = '\t'    (* horizontal tab *)
let FF = '\012'  (* form feed *) (* decimal *)

let WhiteSpace = SP | HT | FF (* | LineTerminator -- handled separately *)

(* 3.7 Comments *)

(* let TraditionalComment = "/*" ([^ '*'] | '*' [^ '/'])* "*/" *)
let DoubleSlashLineComment = "//" InputCharacter* LineTerminator
(* let Comment = TraditionalComment | DoubleSlashLineComment *)

(* 3.8 Identifiers *)

let Letter = ['A'-'Z' 'a'-'z' '_' '$']
let Digit = ['0'-'9']
let Identifier = Letter (Letter | Digit)*

(* 3.10.1 Integer Literals *)

let IntegerTypeSuffix = ['l' 'L']

let DecimalIntegerLiteral = ('0' | ['1'-'9'] Digit*) IntegerTypeSuffix?

let HexDigit = ['0'-'9' 'a'-'f' 'A'-'F']
let HexIntegerLiteral = '0' ['x' 'X'] HexDigit+ IntegerTypeSuffix?

let OctalDigit = ['0'-'7']
let OctalIntegerLiteral = '0' OctalDigit+ IntegerTypeSuffix?

let IntegerLiteral =
  DecimalIntegerLiteral
| HexIntegerLiteral
| OctalIntegerLiteral

(* 3.10.2 Floating-Point Literals *)

let ExponentPart = ['e' 'E'] ['+' '-']? Digit+

let FloatTypeSuffix = ['f' 'F' 'd' 'D']

let FloatingPointLiteral =
  (Digit+ '.' Digit* | '.' Digit+) ExponentPart? FloatTypeSuffix?
| Digit+ (ExponentPart FloatTypeSuffix? | ExponentPart? FloatTypeSuffix)

(* 3.10.3 Boolean Literals *)

let BooleanLiteral = "true" | "false"

(* 3.10.6 Escape Sequences for Character and String Literals *)

let OctalEscape = '\\' ['0'-'3']? OctalDigit? OctalDigit

(* Not in spec -- added because we don't handle Unicode elsewhere. *)

let UnicodeEscape = "\\u" HexDigit HexDigit HexDigit HexDigit

let EscapableCharacter = ['b' 't' 'n' 'f' 'r' '"' '\'' '\\']

let EscapeSequence =
  '\\' EscapableCharacter
| OctalEscape
| UnicodeEscape

(* 3.10.4 Character Literals *)

let SingleCharacter = [^ '\'' '\\' '\n' '\r']
let CharacterLiteral = '\'' (SingleCharacter | EscapeSequence) '\''

(* 3.10.5 String Literals *)

let StringCharacter = [^ '"' '\\' '\n' '\r']
let StringLiteral = '"' (StringCharacter | EscapeSequence)* '"'

(* 3.10 Literals *)

let Literal =
  CharacterLiteral
| FloatingPointLiteral
| IntegerLiteral

let Text_blockOpener = "\"\"\""
let Text_blockCloser = "\"\"\""

(* Assignment operators, except '=', from section 3.12 *)

let AssignmentOperator =
  ('+' | '-' | '*' | '/' | '&' | '|' | '^' | '%' | "<<" | ">>" | ">>>") '='

rule token = parse
| WhiteSpace
    { let _ =Store.whitespace lexbuf in token lexbuf }
| LineTerminator
    { let _ =Store.linebreak lexbuf in token lexbuf }
| "/*"
    { Comment.start_traditional_comment lexbuf; traditional_comment lexbuf; token lexbuf }
| Text_blockOpener
    { Text_block.start lexbuf; text_block lexbuf; token lexbuf }    
| DoubleSlashLineComment
    { let _=Comment.store_double_slash_line_comment lexbuf in token lexbuf }
| Identifier
    { Store.usual (identifier lexbuf) lexbuf }
| CharacterLiteral
    { Store.usual (CHARACTER_LITERAL(Lexing.lexeme lexbuf)) lexbuf }  
| FloatingPointLiteral
    { Store.usual (FLOATING_POINT_LITERAL(Lexing.lexeme lexbuf)) lexbuf }   
| IntegerLiteral
    { Store.usual (INTEGER_LITERAL(Lexing.lexeme lexbuf)) lexbuf }  
| StringLiteral
    { Store.usual (STRING_LITERAL(Lexing.lexeme lexbuf)) lexbuf }         



(* 3.11 Separators *)
| '('  { Store.usual LP lexbuf }
| ')'  { Store.usual RP lexbuf }
| '{'  { Store.usual LC lexbuf }
| '}'  { Store.usual RC lexbuf }
| '['  { Store.usual LB lexbuf }
| ']'  { Store.usual RB lexbuf }
| ';'  { Store.usual SM lexbuf }
| ','  { Store.usual CM lexbuf }
| '.'  { Store.usual DOT lexbuf }

(* 3.12 Operators *)
| "="  { Store.usual EQ lexbuf }
| ">"  { Store.usual GT lexbuf }
| "<"  { Store.usual LT lexbuf }
| "!"  { Store.usual NOT lexbuf }
| "~"  { Store.usual COMPL lexbuf }
| "?"  { Store.usual COND lexbuf }
| ":"  { Store.usual COLON lexbuf }
| "=="  { Store.usual EQ_EQ lexbuf }
| "<="  { Store.usual LE lexbuf }
| ">="  { Store.usual GE lexbuf }
| "!="  { Store.usual NOT_EQ lexbuf }
| "&&"  { Store.usual AND_AND lexbuf }
| "||"  { Store.usual OR_OR lexbuf }
| "++"  { Store.usual INCR lexbuf }
| "--"  { Store.usual DECR lexbuf }
| "+"  { Store.usual PLUS lexbuf }
| "-"  { Store.usual MINUS lexbuf }
| "*"  { Store.usual TIMES lexbuf }
| "/"  { Store.usual DIV lexbuf }
| "&"  { Store.usual AND lexbuf }
| "|"  { Store.usual OR lexbuf }
| "^"  { Store.usual XOR lexbuf }
| "%"  { Store.usual MOD lexbuf }
| "<<"  { Store.usual LS lexbuf }
| ">>"  { Store.usual SRS lexbuf }
| ">>>"  { Store.usual URS lexbuf }
| AssignmentOperator  { Store.usual (assign_op lexbuf) lexbuf }
| "@"  { Store.usual SNAIL lexbuf }

| SUB? eof { EOF }
| _ as c { raise(Unknown_char(c,(Lexing.lexeme_start lexbuf)+1))}

and traditional_comment = parse
  "*/" { Comment.finish_and_store_traditional_comment lexbuf }
| eof  { raise Unterminated_comment }
| _  { Comment.continue_traditional_comment lexbuf; traditional_comment lexbuf }

and text_block = parse
  '\\' { Text_block.continue lexbuf; text_block_after_backslash lexbuf }
| Text_blockCloser { Text_block.finish_and_store lexbuf }
| eof  { raise Unterminated_text_block }
| _  { Text_block.continue lexbuf; text_block lexbuf }

and text_block_after_backslash = parse
  EscapableCharacter { Text_block.continue lexbuf;text_block lexbuf }
| LineTerminator { Text_block.continue lexbuf;text_block lexbuf }  
| eof  { raise Unterminated_text_block }
| _  { raise (Illegal_escape_in_text_block((Lexing.lexeme_start lexbuf)+1)) }


{
  let parse txt= 
    let lexie = Lexing.from_string txt 
    and _=(Store.main_ref:=[]) in
    let walker = ref(token lexie) in  
    let _=(while (!walker)<>EOF
      do
        walker:= (token lexie)
      done)  in 
    List.rev(!(Store.main_ref));;
}
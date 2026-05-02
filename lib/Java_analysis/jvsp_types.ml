(*

#use"lib/Java_analysis/jvsp_types.ml";;

*)


type ident = { id : string; pos : int } ;;

type op = string ;; 

type token =
  IDENTIFIER of string
| LITERAL of op
| PRIMITIVE_TYPE of op
(* Separators *)
|LP		(* ( *)
|RP		(* ) *)
|LC		(* { *)
|RC		(* } *)
|LB		(* [ *)
|RB		(* ] *)
|SM		(* ; *)
|CM		(* , *)
|DOT	(* . *)

(* Operators *)
|EQ		  (* = *)
|GT		  (* > *)
|LT		  (* < *)
|NOT		(* ! *)
|COMPL	(* ~ *)
|COND		(* ? *)
|COLON	(* : *)
|EQ_EQ	(* == *)
|LE		  (* <= *)
|GE		  (* >= *)
|NOT_EQ		(* != *)
|AND_AND	(* && *)
|OR_OR	(* || *)
|INCR		(* ++ *)
|DECR		(* -- *)
|PLUS		(* + *)
|MINUS	(* - *)
|TIMES	(* * *)
|DIV		(* / *)
|AND		(* & *)
|OR		  (* | *)
|XOR		(* ^ *)
|MOD		(* % *)
|LS		  (* << *)
|SRS		(* >> *)
|URS		(* >>> *)
|OPERATOR_EQ of op	(* += -= *= /= &= |= ^= %= <<= >>= >>>= *)
|ANNOTATOR of string
(* Keywords*)
|ABSTRACT |BOOLEAN |BREAK |BYTE |CASE |CATCH |CHAR |CLASS |CONST |CONTINUE
|DEFAULT |DO |DOUBLE |ELSE |EXTENDS |FINAL |FINALLY |FLOAT |FOR |GOTO
|IF |IMPLEMENTS |IMPORT |INSTANCEOF |INT |INTERFACE |LONG
|NATIVE |NEW |PACKAGE |PRIVATE |PROTECTED |PUBLIC |RETURN
|SHORT |STATIC |STRICTFP |SUPER |SWITCH |SYNCHRONIZED
|THIS |THROW |THROWS |TRANSIENT |TRY |VOID |VOLATILE |WHILE
|EOF 
(* Tokens not used during parsing *)
|COMMENT of string 
|WHITESPACE of string ;;

type positioned_token = { tok:token; start:int; endd:int;} ;;
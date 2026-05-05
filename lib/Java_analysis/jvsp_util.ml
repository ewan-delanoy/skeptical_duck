(*

#use"lib/Java_analysis/jvsp_util.ml";;

*)

type token = Jvsp_types.token =
  IDENTIFIER of string
(* Literals *)  
|BOOLEAN_LITERAL of bool
|CHARACTER_LITERAL of string
|FLOATING_POINT_LITERAL of string
|INTEGER_LITERAL of string
|NULL_LITERAL
|STRING_LITERAL of string
|TEXT_BLOCK of string
|LOWLEVEL_TYPE of string
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
|OPERATOR_EQ of string	(* += -= *= /= &= |= ^= %= <<= >>= >>>= *)
|SNAIL
(* Keywords*)
|ABSTRACT |ASSERT |BOOLEAN |BREAK |BYTE |CASE |CATCH |CHAR |CLASS 
|CONST |CONTINUE |DEFAULT |DO |DOUBLE |ELSE |ENUM |EXPORTS |EXTENDS 
|FINAL |FINALLY |FLOAT |FOR |GOTO |IF |IMPLEMENTS |IMPORT |INSTANCEOF 
|INT |INTERFACE |LONG |MODULE |NATIVE |NEW |NONSEALED |OPEN |OPENS 
|PACKAGE |PERMITS |PRIVATE |PROTECTED |PROVIDES |PUBLIC |RECORD |REQUIRES 
|RETURN |SEALED |SHORT |STATIC |STRICTFP |SUPER |SWITCH |SYNCHRONIZED
|THIS |THROW |THROWS |TRANSIENT |TRANSITIVE |TRY |TO |USES 
|VAR |VOID |VOLATILE |WHILE |WITH |YIELD
|EOF 
(* Inactive tokens during parsing *)
|COMMENT of string 
|WHITESPACE of string ;;


type token_type = Jvsp_types.token_type =
  IDENTIFIER_T 
|BOOLEAN_LITERAL_T
|CHARACTER_LITERAL_T
|FLOATING_POINT_LITERAL_T
|INTEGER_LITERAL_T
|NULL_LITERAL_T
|STRING_LITERAL_T
|TEXT_BLOCK_T
|LOWLEVEL_TYPE_T
(* Separators *)
|LP_T		(* ( *)
|RP_T		(* ) *)
|LC_T		(* { *)
|RC_T		(* } *)
|LB_T		(* [ *)
|RB_T		(* ] *)
|SM_T		(* ; *)
|CM_T		(* , *)
|DOT_T	(* . *)

(* Operators *)
|EQ_T		  (* = *)
|GT_T		  (* > *)
|LT_T		  (* < *)
|NOT_T		(* ! *)
|COMPL_T	(* ~ *)
|COND_T		(* ? *)
|COLON_T	(* : *)
|EQ_EQ_T	(* == *)
|LE_T		  (* <= *)
|GE_T		  (* >= *)
|NOT_EQ_T		(* != *)
|AND_AND_T	(* && *)
|OR_OR_T	(* || *)
|INCR_T		(* ++ *)
|DECR_T		(* -- *)
|PLUS_T		(* + *)
|MINUS_T	(* - *)
|TIMES_T	(* * *)
|DIV_T		(* / *)
|AND_T		(* & *)
|OR_T		    (* | *)
|XOR_T		(* ^ *)
|MOD_T		(* % *)
|LS_T		  (* << *)
|SRS_T		(* >> *)
|URS_T		(* >>> *)
|OPERATOR_EQ_T 	(* += -= *= /= &= |= ^= %= <<= >>= >>>= *)
|SNAIL_T 
(* Keywords*)
|ABSTRACT_T |ASSERT_T |BOOLEAN_T |BREAK_T |BYTE_T |CASE_T |CATCH_T |CHAR_T |CLASS_T 
|CONST_T |CONTINUE_T |DEFAULT_T |DO_T |DOUBLE_T |ELSE_T |ENUM_T |EXPORTS_T |EXTENDS_T 
|FINAL_T |FINALLY_T |FLOAT_T |FOR_T |GOTO_T |IF_T |IMPLEMENTS_T |IMPORT_T |INSTANCEOF_T 
|INT_T |INTERFACE_T |LONG_T |MODULE_T |NATIVE_T |NEW_T |NONSEALED_T |OPEN_T |OPENS_T 
|PACKAGE_T |PERMITS_T |PRIVATE_T |PROTECTED_T |PROVIDES_T |PUBLIC_T |RECORD_T |REQUIRES_T 
|RETURN_T |SEALED_T |SHORT_T |STATIC_T |STRICTFP_T |SUPER_T |SWITCH_T |SYNCHRONIZED_T
|THIS_T |THROW_T |THROWS_T |TRANSIENT_T |TRANSITIVE_T |TRY_T |TO_T |USES_T 
|VAR_T |VOID_T |VOLATILE_T |WHILE_T |WITH_T |YIELD_T
|EOF_T  ;;

let token_to_string = function
|(IDENTIFIER txt) -> txt
|(BOOLEAN_LITERAL b) -> string_of_bool b
|(CHARACTER_LITERAL txt) -> txt
|(FLOATING_POINT_LITERAL txt) -> txt
|(INTEGER_LITERAL txt) -> txt
|NULL_LITERAL -> "null"
|(STRING_LITERAL txt) -> txt
|(TEXT_BLOCK txt) -> txt
|(LOWLEVEL_TYPE txt) -> txt
|LP -> "("
|RP -> ")"
|LC -> "{"
|RC -> "}"
|LB -> "["
|RB -> "]"
|SM -> ";"
|CM -> ","
|DOT -> "."
|EQ -> "="
|GT -> ">"
|LT -> "<"
|NOT -> "!"
|COMPL -> "~"
|COND -> "?"
|COLON -> ":"
|EQ_EQ -> "=="
|LE -> "<="
|GE -> ">="
|NOT_EQ -> "!="
|AND_AND -> "&&"
|OR_OR -> "||"
|INCR -> "++"
|DECR -> "--"
|PLUS -> "+"
|MINUS -> "-"
|TIMES -> "*"
|DIV -> "/"
|AND -> "&"
|OR -> "|"
|XOR -> "^"
|MOD -> "%"
|LS -> "<<"
|SRS -> ">>"
|URS -> ">>>"
|(OPERATOR_EQ txt) -> txt
|SNAIL -> "@"
|ABSTRACT -> "abstract"
|ASSERT -> "assert"
|BOOLEAN -> "boolean"
|BREAK -> "break"
|BYTE -> "byte"
|CASE -> "case"
|CATCH -> "catch"
|CHAR -> "char"
|CLASS -> "class"
|CONST -> "const"
|CONTINUE -> "continue"
|DEFAULT -> "default"
|DO -> "do"
|DOUBLE -> "double"
|ELSE -> "else"
|ENUM -> "enum"
|EXPORTS -> "exports"
|EXTENDS -> "extends"
|FINAL -> "final"
|FINALLY -> "finally"
|FLOAT -> "float"
|FOR -> "for"
|GOTO -> "goto"
|IF -> "if"
|IMPLEMENTS -> "implements"
|IMPORT -> "import"
|INSTANCEOF -> "instanceof"
|INT -> "int"
|INTERFACE -> "interface"
|LONG -> "long"
|MODULE -> "module"
|NATIVE -> "native"
|NEW -> "new"
|NONSEALED -> "nonsealed"
|OPEN -> "open"
|OPENS -> "opens"
|PACKAGE -> "package"
|PERMITS -> "permits"
|PRIVATE -> "private"
|PROTECTED -> "protected"
|PROVIDES -> "provides"
|PUBLIC -> "public"
|RECORD -> "record"
|REQUIRES -> "requires"
|RETURN -> "return"
|SEALED -> "sealed"
|SHORT -> "short"
|STATIC -> "static"
|STRICTFP -> "strictfp"
|SUPER -> "super"
|SWITCH -> "switch"
|SYNCHRONIZED -> "synchronized"
|THIS -> "this"
|THROW -> "throw"
|THROWS -> "throws"
|TRANSIENT -> "transient"
|TRANSITIVE -> "transitive"
|TRY -> "try"
|TO -> "to"
|USES -> "uses"
|VAR -> "var"
|VOID -> "void"
|VOLATILE -> "volatile"
|WHILE -> "while"
|WITH -> "with"
|YIELD -> "yield"
|EOF -> "eof"
|(COMMENT txt) -> txt
|(WHITESPACE txt) -> txt ;;

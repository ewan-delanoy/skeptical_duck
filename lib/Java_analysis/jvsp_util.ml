(*

#use"lib/Java_analysis/jvsp_util.ml";;

*)

module Private = struct

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
|WHITESPACE of string 
|LINEBREAK of string;;


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
|EOF_T  
|COMMENT_T 
|WHITESPACE_T 
|LINEBREAK_T;;

let ocaml_name_for_token_type = function 
|IDENTIFIER_T -> "IDENTIFIER_T"
|BOOLEAN_LITERAL_T -> "BOOLEAN_LITERAL_T"
|CHARACTER_LITERAL_T -> "CHARACTER_LITERAL_T"
|FLOATING_POINT_LITERAL_T -> "FLOATING_POINT_LITERAL_T"
|INTEGER_LITERAL_T  -> "INTEGER_LITERAL_T"
|NULL_LITERAL_T  -> "NULL_LITERAL_T"
|STRING_LITERAL_T  -> "STRING_LITERAL_T"
|TEXT_BLOCK_T  -> "TEXT_BLOCK_T"
|LOWLEVEL_TYPE_T  -> "LOWLEVEL_TYPE_T"
|LP_T -> "LP_T"
|RP_T -> "RP_T"
|LC_T -> "LC_T"
|RC_T -> "RC_T"
|LB_T -> "LB_T"
|RB_T -> "RB_T"
|SM_T -> "SM_T"
|CM_T -> "CM_T"
|DOT_T -> "DOT_T"
|EQ_T -> "EQ_T"
|GT_T -> "GT_T"
|LT_T -> "LT_T"
|NOT_T -> "NOT_T"
|COMPL_T -> "COMPL_T"
|COND_T -> "COND_T"
|COLON_T -> "COLON_T"
|EQ_EQ_T -> "EQ_EQ_T"
|LE_T -> "LE_T"
|GE_T -> "GE_T"
|NOT_EQ_T -> "NOT_EQ_T"
|AND_AND_T -> "AND_AND_T"
|OR_OR_T -> "OR_OR_T"
|INCR_T -> "INCR_T"
|DECR_T -> "DECR_T"
|PLUS_T -> "PLUS_T"
|MINUS_T -> "MINUS_T"
|TIMES_T -> "TIMES_T"
|DIV_T -> "DIV_T"
|AND_T -> "AND_T"
|OR_T -> "OR_T"
|XOR_T -> "XOR_T"
|MOD_T -> "MOD_T"
|LS_T -> "LS_T"
|SRS_T -> "SRS_T"
|URS_T -> "URS_T"
|OPERATOR_EQ_T -> "OPERATOR_EQ_T"
|SNAIL_T -> "SNAIL_T"
|ABSTRACT_T -> "ABSTRACT_T"
|ASSERT_T -> "ASSERT_T"
|BOOLEAN_T -> "BOOLEAN_T"
|BREAK_T -> "BREAK_T"
|BYTE_T -> "BYTE_T"
|CASE_T -> "CASE_T"
|CATCH_T -> "CATCH_T"
|CHAR_T -> "CHAR_T"
|CLASS_T -> "CLASS_T"
|CONST_T -> "CONST_T"
|CONTINUE_T -> "CONTINUE_T"
|DEFAULT_T -> "DEFAULT_T"
|DO_T -> "DO_T"
|DOUBLE_T -> "DOUBLE_T"
|ELSE_T -> "ELSE_T"
|ENUM_T -> "ENUM_T"
|EXPORTS_T -> "EXPORTS_T"
|EXTENDS_T -> "EXTENDS_T"
|FINAL_T -> "FINAL_T"
|FINALLY_T -> "FINALLY_T"
|FLOAT_T -> "FLOAT_T"
|FOR_T -> "FOR_T"
|GOTO_T -> "GOTO_T"
|IF_T -> "IF_T"
|IMPLEMENTS_T -> "IMPLEMENTS_T"
|IMPORT_T -> "IMPORT_T"
|INSTANCEOF_T -> "INSTANCEOF_T"
|INT_T -> "INT_T"
|INTERFACE_T -> "INTERFACE_T"
|LONG_T -> "LONG_T"
|MODULE_T -> "MODULE_T"
|NATIVE_T -> "NATIVE_T"
|NEW_T -> "NEW_T"
|NONSEALED_T -> "NONSEALED_T"
|OPEN_T -> "OPEN_T"
|OPENS_T -> "OPENS_T"
|PACKAGE_T -> "PACKAGE_T"
|PERMITS_T -> "PERMITS_T"
|PRIVATE_T -> "PRIVATE_T"
|PROTECTED_T -> "PROTECTED_T"
|PROVIDES_T -> "PROVIDES_T"
|PUBLIC_T -> "PUBLIC_T"
|RECORD_T -> "RECORD_T"
|REQUIRES_T -> "REQUIRES_T"
|RETURN_T -> "RETURN_T"
|SEALED_T -> "SEALED_T"
|SHORT_T -> "SHORT_T"
|STATIC_T -> "STATIC_T"
|STRICTFP_T -> "STRICTFP_T"
|SUPER_T -> "SUPER_T"
|SWITCH_T -> "SWITCH_T"
|SYNCHRONIZED_T -> "SYNCHRONIZED_T"
|THIS_T -> "THIS_T"
|THROW_T -> "THROW_T"
|THROWS_T -> "THROWS_T"
|TRANSIENT_T -> "TRANSIENT_T"
|TRANSITIVE_T -> "TRANSITIVE_T"
|TRY_T -> "TRY_T"
|TO_T -> "TO_T"
|USES_T -> "USES_T"
|VAR_T -> "VAR_T"
|VOID_T -> "VOID_T"
|VOLATILE_T -> "VOLATILE_T"
|WHILE_T -> "WHILE_T"
|WITH_T -> "WITH_T"
|YIELD_T -> "YIELD_T"
|EOF_T -> "EOF_T"
|COMMENT_T -> "COMMENT_T"
|WHITESPACE_T -> "WHITESPACE_T"
|LINEBREAK_T -> "LINEBREAK_T" ;;

let get_token_type = function
|(IDENTIFIER _) -> IDENTIFIER_T
|(BOOLEAN_LITERAL _) -> BOOLEAN_LITERAL_T
|(CHARACTER_LITERAL _) -> CHARACTER_LITERAL_T
|(FLOATING_POINT_LITERAL _) -> FLOATING_POINT_LITERAL_T
|(INTEGER_LITERAL _) -> INTEGER_LITERAL_T
|NULL_LITERAL -> NULL_LITERAL_T
|(STRING_LITERAL _) -> STRING_LITERAL_T
|(TEXT_BLOCK _) -> TEXT_BLOCK_T
|(LOWLEVEL_TYPE _) -> LOWLEVEL_TYPE_T
|LP -> LP_T
|RP -> RP_T
|LC -> LC_T
|RC -> RC_T
|LB -> LB_T
|RB -> RB_T
|SM -> SM_T
|CM -> CM_T
|DOT -> DOT_T
|EQ -> EQ_T
|GT -> GT_T
|LT -> LT_T
|NOT -> NOT_T
|COMPL -> COMPL_T
|COND -> COND_T
|COLON -> COLON_T
|EQ_EQ -> EQ_EQ_T
|LE -> LE_T
|GE -> GE_T
|NOT_EQ -> NOT_EQ_T
|AND_AND -> AND_AND_T
|OR_OR -> OR_OR_T
|INCR -> INCR_T
|DECR -> DECR_T
|PLUS -> PLUS_T
|MINUS -> MINUS_T
|TIMES -> TIMES_T
|DIV -> DIV_T
|AND -> AND_T
|OR -> OR_T
|XOR -> XOR_T
|MOD -> MOD_T
|LS -> LS_T
|SRS -> SRS_T
|URS -> URS_T
|(OPERATOR_EQ _) -> OPERATOR_EQ_T
|SNAIL -> SNAIL_T
|ABSTRACT -> ABSTRACT_T
|ASSERT -> ASSERT_T
|BOOLEAN -> BOOLEAN_T
|BREAK -> BREAK_T
|BYTE -> BYTE_T
|CASE -> CASE_T
|CATCH -> CATCH_T
|CHAR -> CHAR_T
|CLASS -> CLASS_T
|CONST -> CONST_T
|CONTINUE -> CONTINUE_T
|DEFAULT -> DEFAULT_T
|DO -> DO_T
|DOUBLE -> DOUBLE_T
|ELSE -> ELSE_T
|ENUM -> ENUM_T
|EXPORTS -> EXPORTS_T
|EXTENDS -> EXTENDS_T
|FINAL -> FINAL_T
|FINALLY -> FINALLY_T
|FLOAT -> FLOAT_T
|FOR -> FOR_T
|GOTO -> GOTO_T
|IF -> IF_T
|IMPLEMENTS -> IMPLEMENTS_T
|IMPORT -> IMPORT_T
|INSTANCEOF -> INSTANCEOF_T
|INT -> INT_T
|INTERFACE -> INTERFACE_T
|LONG -> LONG_T
|MODULE -> MODULE_T
|NATIVE -> NATIVE_T
|NEW -> NEW_T
|NONSEALED -> NONSEALED_T
|OPEN -> OPEN_T
|OPENS -> OPENS_T
|PACKAGE -> PACKAGE_T
|PERMITS -> PERMITS_T
|PRIVATE -> PRIVATE_T
|PROTECTED -> PROTECTED_T
|PROVIDES -> PROVIDES_T
|PUBLIC -> PUBLIC_T
|RECORD -> RECORD_T
|REQUIRES -> REQUIRES_T
|RETURN -> RETURN_T
|SEALED -> SEALED_T
|SHORT -> SHORT_T
|STATIC -> STATIC_T
|STRICTFP -> STRICTFP_T
|SUPER -> SUPER_T
|SWITCH -> SWITCH_T
|SYNCHRONIZED -> SYNCHRONIZED_T
|THIS -> THIS_T
|THROW -> THROW_T
|THROWS -> THROWS_T
|TRANSIENT -> TRANSIENT_T
|TRANSITIVE -> TRANSITIVE_T
|TRY -> TRY_T
|TO -> TO_T
|USES -> USES_T
|VAR -> VAR_T
|VOID -> VOID_T
|VOLATILE -> VOLATILE_T
|WHILE -> WHILE_T
|WITH -> WITH_T
|YIELD -> YIELD_T
|EOF -> EOF_T
|(COMMENT _) -> COMMENT_T
|(WHITESPACE _) -> WHITESPACE_T 
|(LINEBREAK _) -> LINEBREAK_T ;;


let passive_token_types = [COMMENT_T;WHITESPACE_T;LINEBREAK_T] ;;

let extract_nonpassive_token_type_opt postok =
   let tok =postok.Jvsp_types.tok in 
   let toktype = get_token_type tok in 
   if List.mem toktype passive_token_types 
   then None 
   else Some toktype ;;     

let summary_of_token_type = function
|IDENTIFIER_T -> "ident"
|BOOLEAN_LITERAL_T -> "bool"
|CHARACTER_LITERAL_T -> "charlit"
|FLOATING_POINT_LITERAL_T -> "float"
|INTEGER_LITERAL_T -> "int"
|STRING_LITERAL_T -> "str"
|TEXT_BLOCK_T -> "text"
|LOWLEVEL_TYPE_T -> "typ"
|OPERATOR_EQ_T -> "opeq"
|COMMENT_T -> "comment"
|WHITESPACE_T -> "white"
|LINEBREAK_T -> "linebreak"
|NULL_LITERAL_T -> "null"
|LP_T -> "("
|RP_T -> ")"
|LC_T -> "{"
|RC_T -> "}"
|LB_T -> "["
|RB_T -> "]"
|SM_T -> ";"
|CM_T -> ","
|DOT_T -> "."
|EQ_T -> "="
|GT_T -> ">"
|LT_T -> "<"
|NOT_T -> "!"
|COMPL_T -> "~"
|COND_T -> "?"
|COLON_T -> ":"
|EQ_EQ_T -> "=="
|LE_T -> "<="
|GE_T -> ">="
|NOT_EQ_T -> "!="
|AND_AND_T -> "&&"
|OR_OR_T -> "||"
|INCR_T -> "++"
|DECR_T -> "--"
|PLUS_T -> "+"
|MINUS_T -> "-"
|TIMES_T -> "*"
|DIV_T -> "/"
|AND_T -> "&"
|OR_T -> "|"
|XOR_T -> "^"
|MOD_T -> "%"
|LS_T -> "<<"
|SRS_T -> ">>"
|URS_T -> ">>>"
|SNAIL_T -> "@"
|ABSTRACT_T -> "abstract"
|ASSERT_T -> "assert"
|BOOLEAN_T -> "boolean"
|BREAK_T -> "break"
|BYTE_T -> "byte"
|CASE_T -> "case"
|CATCH_T -> "catch"
|CHAR_T -> "char"
|CLASS_T -> "class"
|CONST_T -> "const"
|CONTINUE_T -> "continue"
|DEFAULT_T -> "default"
|DO_T -> "do"
|DOUBLE_T -> "double"
|ELSE_T -> "else"
|ENUM_T -> "enum"
|EXPORTS_T -> "exports"
|EXTENDS_T -> "extends"
|FINAL_T -> "final"
|FINALLY_T -> "finally"
|FLOAT_T -> "float"
|FOR_T -> "for"
|GOTO_T -> "goto"
|IF_T -> "if"
|IMPLEMENTS_T -> "implements"
|IMPORT_T -> "import"
|INSTANCEOF_T -> "instanceof"
|INT_T -> "int"
|INTERFACE_T -> "interface"
|LONG_T -> "long"
|MODULE_T -> "module"
|NATIVE_T -> "native"
|NEW_T -> "new"
|NONSEALED_T -> "nonsealed"
|OPEN_T -> "open"
|OPENS_T -> "opens"
|PACKAGE_T -> "package"
|PERMITS_T -> "permits"
|PRIVATE_T -> "private"
|PROTECTED_T -> "protected"
|PROVIDES_T -> "provides"
|PUBLIC_T -> "public"
|RECORD_T -> "record"
|REQUIRES_T -> "requires"
|RETURN_T -> "return"
|SEALED_T -> "sealed"
|SHORT_T -> "short"
|STATIC_T -> "static"
|STRICTFP_T -> "strictfp"
|SUPER_T -> "super"
|SWITCH_T -> "switch"
|SYNCHRONIZED_T -> "synchronized"
|THIS_T -> "this"
|THROW_T -> "throw"
|THROWS_T -> "throws"
|TRANSIENT_T -> "transient"
|TRANSITIVE_T -> "transitive"
|TRY_T -> "try"
|TO_T -> "to"
|USES_T -> "uses"
|VAR_T -> "var"
|VOID_T -> "void"
|VOLATILE_T -> "volatile"
|WHILE_T -> "while"
|WITH_T -> "with"
|YIELD_T -> "yield"
|EOF_T -> "eof" ;;

let has_variable_content = function
|IDENTIFIER_T
|BOOLEAN_LITERAL_T
|CHARACTER_LITERAL_T
|FLOATING_POINT_LITERAL_T
|INTEGER_LITERAL_T
|STRING_LITERAL_T
|TEXT_BLOCK_T
|LOWLEVEL_TYPE_T
|OPERATOR_EQ_T
|COMMENT_T
|WHITESPACE_T
|LINEBREAK_T -> true
|NULL_LITERAL_T
|LP_T
|RP_T
|LC_T
|RC_T
|LB_T
|RB_T
|SM_T
|CM_T
|DOT_T
|EQ_T
|GT_T
|LT_T
|NOT_T
|COMPL_T
|COND_T
|COLON_T
|EQ_EQ_T
|LE_T
|GE_T
|NOT_EQ_T
|AND_AND_T
|OR_OR_T
|INCR_T
|DECR_T
|PLUS_T
|MINUS_T
|TIMES_T
|DIV_T
|AND_T
|OR_T
|XOR_T
|MOD_T
|LS_T
|SRS_T
|URS_T
|SNAIL_T
|ABSTRACT_T
|ASSERT_T
|BOOLEAN_T
|BREAK_T
|BYTE_T
|CASE_T
|CATCH_T
|CHAR_T
|CLASS_T
|CONST_T
|CONTINUE_T
|DEFAULT_T
|DO_T
|DOUBLE_T
|ELSE_T
|ENUM_T
|EXPORTS_T
|EXTENDS_T
|FINAL_T
|FINALLY_T
|FLOAT_T
|FOR_T
|GOTO_T
|IF_T
|IMPLEMENTS_T
|IMPORT_T
|INSTANCEOF_T
|INT_T
|INTERFACE_T
|LONG_T
|MODULE_T
|NATIVE_T
|NEW_T
|NONSEALED_T
|OPEN_T
|OPENS_T
|PACKAGE_T
|PERMITS_T
|PRIVATE_T
|PROTECTED_T
|PROVIDES_T
|PUBLIC_T
|RECORD_T
|REQUIRES_T
|RETURN_T
|SEALED_T
|SHORT_T
|STATIC_T
|STRICTFP_T
|SUPER_T
|SWITCH_T
|SYNCHRONIZED_T
|THIS_T
|THROW_T
|THROWS_T
|TRANSIENT_T
|TRANSITIVE_T
|TRY_T
|TO_T
|USES_T
|VAR_T
|VOID_T
|VOLATILE_T
|WHILE_T
|WITH_T
|YIELD_T
|EOF_T -> false ;;

end ;;

open Private ;;

let all_token_types = 
[ABSTRACT_T;AND_AND_T;AND_T;ASSERT_T;BOOLEAN_T;BREAK_T;BYTE_T;CASE_T;CATCH_T;CHAR_T;CLASS_T;CM_T;COLON_T;
COMMENT_T;COMPL_T;COND_T;CONTINUE_T;DECR_T;DEFAULT_T;DIV_T;DOT_T;DOUBLE_T;DO_T;ELSE_T;ENUM_T;EQ_EQ_T;EQ_T;EXPORTS_T;
EXTENDS_T;FINALLY_T;FINAL_T;FLOAT_T;FOR_T;GE_T;GT_T;IF_T;IMPLEMENTS_T;IMPORT_T;INCR_T;INSTANCEOF_T;INTERFACE_T;
INT_T;LB_T;LC_T;LE_T;LP_T;LINEBREAK_T;LONG_T;LP_T;LS_T;LT_T;MINUS_T;MODULE_T;MOD_T;NATIVE_T;NEW_T;NONSEALED_T;
NOT_EQ_T;NOT_T;OPEN_T;OPENS_T;
OPERATOR_EQ_T;OR_OR_T;OR_T;PACKAGE_T;PERMITS_T;PLUS_T;PRIVATE_T;PROTECTED_T;PROVIDES_T;PUBLIC_T;RECORD_T;
REQUIRES_T;RETURN_T;RB_T;RC_T;RP_T;SEALED_T;SHORT_T;SM_T;SNAIL_T;SRS_T;STATIC_T;STRICTFP_T;SUPER_T;SWITCH_T;SYNCHRONIZED_T;
THIS_T;THROWS_T;THROW_T;TIMES_T;TO_T;TRANSIENT_T;TRANSITIVE_T;TRY_T;URS_T;USES_T;VAR_T;VOID_T;VOLATILE_T;WHILE_T;
WHITESPACE_T;WITH_T;XOR_T;YIELD_T] ;; 

let code_for_tokentype_in_production_names toktype = 
   let full_name = Private.ocaml_name_for_token_type toktype in 
   let short_name = Cull_string.two_sided_cutting ("","_T") full_name in 
   let parts = Str.split (Str.regexp_string "_") short_name in 
   let lowercase_parts = Image.image (fun part->String.capitalize_ascii(String.lowercase_ascii part)) parts in 
   String.concat "" lowercase_parts ;;
let code_for_tokentype_sequence_in_production_names seq =
   "Molecular"^(String.concat "_" (Image.image code_for_tokentype_in_production_names seq)) ;;
 
let extract_nonpassive_token_types postoks = 
    List.filter_map Private.extract_nonpassive_token_type_opt postoks ;;

let get_token_type = Private.get_token_type ;;


let has_variable_content = Private.has_variable_content ;;

let ocaml_name_for_token_type = Private.ocaml_name_for_token_type ;;
let passive_token_types = Private.passive_token_types ;;

let pretty_print_list_of_token_types toktypes =
   let temp = Image.image summary_of_token_type toktypes in 
   let text = String.concat " " temp in 
   let n = String.length text in 
   if n<=300
   then "\""^text^"\"" 
   else "\""^(Cull_string.beginning 300 text)^"(...)\"" ;;

let summary_of_token_type = Private.summary_of_token_type ;;

let token_type_from_ocaml_name name = 
  List.find (
   fun tok_type -> ocaml_name_for_token_type tok_type = name
  ) all_token_types ;;


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
|(WHITESPACE txt) -> txt 
|(LINEBREAK txt) -> txt;;






 
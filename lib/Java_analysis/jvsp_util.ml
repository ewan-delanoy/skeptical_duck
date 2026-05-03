(*

#use"lib/Java_analysis/jvsp_util.ml";;

*)

type token_type = Jvsp_types.token_type =
  IDENTIFIER_T 
| LITERAL_T  
| PRIMITIVE_TYPE_T 
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
|ANNOTATOR_T 
(* Keywords*)
|ABSTRACT_T |BOOLEAN_T |BREAK_T |BYTE_T |CASE_T |CATCH_T |CHAR_T |CLASS_T |CONST_T |CONTINUE_T
|DEFAULT_T |DO_T |DOUBLE_T |ELSE_T |EXTENDS_T |FINAL_T |FINALLY_T |FLOAT_T |FOR_T |GOTO_T
|IF_T |IMPLEMENTS_T |IMPORT_T |INSTANCEOF_T |INT_T |INTERFACE_T |LONG_T
|NATIVE_T |NEW_T |PACKAGE_T |PRIVATE_T |PROTECTED_T |PUBLIC_T |RETURN_T
|SHORT_T |STATIC_T |STRICTFP_T |SUPER_T |SWITCH_T |SYNCHRONIZED_T
|THIS_T |THROW_T |THROWS_T |TRANSIENT_T |TRY_T |VOID_T |VOLATILE_T |WHILE_T
|EOF_T  ;;

let table_for_reading_token_types = [
  (* Separators *)
("(",LP_T);
(")",RP_T);
("{",LC_T);
("}",RC_T);
("[",LB_T);
("]",RB_T);
(";",SM_T);
(",",CM_T);
(".",DOT_T);
(* Operators *)
("=",EQ_T);
(">",GT_T);
("<",LT_T);
("!",NOT_T);
("~",COMPL_T);
("?",COND_T);
(":",COLON_T);
("==",EQ_EQ_T);
("<=",LE_T);
(">=",GE_T);
("!=",NOT_EQ_T);
("&&",AND_AND_T);
("||",OR_OR_T);
("++",INCR_T);
("--",DECR_T);
("+",PLUS_T);
("-",MINUS_T);
("*",TIMES_T);
("/",DIV_T);
("&",AND_T);
("|",OR_T);
("^",XOR_T);
("%",MOD_T);
("<<",LS_T);
(">>",SRS_T);
(">>>",URS_T);
(* Keywords *)
("abstract",ABSTRACT_T);
("boolean",BOOLEAN_T);
("break",BREAK_T);
("byte",BYTE_T);
("case",CASE_T);
("catch",CATCH_T);
("char",CHAR_T);
("class",CLASS_T);
("const",CONST_T);
("continue",CONTINUE_T);
("default",DEFAULT_T);
("do",DO_T);
("double",DOUBLE_T);
("else",ELSE_T);
("extends",EXTENDS_T);
("final",FINAL_T);
("finally",FINALLY_T);
("float",FLOAT_T);
("for",FOR_T);
("goto",GOTO_T);
("if",IF_T);
("implements",IMPLEMENTS_T);
("import",IMPORT_T);
("instanceof",INSTANCEOF_T);
("int",INT_T);
("interface",INTERFACE_T);
("long",LONG_T);
("native",NATIVE_T);
("new",NEW_T);
("package",PACKAGE_T);
("private",PRIVATE_T);
("protected",PROTECTED_T);
("public",PUBLIC_T);
("return",RETURN_T);
("short",SHORT_T);
("static",STATIC_T);
("strictfp",STRICTFP_T);
("super",SUPER_T);
("switch",SWITCH_T);
("synchronized",SYNCHRONIZED_T);
("this",THIS_T);
("throw",THROW_T);
("throws",THROWS_T);
("transient",TRANSIENT_T);
("try",TRY_T);
("void",VOID_T);
("volatile",VOLATILE_T);
("while",WHILE_T)
] ;;


let ocaml_name_for_token_type = function 
|IDENTIFIER_T -> "IDENTIFIER_T"
|LITERAL_T -> "LITERAL_T"
|PRIMITIVE_TYPE_T -> "PRIMITIVE_TYPE_T"
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
|ANNOTATOR_T -> "ANNOTATOR_T"
|ABSTRACT_T -> "ABSTRACT_T"
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
|NATIVE_T -> "NATIVE_T"
|NEW_T -> "NEW_T"
|PACKAGE_T -> "PACKAGE_T"
|PRIVATE_T -> "PRIVATE_T"
|PROTECTED_T -> "PROTECTED_T"
|PUBLIC_T -> "PUBLIC_T"
|RETURN_T -> "RETURN_T"
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
|TRY_T -> "TRY_T"
|VOID_T -> "VOID_T"
|VOLATILE_T -> "VOLATILE_T"
|WHILE_T -> "WHILE_T"
|EOF_T -> "EOF_T" ;;


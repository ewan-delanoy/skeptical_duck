(*

#use"Padioleau/yp_php_token_t.ml";;

*)

type t = 
  EOF of Yp_parse_info.t
| LDots of Yp_parse_info.t
| RDots of Yp_parse_info.t
| TAND of Yp_parse_info.t
| TANTISLASH of Yp_parse_info.t
| TBACKQUOTE of Yp_parse_info.t
| TBANG of Yp_parse_info.t
| TCBRA of Yp_parse_info.t
| TCBRACE of Yp_parse_info.t
| TCommentPP of Yp_parse_info.t
| TCOLCOL of Yp_parse_info.t
| TCOLON of Yp_parse_info.t
| TCOMMA of Yp_parse_info.t
| TCPAR of Yp_parse_info.t
| TDIV of Yp_parse_info.t
| TDOLLAR of Yp_parse_info.t
| TDOLLARDOLLAR of Yp_parse_info.t
| TDOT of Yp_parse_info.t
| TEQ of Yp_parse_info.t
| TGREATER of Yp_parse_info.t
| TGUIL of Yp_parse_info.t
| TMINUS of Yp_parse_info.t
| TMOD of Yp_parse_info.t
| TMUL of Yp_parse_info.t
| TNewline of Yp_parse_info.t
| TOATTR of Yp_parse_info.t
| TOBRA of Yp_parse_info.t
| TOBRACE of Yp_parse_info.t
| TOPAR of Yp_parse_info.t
| TOR of Yp_parse_info.t
| TPLUS of Yp_parse_info.t
| TPOW of Yp_parse_info.t
| TQUESTION of Yp_parse_info.t
| TSEMICOLON of Yp_parse_info.t
| TSMALLER of Yp_parse_info.t
| TSpaces of Yp_parse_info.t
| TTILDE of Yp_parse_info.t
| TUnknown of Yp_parse_info.t
| TXOR of Yp_parse_info.t
| T_ABSTRACT of Yp_parse_info.t
| T_AND_EQUAL of Yp_parse_info.t
| T_ARRAY of Yp_parse_info.t
| T_ARRAY_CAST of Yp_parse_info.t
| T_ARROW of Yp_parse_info.t
| T_AS of Yp_parse_info.t
| T_ASYNC of Yp_parse_info.t
| T_AWAIT of Yp_parse_info.t
| T_BOOL of bool * Yp_parse_info.t
| T_BOOLEAN_AND of Yp_parse_info.t
| T_BOOLEAN_OR of Yp_parse_info.t
| T_BOOLEAN_PIPE of Yp_parse_info.t
| T_BOOL_CAST of Yp_parse_info.t
| T_BREAK of Yp_parse_info.t
| T_CASE of Yp_parse_info.t
| T_CATCH of Yp_parse_info.t
| T_CLASS of Yp_parse_info.t
| T_CLASS_C of Yp_parse_info.t
| T_CLONE of Yp_parse_info.t
| T_CLOSE_TAG of Yp_parse_info.t
| T_CLOSE_TAG_OF_ECHO of Yp_parse_info.t
| T_COMMENT of Yp_parse_info.t
| T_CONCAT_EQUAL of Yp_parse_info.t
| T_CONST of Yp_parse_info.t
| T_CONSTANT_ENCAPSED_STRING of string * Yp_parse_info.t
| T_CONTINUE of Yp_parse_info.t
| T_CURLY_OPEN of Yp_parse_info.t
| T_DEC of Yp_parse_info.t
| T_DECLARE of Yp_parse_info.t
| T_DEFAULT of Yp_parse_info.t
| T_DIR of Yp_parse_info.t
| T_DIV_EQUAL of Yp_parse_info.t
| T_DNUMBER of float option * Yp_parse_info.t
| T_DO of Yp_parse_info.t
| T_DOC_COMMENT of Yp_parse_info.t
| T_DOLLAR_OPEN_CURLY_BRACES of Yp_parse_info.t
| T_DOUBLE_ARROW of Yp_parse_info.t
| T_DOUBLE_CAST of Yp_parse_info.t
| T_ECHO of Yp_parse_info.t
| T_ELLIPSIS of Yp_parse_info.t
| T_ELSE of Yp_parse_info.t
| T_ELSEIF of Yp_parse_info.t
| T_EMPTY of Yp_parse_info.t
| T_ENCAPSED_AND_WHITESPACE of string * Yp_parse_info.t
| T_ENDDECLARE of Yp_parse_info.t
| T_ENDFOR of Yp_parse_info.t
| T_ENDFOREACH of Yp_parse_info.t
| T_ENDIF of Yp_parse_info.t
| T_ENDSWITCH of Yp_parse_info.t
| T_ENDWHILE of Yp_parse_info.t
| T_END_HEREDOC of Yp_parse_info.t
| T_ENUM of Yp_parse_info.t
| T_EVAL of Yp_parse_info.t
| T_EXIT of Yp_parse_info.t
| T_EXTENDS of Yp_parse_info.t
| T_FILE of Yp_parse_info.t
| T_FINAL of Yp_parse_info.t
| T_FINALLY of Yp_parse_info.t
| T_FOR of Yp_parse_info.t
| T_FOREACH of Yp_parse_info.t
| T_FROM of Yp_parse_info.t
| T_FUNCTION of Yp_parse_info.t
| T_FUNC_C of Yp_parse_info.t
| T_GLOBAL of Yp_parse_info.t
| T_GOTO of Yp_parse_info.t
| T_IDENT of string * Yp_parse_info.t
| T_IF of Yp_parse_info.t
| T_IMPLEMENTS of Yp_parse_info.t
| T_INC of Yp_parse_info.t
| T_INCLUDE of Yp_parse_info.t
| T_INCLUDE_ONCE of Yp_parse_info.t
| T_INLINE_HTML of string * Yp_parse_info.t
| T_INSTANCEOF of Yp_parse_info.t
| T_INSTEADOF of Yp_parse_info.t
| T_INTERFACE of Yp_parse_info.t
| T_INT_CAST of Yp_parse_info.t
| T_ISSET of Yp_parse_info.t
| T_IS_EQUAL of Yp_parse_info.t
| T_IS_GREATER_OR_EQUAL of Yp_parse_info.t
| T_IS_IDENTICAL of Yp_parse_info.t
| T_IS_NOT_EQUAL of Yp_parse_info.t
| T_IS_NOT_IDENTICAL of Yp_parse_info.t
| T_IS_SMALLER_OR_EQUAL of Yp_parse_info.t
| T_LAMBDA_CPAR of Yp_parse_info.t
| T_LAMBDA_OPAR of Yp_parse_info.t
| T_LINE of Yp_parse_info.t
| T_LIST of Yp_parse_info.t
| T_LNUMBER of int option * Yp_parse_info.t
| T_LOGICAL_AND of Yp_parse_info.t
| T_LOGICAL_OR of Yp_parse_info.t
| T_LOGICAL_XOR of Yp_parse_info.t
| T_METAVAR of string * Yp_parse_info.t
| T_METHOD_C of Yp_parse_info.t
| T_MINUS_EQUAL of Yp_parse_info.t
| T_MOD_EQUAL of Yp_parse_info.t
| T_MUL_EQUAL of Yp_parse_info.t
| T_NAMESPACE of Yp_parse_info.t
| T_NAMESPACE_C of Yp_parse_info.t
| T_NEW of Yp_parse_info.t
| T_NUM_STRING of string * Yp_parse_info.t
| T_OBJECT_CAST of Yp_parse_info.t
| T_OBJECT_OPERATOR of Yp_parse_info.t
| T_OPEN_TAG of Yp_parse_info.t
| T_OPEN_TAG_WITH_ECHO of Yp_parse_info.t
| T_OR_EQUAL of Yp_parse_info.t
| T_PARENT of Yp_parse_info.t
| T_PLUS_EQUAL of Yp_parse_info.t
| T_PRINT of Yp_parse_info.t
| T_PRIVATE of Yp_parse_info.t
| T_PROTECTED of Yp_parse_info.t
| T_PUBLIC of Yp_parse_info.t
| T_REQUIRE of Yp_parse_info.t
| T_REQUIRE_ONCE of Yp_parse_info.t
| T_RETURN of Yp_parse_info.t
| T_ROCKET of Yp_parse_info.t
| T_SELF of Yp_parse_info.t
| T_SL of Yp_parse_info.t
| T_SL_EQUAL of Yp_parse_info.t
| T_SR of Yp_parse_info.t
| T_SR_EQUAL of Yp_parse_info.t
| T_START_HEREDOC of Yp_parse_info.t
| T_STATIC of Yp_parse_info.t
| T_STRING_CAST of Yp_parse_info.t
| T_STRING_VARNAME of string * Yp_parse_info.t
| T_SUPER of Yp_parse_info.t
| T_SWITCH of Yp_parse_info.t
| T_THROW of Yp_parse_info.t
| T_TRAIT of Yp_parse_info.t
| T_TRAIT_C of Yp_parse_info.t
| T_TRY of Yp_parse_info.t
| T_TYPE of Yp_parse_info.t
| T_UNSET of Yp_parse_info.t
| T_UNSET_CAST of Yp_parse_info.t
| T_USE of Yp_parse_info.t
| T_VAR of Yp_parse_info.t
| T_VARIABLE of string * Yp_parse_info.t
| T_WHILE of Yp_parse_info.t
| T_XOR_EQUAL of Yp_parse_info.t
| T_YIELD of Yp_parse_info.t
| T__AT of Yp_parse_info.t ;;

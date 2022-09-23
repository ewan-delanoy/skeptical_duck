(*

#use"Padioleau/yp_php_token_t.ml";;

*)

type t = 
  EOF of Yp_token_info_t.t
| LDots of Yp_token_info_t.t
| RDots of Yp_token_info_t.t
| TAND of Yp_token_info_t.t
| TANTISLASH of Yp_token_info_t.t
| TBACKQUOTE of Yp_token_info_t.t
| TBANG of Yp_token_info_t.t
| TCBRA of Yp_token_info_t.t
| TCBRACE of Yp_token_info_t.t
| TCommentPP of Yp_token_info_t.t
| TCOLCOL of Yp_token_info_t.t
| TCOLON of Yp_token_info_t.t
| TCOMMA of Yp_token_info_t.t
| TCPAR of Yp_token_info_t.t
| TDIV of Yp_token_info_t.t
| TDOLLAR of Yp_token_info_t.t
| TDOLLARDOLLAR of Yp_token_info_t.t
| TDOT of Yp_token_info_t.t
| TEQ of Yp_token_info_t.t
| TGREATER of Yp_token_info_t.t
| TGUIL of Yp_token_info_t.t
| TMINUS of Yp_token_info_t.t
| TMOD of Yp_token_info_t.t
| TMUL of Yp_token_info_t.t
| TNewline of Yp_token_info_t.t
| TOATTR of Yp_token_info_t.t
| TOBRA of Yp_token_info_t.t
| TOBRACE of Yp_token_info_t.t
| TOPAR of Yp_token_info_t.t
| TOR of Yp_token_info_t.t
| TPLUS of Yp_token_info_t.t
| TPOW of Yp_token_info_t.t
| TQUESTION of Yp_token_info_t.t
| TSEMICOLON of Yp_token_info_t.t
| TSMALLER of Yp_token_info_t.t
| TSpaces of Yp_token_info_t.t
| TTILDE of Yp_token_info_t.t
| TUnknown of Yp_token_info_t.t
| TXOR of Yp_token_info_t.t
| T_ABSTRACT of Yp_token_info_t.t
| T_AND_EQUAL of Yp_token_info_t.t
| T_ARRAY of Yp_token_info_t.t
| T_ARRAY_CAST of Yp_token_info_t.t
| T_ARROW of Yp_token_info_t.t
| T_AS of Yp_token_info_t.t
| T_ASYNC of Yp_token_info_t.t
| T_AWAIT of Yp_token_info_t.t
| T_BOOL of bool * Yp_token_info_t.t
| T_BOOLEAN_AND of Yp_token_info_t.t
| T_BOOLEAN_OR of Yp_token_info_t.t
| T_BOOLEAN_PIPE of Yp_token_info_t.t
| T_BOOL_CAST of Yp_token_info_t.t
| T_BREAK of Yp_token_info_t.t
| T_CASE of Yp_token_info_t.t
| T_CATCH of Yp_token_info_t.t
| T_CLASS of Yp_token_info_t.t
| T_CLASS_C of Yp_token_info_t.t
| T_CLONE of Yp_token_info_t.t
| T_CLOSE_TAG of Yp_token_info_t.t
| T_CLOSE_TAG_OF_ECHO of Yp_token_info_t.t
| T_COMMENT of Yp_token_info_t.t
| T_CONCAT_EQUAL of Yp_token_info_t.t
| T_CONST of Yp_token_info_t.t
| T_CONSTANT_ENCAPSED_STRING of string * Yp_token_info_t.t
| T_CONTINUE of Yp_token_info_t.t
| T_CURLY_OPEN of Yp_token_info_t.t
| T_DEC of Yp_token_info_t.t
| T_DECLARE of Yp_token_info_t.t
| T_DEFAULT of Yp_token_info_t.t
| T_DIR of Yp_token_info_t.t
| T_DIV_EQUAL of Yp_token_info_t.t
| T_DNUMBER of float option * Yp_token_info_t.t
| T_DO of Yp_token_info_t.t
| T_DOC_COMMENT of Yp_token_info_t.t
| T_DOLLAR_OPEN_CURLY_BRACES of Yp_token_info_t.t
| T_DOUBLE_ARROW of Yp_token_info_t.t
| T_DOUBLE_CAST of Yp_token_info_t.t
| T_ECHO of Yp_token_info_t.t
| T_ELLIPSIS of Yp_token_info_t.t
| T_ELSE of Yp_token_info_t.t
| T_ELSEIF of Yp_token_info_t.t
| T_EMPTY of Yp_token_info_t.t
| T_ENCAPSED_AND_WHITESPACE of string * Yp_token_info_t.t
| T_ENDDECLARE of Yp_token_info_t.t
| T_ENDFOR of Yp_token_info_t.t
| T_ENDFOREACH of Yp_token_info_t.t
| T_ENDIF of Yp_token_info_t.t
| T_ENDSWITCH of Yp_token_info_t.t
| T_ENDWHILE of Yp_token_info_t.t
| T_END_HEREDOC of Yp_token_info_t.t
| T_ENUM of Yp_token_info_t.t
| T_EVAL of Yp_token_info_t.t
| T_EXIT of Yp_token_info_t.t
| T_EXTENDS of Yp_token_info_t.t
| T_FILE of Yp_token_info_t.t
| T_FINAL of Yp_token_info_t.t
| T_FINALLY of Yp_token_info_t.t
| T_FOR of Yp_token_info_t.t
| T_FOREACH of Yp_token_info_t.t
| T_FROM of Yp_token_info_t.t
| T_FUNCTION of Yp_token_info_t.t
| T_FUNC_C of Yp_token_info_t.t
| T_GLOBAL of Yp_token_info_t.t
| T_GOTO of Yp_token_info_t.t
| T_IDENT of string * Yp_token_info_t.t
| T_IF of Yp_token_info_t.t
| T_IMPLEMENTS of Yp_token_info_t.t
| T_INC of Yp_token_info_t.t
| T_INCLUDE of Yp_token_info_t.t
| T_INCLUDE_ONCE of Yp_token_info_t.t
| T_INLINE_HTML of string * Yp_token_info_t.t
| T_INSTANCEOF of Yp_token_info_t.t
| T_INSTEADOF of Yp_token_info_t.t
| T_INTERFACE of Yp_token_info_t.t
| T_INT_CAST of Yp_token_info_t.t
| T_ISSET of Yp_token_info_t.t
| T_IS_EQUAL of Yp_token_info_t.t
| T_IS_GREATER_OR_EQUAL of Yp_token_info_t.t
| T_IS_IDENTICAL of Yp_token_info_t.t
| T_IS_NOT_EQUAL of Yp_token_info_t.t
| T_IS_NOT_IDENTICAL of Yp_token_info_t.t
| T_IS_SMALLER_OR_EQUAL of Yp_token_info_t.t
| T_LAMBDA_CPAR of Yp_token_info_t.t
| T_LAMBDA_OPAR of Yp_token_info_t.t
| T_LINE of Yp_token_info_t.t
| T_LIST of Yp_token_info_t.t
| T_LNUMBER of int option * Yp_token_info_t.t
| T_LOGICAL_AND of Yp_token_info_t.t
| T_LOGICAL_OR of Yp_token_info_t.t
| T_LOGICAL_XOR of Yp_token_info_t.t
| T_METAVAR of string * Yp_token_info_t.t
| T_METHOD_C of Yp_token_info_t.t
| T_MINUS_EQUAL of Yp_token_info_t.t
| T_MOD_EQUAL of Yp_token_info_t.t
| T_MUL_EQUAL of Yp_token_info_t.t
| T_NAMESPACE of Yp_token_info_t.t
| T_NAMESPACE_C of Yp_token_info_t.t
| T_NEW of Yp_token_info_t.t
| T_NUM_STRING of string * Yp_token_info_t.t
| T_OBJECT_CAST of Yp_token_info_t.t
| T_OBJECT_OPERATOR of Yp_token_info_t.t
| T_OPEN_TAG of Yp_token_info_t.t
| T_OPEN_TAG_WITH_ECHO of Yp_token_info_t.t
| T_OR_EQUAL of Yp_token_info_t.t
| T_PARENT of Yp_token_info_t.t
| T_PLUS_EQUAL of Yp_token_info_t.t
| T_PRINT of Yp_token_info_t.t
| T_PRIVATE of Yp_token_info_t.t
| T_PROTECTED of Yp_token_info_t.t
| T_PUBLIC of Yp_token_info_t.t
| T_REQUIRE of Yp_token_info_t.t
| T_REQUIRE_ONCE of Yp_token_info_t.t
| T_RETURN of Yp_token_info_t.t
| T_ROCKET of Yp_token_info_t.t
| T_SELF of Yp_token_info_t.t
| T_SL of Yp_token_info_t.t
| T_SL_EQUAL of Yp_token_info_t.t
| T_SR of Yp_token_info_t.t
| T_SR_EQUAL of Yp_token_info_t.t
| T_START_HEREDOC of Yp_token_info_t.t
| T_STATIC of Yp_token_info_t.t
| T_STRING_CAST of Yp_token_info_t.t
| T_STRING_VARNAME of string * Yp_token_info_t.t
| T_SUPER of Yp_token_info_t.t
| T_SWITCH of Yp_token_info_t.t
| T_THROW of Yp_token_info_t.t
| T_TRAIT of Yp_token_info_t.t
| T_TRAIT_C of Yp_token_info_t.t
| T_TRY of Yp_token_info_t.t
| T_TYPE of Yp_token_info_t.t
| T_UNSET of Yp_token_info_t.t
| T_UNSET_CAST of Yp_token_info_t.t
| T_USE of Yp_token_info_t.t
| T_VAR of Yp_token_info_t.t
| T_VARIABLE of string * Yp_token_info_t.t
| T_WHILE of Yp_token_info_t.t
| T_XOR_EQUAL of Yp_token_info_t.t
| T_YIELD of Yp_token_info_t.t
| T__AT of Yp_token_info_t.t ;;

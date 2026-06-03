(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_ladder_finder_example.ml";;

*)

module T = Jvsp_types ;;

exception Malformed_conditional_expression of T.token_type list ;;

module Private = struct 

  let cons_opt x = Option.map (fun l->x::l) ;;

  let left_to_right_associative current_name left_handler separators right_handler name_for_decomposed other_case l=
   match List_again.find_pattern_and_remember_opt separators l with 
   None -> cons_opt current_name (other_case l)
   |Some (before,_,after) -> 
     if ((left_handler before)<>None)&&
        ((right_handler after)<>None)
     then Some [current_name;name_for_decomposed]
     else None ;;

  let right_to_left_associative current_name left_handler separators right_handler name_for_decomposed other_case l=
   match List_again.find_pattern_and_remember_opt separators l with 
   None -> cons_opt current_name (other_case l)
   |Some (before,_,after) -> 
     if ((left_handler before)<>None)&&
        ((right_handler after)<>None)
     then Some [current_name;name_for_decomposed]
     else None ;;

  let rec recognize_StarredMolecularDot_Identifier l =
  match l with 
  [] -> true 
  |tok1 :: others1 ->
    if tok1<>T.DOT_T then false else 
    match others1 with 
     [] -> false 
    |tok2 :: others2 -> 
      if tok2<>T.IDENTIFIER_T then false else 
      recognize_StarredMolecularDot_Identifier others2 ;;

let recognize_ExpressionName l =
  match l with 
  [] -> false 
  |tok1 :: others1 ->
    if tok1<>T.IDENTIFIER_T then false else
    recognize_StarredMolecularDot_Identifier others1 ;;

   let ladder_for_Primary_opt _l = None ;;
   let ladder_for_ExpressionName_opt l = 
      if recognize_ExpressionName l 
      then Some["ExpressionName"]
      else None      ;;
   let ladder_for_PostIncrementExpression_opt _l = None ;;
     
   let ladder_for_PostDecrementExpression_opt _l = None ;; 

   let ladder_for_PostfixExpression_opt l= List.find_map (
     fun f-> f l
   ) [
     ladder_for_Primary_opt;
     ladder_for_ExpressionName_opt;
     ladder_for_PostIncrementExpression_opt;
     ladder_for_PostDecrementExpression_opt;
   ] ;; 

   let ladder_for_ParenthesedUnaryExpressionNotPlusMinus_opt _l = None ;; 

   let ladder_for_CastExpression_opt _l = None ;; 

   let ladder_for_UnaryExpressionIII_opt _l = None ;; 

   let ladder_for_UnaryExpressionNotPlusMinus_opt l=match l with 
    [] -> None 
    |toktype :: other_toktypes ->
      if toktype = T.COMPL_T then ladder_for_UnaryExpressionIII_opt other_toktypes else 
      if toktype = T.NOT_T then ladder_for_UnaryExpressionIII_opt other_toktypes else   
      if toktype = T.LP_T then cons_opt "UnaryExpressionNotPlusMinus" (ladder_for_CastExpression_opt l) else  
      if toktype = T.SWITCH_T then cons_opt "UnaryExpressionNotPlusMinus" (ladder_for_ParenthesedUnaryExpressionNotPlusMinus_opt l) else   
      (*        
      match toktype with 
      T.COMPL_T -> ladder_for_UnaryExpressionIII_opt other_toktypes
      |T.NOT_T -> ladder_for_UnaryExpressionIII_opt other_toktypes 
      |T.LP_T -> cons_opt "UnaryExpressionNotPlusMinus" (ladder_for_CastExpression_opt l)
      |T.SWITCH_T -> cons_opt "UnaryExpressionNotPlusMinus" (ladder_for_ParenthesedUnaryExpressionNotPlusMinus_opt l)

      |T.ABSTRACT_T|T.AND_T|T.AND_AND_T|T.ASSERT_T|T.BOOLEAN_T|T.BREAK_T|T.BYTE_T|T.CASE_T|T.CATCH_T|T.CHAR_T|T.CLASS_T|T.CM_T
|T.COLON_T|T.COMMENT_T|T.COND_T|T.CONTINUE_T|T.DECR_T|T.DEFAULT_T|T.DIV_T|T.DO_T|T.DOT_T|T.DOUBLE_T|T.ELSE_T
|T.ENUM_T|T.EQ_T|T.EQ_EQ_T|T.EXPORTS_T|T.EXTENDS_T|T.FINAL_T|T.FINALLY_T|T.FLOAT_T|T.FOR_T|T.GE_T|T.GT_T|T.IDENTIFIER_T
|T.IF_T|T.IMPLEMENTS_T|T.IMPORT_T|T.INCR_T|T.INSTANCEOF_T|T.INT_T|T.INTERFACE_T|T.LB_T|T.LC_T|T.LE_T|T.LINEBREAK_T
|T.LONG_T|T.LS_T|T.LT_T|T.MINUS_T|T.MOD_T|T.MODULE_T|T.NATIVE_T|T.NEW_T|T.NONSEALED_T|T.NOT_EQ_T|T.OPEN_T
|T.OPENS_T|T.OPERATOR_EQ_T|T.OR_T|T.OR_OR_T|T.PACKAGE_T|T.PERMITS_T|T.PLUS_T|T.PRIVATE_T|T.PROTECTED_T|T.PROVIDES_T
|T.PUBLIC_T|T.RB_T|T.RC_T|T.RECORD_T|T.REQUIRES_T|T.RETURN_T|T.RP_T|T.SEALED_T|T.SHORT_T|T.SM_T|T.SNAIL_T|T.SRS_T|T.STATIC_T
|T.STRICTFP_T|T.SUPER_T|T.SYNCHRONIZED_T|T.THIS_T|T.THROW_T|T.THROWS_T|T.TIMES_T|T.TO_T|T.TRANSIENT_T
|T.TRANSITIVE_T|T.TRY_T|T.URS_T|T.USES_T|T.VAR_T|T.VOID_T|T.VOLATILE_T|T.WHILE_T|T.WHITESPACE_T|T.WITH_T|T.XOR_T|T.YIELD_T ->
     
   *)
   cons_opt "UnaryExpressionNotPlusMinus" (ladder_for_PostfixExpression_opt l)  ;;


   let ladder_for_UnaryExpressionII_opt _l = None ;; 

   let ladder_for_UnaryExpression_opt l=match l with 
    [] -> None 
    |toktype :: other_toktypes ->
      if List.mem toktype [T.DECR_T;T.INCR_T;T.MINUS_T;T.PLUS_T]
      then ladder_for_UnaryExpressionII_opt other_toktypes 
      else cons_opt "UnaryExpression" (ladder_for_UnaryExpressionNotPlusMinus_opt l)  ;;

   let ladder_for_MultiplicativeExpressionII_opt _l = None ;; 

    let ladder_for_MultiplicativeExpression_opt =
   left_to_right_associative 
    "RelationalExpression" 
     ladder_for_MultiplicativeExpressionII_opt [[T.DIV_T];[T.MOD_T];[T.TIMES_T]] ladder_for_UnaryExpression_opt 
      "UnaryExpression"  
       ladder_for_UnaryExpression_opt  ;;



   let ladder_for_AdditiveExpressionII_opt _l = None ;; 

   let ladder_for_AdditiveExpression_opt =
   left_to_right_associative 
    "AdditiveExpression" 
     ladder_for_AdditiveExpressionII_opt [[T.MINUS_T];[T.PLUS_T]] ladder_for_MultiplicativeExpression_opt 
      "MultiplicativeExpression"  
       ladder_for_MultiplicativeExpression_opt  ;; 

   let ladder_for_ShiftExpressionII_opt _l = None ;; 

   let ladder_for_ShiftExpression_opt =
   left_to_right_associative 
    "ShiftExpression" 
     ladder_for_ShiftExpressionII_opt [[T.LS_T];[T.SRS_T];[T.URS_T];] ladder_for_AdditiveExpression_opt 
      "AdditiveExpression"  
       ladder_for_AdditiveExpression_opt  ;;

   let ladder_for_RelationalExpressionII_opt _l = None ;; 

   let ladder_for_RelationalExpression_opt =
   left_to_right_associative 
    "RelationalExpression" 
     ladder_for_RelationalExpressionII_opt [[T.LE_T];[T.LT_T];[T.GE_T];[T.GT_T];[T.INSTANCEOF_T]] ladder_for_ShiftExpression_opt 
      "ShiftExpression"  
       ladder_for_ShiftExpression_opt  ;;

   let ladder_for_EqualityExpressionII_opt _l = None ;;  

   let ladder_for_EqualityExpression_opt =
   left_to_right_associative 
    "EqualityExpression" 
     ladder_for_EqualityExpressionII_opt [[T.EQ_EQ_T];[T.NOT_EQ_T]] ladder_for_RelationalExpression_opt 
      "ConditionalAndExpression"  
       ladder_for_RelationalExpression_opt  ;;

   let ladder_for_AndExpressionII_opt _l = None ;; 

   let ladder_for_AndExpression_opt =
   left_to_right_associative 
    "AndExpression" 
     ladder_for_AndExpressionII_opt [[T.AND_T]] ladder_for_EqualityExpression_opt 
      "EqualityExpression"  
       ladder_for_EqualityExpression_opt  ;;

   let ladder_for_ExclusiveExpressionII_opt _l = None ;; 

   let ladder_for_ExclusiveOrExpression_opt =
   left_to_right_associative 
    "ExclusiveOrExpression" 
     ladder_for_ExclusiveExpressionII_opt [[T.XOR_T]] ladder_for_AndExpression_opt 
      "ConditionalAndExpression"  
       ladder_for_AndExpression_opt  ;;

  let ladder_for_InclusiveExpressionII_opt _l = None ;; 

  let ladder_for_InclusiveOrExpression_opt =
   left_to_right_associative 
    "InclusiveOrExpression" 
     ladder_for_InclusiveExpressionII_opt [[T.OR_T]] ladder_for_ExclusiveOrExpression_opt 
      "ExclusiveOrExpression"  
       ladder_for_ExclusiveOrExpression_opt  ;;

  let ladder_for_ConditionalAndExpressionII_opt _l = None ;; 
    
  let ladder_for_ConditionalAndExpression_opt =
   left_to_right_associative 
    "ConditionalAndExpression" 
     ladder_for_ConditionalAndExpressionII_opt [[T.AND_AND_T]] ladder_for_InclusiveOrExpression_opt 
      "ConditionalAndExpression"  
       ladder_for_InclusiveOrExpression_opt  ;;
    
  let ladder_for_ConditionalOrExpressionII_opt _l = None ;;

  let  ladder_for_ConditionalOrExpression_opt =
   left_to_right_associative 
    "ConditionalOrExpression" 
     ladder_for_ConditionalOrExpressionII_opt [[T.OR_OR_T]] ladder_for_ConditionalAndExpression_opt 
      "ConditionalAndExpression"  
       ladder_for_ConditionalAndExpression_opt  ;;

  let ladder_for_ExpressionIII_opt _l = None ;;

  let ladder_for_ConditionalExpressionII_opt _l = None ;;

  let ladder_for_LambdaExpression_opt _l = None ;;

 let ladder_for_ConditionalExpression_opt  l=
 let current_name = "ConditionalExpression" in
   match List_again.find_interval_sublist_and_remember_opt [T.COND_T] l with 
   None -> cons_opt current_name (ladder_for_ConditionalOrExpression_opt l)
   |Some (before1,after1) ->
      match List_again.find_interval_sublist_and_remember_opt [T.COLON_T] after1 with 
       None -> raise(Malformed_conditional_expression(l))
      |Some (before2,after2) ->   
     if ((ladder_for_ConditionalOrExpression_opt before1)=None)||
        ((ladder_for_ExpressionIII_opt before2)=None)
     then None 
     else 
     if (ladder_for_ConditionalExpressionII_opt after2)<>None 
     then Some [current_name;"LambdalessConditionalAndExpressionConditionalExpression"]
     else 
      if (ladder_for_LambdaExpression_opt after2)<>None 
     then Some [current_name;"LambdafulConditionalAndExpressionConditionalExpression"]
     else None ;;

let ladder_for_LeftHandSide_opt _l = None ;;   

let ladder_for_ExpressionII_opt _l = None ;;  


let ladder_for_AssignmentExpression_opt =
   right_to_left_associative 
    "AssignmentExpression" 
     ladder_for_LeftHandSide_opt [[T.OPERATOR_EQ_T]] ladder_for_ExpressionII_opt 
      "Assignment"  
       ladder_for_ConditionalExpression_opt ;;


let ladder_for_LambdaParameters_opt _l = None ;;   

let ladder_for_LambdaBody_opt _l = None ;;  

let ladder_for_Expression_opt =
   right_to_left_associative 
    "Expression" 
     ladder_for_LambdaParameters_opt [[T.MINUS_T;T.GT_T]] ladder_for_LambdaBody_opt 
      "LambdaExpression"  
       ladder_for_AssignmentExpression_opt ;;
   
  
    
let initial_Expression_followed_by_Semicolon_opt abstract_l =
   let l = Jvsp_token_types_list.unveil abstract_l in 
   match List_again.find_and_remember_opt (fun tok->tok=T.SM_T) l with 
   None -> None
   |(Some(start,_,_)) -> 
    Option.map (Image.image Jvng_duplicated_name.of_string)
    (ladder_for_Expression_opt start) ;;


let example =   Jvng_disjunction_ladder_finder.make [
   initial_Expression_followed_by_Semicolon_opt
] ;;

end ;;

let example = Private.example ;;
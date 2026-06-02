(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_ladder_list_example.ml";;

*)

module T = Jvsp_types ;;

module Private = struct 

exception Not_defined_yet of string * (Jvsp_types.token_type list);;

let realize_as_Expression _ = () ;;

List_again.find_and_remember_opt ;;


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
    
let detect_ExpressionName_followed_by_Semicolon abstract_l =
   let l = Jvsp_token_types_list.unveil abstract_l in 
   match List_again.find_and_remember_opt (fun tok->tok=T.SM_T) l with 
   None -> false 
   |(Some(start,_,_)) -> recognize_ExpressionName start ;;

let example = Jvng_disjunction_ladder_list.make  [
   detect_ExpressionName_followed_by_Semicolon,[
    "Expression";"AssignmentExpression";"ConditionalExpression";"ConditionalOrExpression";"ConditionalAndExpression";
    "InclusiveOrExpression";"ExclusiveOrExpression";"AndExpression";"EqualityExpression";"RelationalExpression";
    "ShiftExpression";"AdditiveExpression";"MultiplicativeExpression";"UnaryExpression";"UnaryExpressionNotPlusMinus";
    "PostfixExpression";"ExpressionName"
   ]
] ;;

end ;;

let example = Private.example ;;
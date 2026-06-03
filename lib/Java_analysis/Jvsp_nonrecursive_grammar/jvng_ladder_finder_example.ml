(*

#use"lib/Java_analysis/Jvsp_nonrecursive_grammar/jvng_ladder_finder_example.ml";;

*)

module T = Jvsp_types ;;

exception Malformed_conditional_expression of T.token_type list ;;

module Private = struct 

  let cons_opt x = Option.map (fun l->x::l) ;;

  let usual_combination current_name left_handler separator right_handler name_for_decomposed other_case l=
   match List_again.find_interval_sublist_and_remember_opt separator l with 
   None -> cons_opt current_name (other_case l)
   |Some (before,after) -> 
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

  let  ladder_for_ConditionalOrExpression_opt _l = None ;;

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
   usual_combination 
    "AssignmentExpression" 
     ladder_for_LeftHandSide_opt [T.OPERATOR_EQ_T] ladder_for_ExpressionII_opt 
      "Assignment"  
       ladder_for_ConditionalExpression_opt ;;


let ladder_for_LambdaParameters_opt _l = None ;;   

let ladder_for_LambdaBody_opt _l = None ;;  

let ladder_for_Expression_opt =
   usual_combination 
    "Expression" 
     ladder_for_LambdaParameters_opt [T.MINUS_T;T.GT_T] ladder_for_LambdaBody_opt 
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
(*

#use "Text_editing/Text_lengthening/txl_apply.ml";;

*)


exception Nonregistered_short_form of string;;

module Private = struct

let expand_prefix txl word=
   let opt = Option.seek(fun (x,y)->Supstring.begins_with word x)
   (Txl_field.prefix_abbreviations txl) in 
   match opt with 
   None->(opt,word)
  |Some(x0,y0)->
    let shorter_word=Cull_string.cobeginning (String.length x0) word in 
    (opt,y0^shorter_word);;

let expand_left_core txl abbrv=
   let opt = Option.seek(fun (x,y)->y=abbrv) (Txl_field.left_core_abbreviations txl) in 
   match opt with 
   None->(opt,abbrv)
   |Some(full_word,_)->(opt,full_word);;


let apply_decompression txl (accu,left_part) short_form= 
   match Option.seek(
     fun (p,_,_)->p=short_form
   )(Txl_field.adjustable_decompressions txl) with
   None->raise(Nonregistered_short_form(short_form))
   |Some(_,long_form,special_cases)->
       (
         match Option.seek(
           fun (x,rarely_used,y)->y=left_part
         )(special_cases) with
         None->(accu,left_part^long_form)
         |Some(corrected_beginning,rarely_used,_)->
           let (correct_ending,new_accu)=(
             if rarely_used="" then (long_form,accu) else (rarely_used,rarely_used::accu)
           ) in 
           (new_accu,corrected_beginning^correct_ending)
       );;

let apply_decrompressions txl left_part l=
    List.fold_left (apply_decompression txl) ([],left_part)  l;;

let apply_decompressions_if_allowed txl word l=
   let whole_suffix=String.concat "" l in 
   if Supstring.ends_with word whole_suffix
   then let left_part=Cull_string.coending (String.length whole_suffix) word in 
        Some(apply_decrompressions txl left_part l)
   else None;;

let apply_disjunction_of_expansions txl word=
  let opt = Option.find_and_stop(apply_decompressions_if_allowed txl word) 
    (Txl_field.expansions txl) in 
  match opt with 
  None->(opt,[],word)
  |Some(remembered_adjustments,result)->(opt,remembered_adjustments,result);;


let expand_all txl word =
   if List.mem word (Txl_field.inert_words txl) 
   then Txl_result_t.Builtin_inert(word) 
   else 
   if Supstring.begins_with word "\\"
   then Txl_result_t.Declared_inert(Cull_string.cobeginning 1 word)
   else 
   let (opt1,res1)=expand_prefix txl word in 
   let (opt2,res2)=expand_left_core txl res1 in
   let (opt3,adjs,res3)=apply_disjunction_of_expansions txl res2  in 
   Txl_result_t.Usual(opt1,opt2,opt3,adjs,res3);;


end;;

let apply_slowly = Private.expand_all;;

let apply txl word=match apply_slowly txl word with 
 (Txl_result_t.Builtin_inert answer)->answer
 |(Txl_result_t.Declared_inert answer)->answer
 |(Txl_result_t.Usual(_,_,_,_,answer))->answer;;
 
   

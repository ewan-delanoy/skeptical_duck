(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_thumbnail.ml";;

*)

open Jvag_types ;;

exception Step_on_empty_thumbnail ;;
exception Step_on_empty_row ;;

module Private = struct 

   let expand (Tn(l)) ?(index_among_equals=1) name (Tn expansion_for_name)= 
    let indexed = Int_range.index_everything l in 
    let temp2 = List.filter (fun (_idx,row)->List.hd(row)=name) indexed in 
    let r = List.length (temp2) in 
    let idx2 = (index_among_equals mod r) in 
    let idx3 = (if idx2=0 then r else idx2) in 
    let (final_idx,final_row) = List.nth temp2 (idx3-1) in 
    let final_tail = List.tl final_row in 
    (Tn(List.flatten(Image.image (
      fun (idx,row)->
       if idx=final_idx 
       then Image.image (fun row2 -> row2@final_tail) expansion_for_name 
       else [row]
    ) indexed)));; 

let get gram name = 
   let content =(match Jvag_grammar.get gram name with
      Optional _
      | Star _
      | Molecular _ -> [[name]]     
  | Concat (l) -> [l]
  | Disjunction(l)->  Image.image (fun x->[x]) l
  | Synonym (nm)-> [[nm]]) in
  Tn(content) ;;   


end ;;   

let chain_expansion gram chain =
   let expa = (fun tnail x->Private.expand tnail x (Private.get gram x)) in 
   List.fold_left expa (Private.get gram (List.hd chain)) (List.tl chain);;

let expand = Private.expand ;;

let get = Private.get ;;


let pumping_lemma (Tn(l)) ~core_name ~extender_name = 
    (Tn(List.filter_map (
      fun row->
         if List.hd(row)=core_name 
         then None 
         else Some(row@["Starred"^extender_name])
    ) l)) ;;

let step gram (Tn l) = 
   match l with 
    [] -> raise (Step_on_empty_thumbnail)
    | row :: other_rows ->
   match row with 
   [] -> raise Step_on_empty_row
  | name :: other_names ->
     let (offshoot_opt,new_rows)=(
     match Jvag_grammar.get gram name with 
      Optional (nm)->(None,[other_names;nm::other_names])
    | Concat (l) -> (None,[l@other_names])
  | Disjunction(l)->(None,Image.image (fun x->x::other_names) l)
  | Star (nm)->(None,[other_names;nm::name::other_names])
  | Synonym (nm)->(None,[nm::other_names])
  | Molecular(toks)->
      let (head_tok,other_toks)=List_again.head_with_tail toks in 
      if other_toks=[]
      then (Some(head_tok,other_names),[])
      else 
      let name_for_tail =Jvsp_util.code_for_tokentype_sequence_in_production_names other_toks in 
      (Some(head_tok,name_for_tail::other_names),[])) in
   (offshoot_opt,Tn(new_rows@other_rows)) ;;     

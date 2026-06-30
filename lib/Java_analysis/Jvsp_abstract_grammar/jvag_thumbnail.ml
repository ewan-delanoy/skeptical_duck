(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_thumbnail.ml";;

*)

open Jvag_types ;;

let get gram name = 
   let content =(match Jvag_grammar.get gram name with
      Optional _
      | Star _
      | Molecular _ -> [[name]]     
  | Concat (l) -> [l]
  | Disjunction(l)->  Image.image (fun x->[x]) l
  | Synonym (nm)-> [[nm]]) in
  Tn(content) ;;

let expand (Tn(l)) ?(index_among_equals=1) name (Tn expansion_for_name)= 
    let indexed = Int_range.index_everything l in 
    let temp2 = List.filter (fun (_idx,row)->List.hd(row)=name) indexed in 
    let r = List.length (temp2) in 
    let idx2 = (index_among_equals mod r) in 
    let idx3 = (if idx2=0 then r else idx2) in 
    let (idx4,_) = List.nth temp2 (idx3-1) in 
    List.flatten(Image.image (
      fun (idx,row)->if idx=idx4 then expansion_for_name else [row]
    ) indexed);; 

let pumping_lemma (Tn(l)) ~core_name ~extender_name = 
    (Tn(List.filter_map (
      fun row->
         if List.hd(row)=core_name 
         then None 
         else Some(row@["Starred"^extender_name])
    ) l)) ;;

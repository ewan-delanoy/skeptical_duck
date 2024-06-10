(*

#use"lib/longest_match_extractor.ml";;

Works only with a small list of words.

*)


let compute_all_prefixes words = 
   let temp1 = [""] ::(Image.image (fun word->
     Int_range.scale (
       fun k->String.sub word 0 k
     ) 1 (String.length word)
  ) words) in 
  Ordered.fold_merge 
       Total_ordering.lex_for_strings temp1 ;;	 

let compute_alphabet words = 
	let temp1 = Image.image Strung.explode words in  
    let temp2 = List.flatten temp1 in 
    Ordered.sort Total_ordering.for_characters temp2 ;; 

let rec index_among_prefixes_opt s l = 
   match l with 
   [] -> None 
   | (idx,s2) :: others ->
   match Total_ordering.lex_for_strings s s2 with 
     Total_ordering_result_t.Equal -> Some idx  
     |Total_ordering_result_t.Lower -> None 
     |Total_ordering_result_t.Greater -> index_among_prefixes_opt s others ;;


let make original_list_of_words = 
  let words = Ordered.sort 
	   Total_ordering.lex_for_strings original_list_of_words in  
  let prefixes = compute_all_prefixes words 
  and alphabet = compute_alphabet words in 
  let indexed_prefixes = Int_range.index_everything prefixes in    
  let index_among_prefixes_opt =(fun s -> 
    index_among_prefixes_opt s indexed_prefixes) in  
  let inner_fan =(fun s c ->
  	let new_s = s ^ (String.make 1 c) in 
    Option.map (
     fun new_idx -> (c,new_idx)
    )(index_among_prefixes_opt new_s)) in
   Longest_match_extractor_t.LME(
     Image.image (fun (idx,s) 
      -> 
       let s_is_inside = 
          Ordered.mem Total_ordering.lex_for_strings s words in  
       (idx,(s_is_inside,List.filter_map (inner_fan s) alphabet))
   ) indexed_prefixes );; 

let rec assoc_character_opt c l = 
   match l with 
   [] -> None 
   | (c2,new_idx) :: others ->
   match Total_ordering.for_characters c c2 with 
     Total_ordering_result_t.Equal -> Some new_idx  
     |Total_ordering_result_t.Lower -> None 
     |Total_ordering_result_t.Greater -> assoc_character_opt c others ;;


let rec helper_for_applier triple 
   (best_found_so_far,prefix_so_far,prefix_idx,next_idx_in_text) = 
  let (Longest_match_extractor_t.LME data,text,text_length) = triple in 
  if next_idx_in_text > text_length 
  then best_found_so_far 
  else 
  let c= String.get text (next_idx_in_text-1) in 
  let paths_from_here = snd(List.assoc prefix_idx data) in 
  match assoc_character_opt c paths_from_here with 
    None -> best_found_so_far 
   |Some(new_prefix_idx) ->
    let new_prefix = prefix_so_far ^ (String.make 1 c) in 
    let new_prefix_is_inside = 
       fst(List.assoc new_prefix_idx data) in 
    let new_best_found = (
      if new_prefix_is_inside then Some new_prefix else best_found_so_far 
    ) in 
    helper_for_applier triple 
   (new_best_found,new_prefix,new_prefix_idx,next_idx_in_text+1) ;; 

let apply lme text idx_in_text = 
	helper_for_applier (lme,text,String.length text) 
   (None,"",1,idx_in_text) ;;

(*
	
let lme1 = make ["Ronan";"gain";"Ron"];;
let text1 = "This is Saiorse Ronan again";;
let res1 = apply lme1 text1 17 ;; 
let res2 = apply lme1 text1 18 ;; 

*)




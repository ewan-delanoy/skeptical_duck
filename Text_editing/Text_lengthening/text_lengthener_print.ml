(*

#use "Text_editing/Text_lengthening/text_lengthener_print.ml";;

*)

module Private=struct

let display_list display_elt horizontal_offset_length l=  
   let horizontal_offset=String.make horizontal_offset_length ' ' in 
   if l=[] then horizontal_offset^"[]" else 
   let temp1=Image.image (fun t->(display_elt (horizontal_offset_length+2) t)^";") l in 
   let temp2=("[")::temp1@["]"] in                            
   String.concat "\n" temp2;;

let adjustment horizontal_offset_length (a,b,c)=
   let horizontal_offset=String.make horizontal_offset_length ' ' in 
   horizontal_offset^"(\""^a^"\",\""^b^"\",\""^c^"\")";;

let adjustments =  display_list adjustment ;;

let decompression horizontal_offset_length (short_form,long_form,l_adjs)=
   let horizontal_offset=String.make horizontal_offset_length ' ' in 
   let temp1=horizontal_offset^"(\""^short_form^"\",\""^long_form^"\",\n" in 
   let n1=String.length temp1 in 
   temp1^
         (adjustments n1 l_adjs)^
   "\n"^
   horizontal_offset^")"
;;
   
let decompressions = display_list decompression;;   

let expansion horizontal_offset_length expsn=
   let horizontal_offset=String.make horizontal_offset_length ' ' in 
   let temp1=String.concat ";" (Image.image (fun s->"\""^s^"\"") expsn) in 
   horizontal_offset^"["^temp1^"]";;
   
let expansions = display_list expansion;; 

let inert_word horizontal_offset_length word=
   let horizontal_offset=String.make horizontal_offset_length ' ' in 
   horizontal_offset^"\""^word^"\"";;

let inert_words = display_list inert_word;;    
    
let abbreviation horizontal_offset_length (a,b)=
   let horizontal_offset=String.make horizontal_offset_length ' ' in 
   horizontal_offset^"(\""^a^"\",\""^b^"\")";;

let abbreviations = display_list abbreviation;; 

let t x=
  let s_decomps=decompressions 2 (Txl_field.uncapitalized_adjustable_decompressions x) 
  and s_expansions=expansions 2 (Txl_field.expansions x)
  and s_iwords=inert_words  2 (Txl_field.uncapitalized_inert_words x)
  and s_lc_abbreviations=abbreviations 2 (Txl_field.uncapitalized_left_core_abbreviations x)
  and s_px_abbreviations=abbreviations 2(Txl_field.uncapitalized_prefix_abbreviations x)
  and s_ci_decomps=decompressions 2 (Txl_field.adjustable_decompressions x) 
  and s_ci_iwords=inert_words  2 (Txl_field.inert_words x)
  and s_ci_lc_abbreviations=abbreviations 2 (Txl_field.left_core_abbreviations x)
  and s_ci_px_abbreviations=abbreviations 2 (Txl_field.prefix_abbreviations x)
  in 
  "{\n"^
  " Text_length"^"ener_t.adjustable_decompressions=\n"^s_decomps^";\n"^
  "expansions=\n"^s_expansions^";\n"^
  "inert_words=\n"^s_iwords^";\n"^
  "left_core_abbreviations=\n"^s_lc_abbreviations^";\n"^ 
  "prefix_abbreviations=\n"^s_px_abbreviations^";\n"^
  "case_insensitive_adjustable_decompressions=\n"^s_ci_decomps^";\n"^
  "case_insensitive_inert_words=\n"^s_ci_iwords^";\n"^
  "case_insensitive_left_core_abbreviations=\n"^s_ci_lc_abbreviations^";\n"^ 
  "case_insensitive_prefix_abbreviations=\n"^s_ci_px_abbreviations^";\n"^
  "}";;

end;;

let print=Private.t;;





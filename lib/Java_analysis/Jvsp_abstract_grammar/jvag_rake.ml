(*

#use"lib/Java_analysis/Jvsp_abstract_grammar/jvag_rake.ml";;

*)

open Jvag_types ;;
     
let make cfg = 
  let gram1 = Jvag_grammar.modify cfg.original_grammar [Local(cfg.main_name,[Lm_expand_all_disjunctions])] in 
  let dis1 = Jvag_form.disjunction_content (Jvag_grammar.get gram1 cfg.main_name) in 
  {
    config = cfg ;
    current_grammar = gram1 ;
    current_expansion = Image.image (fun nm->([nm],Jvag_form.concat_content (Jvag_grammar.get gram1 nm))) dis1 ;
  } ;;

(*

 #use "Text_editing/Text_lengthening/abbrex_modify_standard.ml";;

 *)

  module Private=struct

  let current_state ()=
     {
      Abbreviation_expander_t.worker = (Txl_standard.current_state());
      production = (Dtu_standard.current_state());
     };;
  
  let persist abbrex=
        Txl_standard.Friend.set_state(abbrex.Abbreviation_expander_t.worker);
        Txl_update_standard.persist_to_file();
        Dtu_standard.Friend.set_state(abbrex.Abbreviation_expander_t.production);
        Dtu_update_standard.persist_to_file();;
  
  let standardize_with_no_offshoot f arg=
     let old_abbrex=current_state () in 
     let new_abbrex = f old_abbrex arg in  
     persist new_abbrex;;

  let standardize f arg=
     let old_abbrex=current_state () in 
     let (new_abbrex,result) = f old_abbrex arg in  
     let _=persist new_abbrex in 
     result;;
   

 end;;

  let add_word =Private.standardize_with_no_offshoot Abbrex_modify.add_word;;
    
  let add_words=Private.standardize_with_no_offshoot Abbrex_modify.add_words;;  

  let fix_order=Private.standardize_with_no_offshoot (fun abbrex ()->Abbrex_modify.fix_order abbrex) ;;

  let i_adjustment=Private.standardize Abbrex_modify.i_adjustment;; 
  let i_decompression=Private.standardize Abbrex_modify.i_decompression;; 
  let i_expansion=Private.standardize Abbrex_modify.i_expansion;; 
  let i_inert_word=Private.standardize Abbrex_modify.i_inert_word;; 
  let i_left_core_abbreviation=Private.standardize Abbrex_modify.i_left_core_abbreviation;;   
  let i_prefix_abbreviation=Private.standardize Abbrex_modify.i_prefix_abbreviation;;   

  let r_adjustment=Private.standardize Abbrex_modify.r_adjustment;; 
  let r_decompression=Private.standardize Abbrex_modify.r_decompression;; 
  let r_expansion=Private.standardize Abbrex_modify.r_expansion;; 
  let r_inert_word=Private.standardize Abbrex_modify.r_inert_word;; 
  let r_left_core_abbreviation=Private.standardize Abbrex_modify.r_left_core_abbreviation;;   
  let r_prefix_abbreviation=Private.standardize Abbrex_modify.r_prefix_abbreviation;;  
 
  (*
 let example=
   {
      Abbreviation_expander_t.worker = (!(Txl_standard.one));
      production = Dtu_standard.one;
   };;

 add_word example "habitllm";;
 let see1=example.Abbreviation_expander_t.production;;
 let see2=Dtu_construct.deconstruct see1;;



 *) 
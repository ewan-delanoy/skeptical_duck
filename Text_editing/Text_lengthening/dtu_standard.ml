(*

 #use "Text_editing/Text_lengthening/abbrex_modify.ml";;

 *)

  module Private=struct

  let propagate_txl_method abbrex f arg_for_f=
    let old_txl=abbrex.Abbreviation_expander_t.worker in 
    let new_txl=f old_txl arg_for_f in 
    let _=(abbrex.Abbreviation_expander_t.worker <- new_txl) in 
    Dtu_modify.recompute abbrex.Abbreviation_expander_t.production new_txl;;
 end;;

  let add_word abbrex word=
    let txl=abbrex.Abbreviation_expander_t.worker 
    and dtu=abbrex.Abbreviation_expander_t.production in
    let lengthened_word=Txl_apply.apply txl word in
    (
       Dtu_modify.add_pair dtu (word,lengthened_word);
    );;

  let add_words abbrex words = List.iter (add_word abbrex) words;;

  let fix_order abbrex =
    let old_txl=abbrex.Abbreviation_expander_t.worker in 
    let new_txl=Txl_modify.order_fix  old_txl in 
    (abbrex.Abbreviation_expander_t.worker <- new_txl);;

  let i_adjustment abbrex=
    Private.propagate_txl_method abbrex Txl_modify.i_adjustment;;

  let i_decompression abbrex=
    Private.propagate_txl_method abbrex Txl_modify.i_decompression;;

  let i_expansion abbrex=
    Private.propagate_txl_method abbrex Txl_modify.i_expansion;;

  let i_inert_word abbrex=
    Private.propagate_txl_method abbrex Txl_modify.i_inert_word;;

  let i_left_core_abbreviation abbrex=
    Private.propagate_txl_method abbrex Txl_modify.i_left_core_abbreviation;;

  let i_prefix_abbreviation abbrex=
    Private.propagate_txl_method abbrex Txl_modify.i_prefix_abbreviation;;

 
  let r_adjustment abbrex=
    Private.propagate_txl_method abbrex Txl_modify.r_adjustment;;

  let r_decompression abbrex=
    Private.propagate_txl_method abbrex Txl_modify.r_decompression;;

  let r_expansion abbrex=
    Private.propagate_txl_method abbrex Txl_modify.r_expansion;;

  let r_inert_word abbrex=
    Private.propagate_txl_method abbrex Txl_modify.r_inert_word;;

  let r_left_core_abbreviation abbrex=
    Private.propagate_txl_method abbrex Txl_modify.r_left_core_abbreviation;;

  let r_prefix_abbreviation abbrex=
    Private.propagate_txl_method abbrex Txl_modify.r_prefix_abbreviation;;

 
 
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
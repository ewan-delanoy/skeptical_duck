(*

 #use "Text_editing/Text_lengthening/sabbrex_modify.ml";;

 *)

  module Private=struct

  let display_on_screen sabbrex =
      let abbrex=sabbrex.Screened_abbreviation_expander_t.engine  in 
      let dtu=abbrex.Abbreviation_expander_t.production in 
      let text=Dtu_excerpt_from_end.excerpt_from_end dtu 
      and screen=sabbrex.Screened_abbreviation_expander_t.screen in 
      Io.overwrite_with screen text;;

  let propagate_abbrex_method sabbrex f arg_for_f=
    let old_abbrex=sabbrex.Screened_abbreviation_expander_t.engine  in 
    let (new_abbrex,offshoots)=f old_abbrex arg_for_f in 
    let new_sabbrex = {
      sabbrex with
       Screened_abbreviation_expander_t.engine = new_abbrex;
    } in 
    let _=display_on_screen new_sabbrex in 
    (new_sabbrex,offshoots);;
 end;;

  let add_word sabbrex word=
    let old_abbrex=sabbrex.Screened_abbreviation_expander_t.engine  in 
    let new_abbrex=Abbrex_modify.add_word  old_abbrex word in 
    let new_sabbrex = {
      sabbrex with
       Screened_abbreviation_expander_t.engine = new_abbrex;
    } in 
    let _=Private.display_on_screen new_sabbrex in 
    new_sabbrex;;

  let add_words abbrex words = List.fold_left add_word abbrex words;;

  let fix_order sabbrex =
    let old_abbrex=sabbrex.Screened_abbreviation_expander_t.engine in 
    let new_abbrex=Abbrex_modify.fix_order  old_abbrex in 
    {
       sabbrex with 
       Screened_abbreviation_expander_t.engine = new_abbrex;
    };;

  let i_adjustment sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.i_adjustment;;

  let i_decompression sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.i_decompression;;

  let i_expansion sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.i_expansion;;

  let i_inert_word sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.i_inert_word;;

  let i_left_core_abbreviation sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.i_left_core_abbreviation;;

  let i_prefix_abbreviation sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.i_prefix_abbreviation;;

 
  let r_adjustment sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.r_adjustment;;

  let r_decompression sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.r_decompression;;

  let r_expansion sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.r_expansion;;

  let r_inert_word sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.r_inert_word;;

  let r_left_core_abbreviation sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.r_left_core_abbreviation;;

  let r_prefix_abbreviation sabbrex=
    Private.propagate_abbrex_method sabbrex Abbrex_modify.r_prefix_abbreviation;;

 
 
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
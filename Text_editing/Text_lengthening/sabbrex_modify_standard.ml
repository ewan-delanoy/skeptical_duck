(*

#use "Text_editing/Text_lengthening/sabbrex_modify_standard.ml";;

*)



let screen_location=ref(
   Absolute_path.of_string "Text_editing/Text_lengthening/screen.txt"
);;

module Private=struct

   let current_state ()=
      let current_abbrex = 
      {
         Abbreviation_expander_t.worker = (Txl_standard.current_state());
         production = (Dtu_standard.current_state());
      } in 
      {
         Screened_abbreviation_expander_t.engine = current_abbrex;
         screen = (!screen_location);
      }

   let set_state sabbrex =
      let abbrex = sabbrex.Screened_abbreviation_expander_t.engine in 
      Txl_standard.Friend.set_state( abbrex.Abbreviation_expander_t.worker );
      Txl_update_standard.persist_to_file();
      Dtu_standard.Friend.set_state( abbrex.Abbreviation_expander_t.production );
      Dtu_update_standard.persist_to_file();
      ;;  

   let particularize_sabbrex_method f arg_for_f=
      let old_sabbrex=current_state()  in 
      let (new_sabbrex,offshoots)=f old_sabbrex arg_for_f in 
      let  _=set_state old_sabbrex in 
      offshoots;;
 end;;

let add_word word = 
   let old_sabbrex=Private.current_state()  in 
   let new_sabbrex=Sabbrex_modify.add_word  old_sabbrex word in 
   let new_abbrex = new_sabbrex.Screened_abbreviation_expander_t.engine in
   Dtu_standard.Friend.set_state (new_abbrex.Abbreviation_expander_t.production);
   Dtu_update_standard.persist_to_file();;

let add_words words = List.iter add_word words;;

let fix_order ()=
   let old_sabbrex=Private.current_state()  in 
   let new_sabbrex=Sabbrex_modify.fix_order old_sabbrex in 
   let new_abbrex = new_sabbrex.Screened_abbreviation_expander_t.engine in
   Txl_standard.Friend.set_state ( new_abbrex.Abbreviation_expander_t.worker );
   Txl_update_standard.persist_to_file();;


  let i_adjustment sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.i_adjustment;;

  let i_decompression sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.i_decompression;;

  let i_expansion sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.i_expansion;;

  let i_inert_word sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.i_inert_word;;

  let i_left_core_abbreviation sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.i_left_core_abbreviation;;

  let i_prefix_abbreviation sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.i_prefix_abbreviation;;

 
  let r_adjustment sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.r_adjustment;;

  let r_decompression sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.r_decompression;;

  let r_expansion sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.r_expansion;;

  let r_inert_word sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.r_inert_word;;

  let r_left_core_abbreviation sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.r_left_core_abbreviation;;

  let r_prefix_abbreviation sabbrex=
    Private.particularize_sabbrex_method Sabbrex_modify.r_prefix_abbreviation;;

 
 
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

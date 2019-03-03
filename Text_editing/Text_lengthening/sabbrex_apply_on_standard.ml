(*

#use "Text_editing/Text_lengthening/sabbrex_apply_on_standard.ml";;

*)


module Md = Sabbrex_modify_standard;;

let apply cmd= match cmd with 
    Command_on_abbreviation_expander_t.Add_words (words)->
      let _=Md.add_words words 
      in 
      Unexpected_change_after_update_t.Ucau []
   |Do_nothing -> Unexpected_change_after_update_t.Ucau []   
   |Insert_adjustment(u,v,(ad1,ad2,ad3))->Md.i_adjustment(u,v,(ad1,ad2,ad3))
   |Insert_decompression(u,v)->Md.i_decompression(u,v)
   |Insert_expansion(expansion)->Md.i_expansion expansion
   |Insert_inert_word(word)->Md.i_inert_word word
   |Insert_left_core_abbreviation(u,v)->Md.i_left_core_abbreviation(u,v)
   |Insert_prefix_abbreviation(u,v)->Md.i_prefix_abbreviation(u,v)
   |Remove_adjustment(u,v,(ad1,ad2,ad3))->Md.r_adjustment(u,v,(ad1,ad2,ad3))
   |Remove_decompression(u,v)->Md.r_decompression(u,v)
   |Remove_expansion(expansion)->Md.r_expansion expansion
   |Remove_inert_word(word)->Md.r_inert_word word
   |Remove_left_core_abbreviation(u,v)->Md.r_left_core_abbreviation(u,v)
   |Remove_prefix_abbreviation(u,v)->Md.r_prefix_abbreviation(u,v) ;;



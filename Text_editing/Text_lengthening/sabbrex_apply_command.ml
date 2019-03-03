(*

#use "Text_editing/Text_lengthening/sabbrex_apply_command.ml";;

*)

module Md = Sabbrex_modify ;;
let no_unexpected_change = Unexpected_change_after_update_t.Ucau [];;


let apply cmd sabbrex = match cmd with 
    Command_on_abbreviation_expander_t.Add_newline(nbr_of_newlines) ->
       let newlines = String.make nbr_of_newlines "\n" in 
       (Md.add_word sabbrex newlines,no_unexpected_change)
   |Add_words(words)->(Md.add_words sabbrex words,no_unexpected_change)
   |Do_nothing -> (sabbrex,no_unexpected_change) 
   |Insert_adjustment(u,v,(ad1,ad2,ad3))->Md.i_adjustment sabbrex (u,v,(ad1,ad2,ad3))
   |Insert_decompression(u,v)->Md.i_decompression sabbrex (u,v)
   |Insert_expansion(expansion)->Md.i_expansion sabbrex expansion
   |Insert_inert_word(word)->Md.i_inert_word sabbrex word
   |Insert_left_core_abbreviation(u,v)->Md.i_left_core_abbreviation sabbrex (u,v)
   |Insert_prefix_abbreviation(u,v)->Md.i_prefix_abbreviation sabbrex (u,v)
   |Remove_adjustment(u,v,(ad1,ad2,ad3))->Md.r_adjustment sabbrex (u,v,(ad1,ad2,ad3))
   |Remove_decompression(u,v)->Md.r_decompression sabbrex (u,v)
   |Remove_expansion(expansion)->Md.r_expansion sabbrex expansion
   |Remove_inert_word(word)->Md.r_inert_word sabbrex word
   |Remove_left_core_abbreviation(u,v)->Md.r_left_core_abbreviation sabbrex (u,v)
   |Remove_prefix_abbreviation(u,v)->Md.r_prefix_abbreviation sabbrex (u,v) ;;



(*

#use"Text_editing/whitespace_separated.ml";;

This is basically just the Str.full_split map, with 
a little bit of standardization (if the string starts with
whitespace we artificially insert a dummy empty text at the beginning,
and if the string ends with non-whitespace we artificially a dummy
empty whitespace sequence at the end).

*)

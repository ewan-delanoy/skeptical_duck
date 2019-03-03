(*

#use "Text_editing/Text_lengthening/parse_sabbrex_command.ml";;

*)

exception Nth_exn of (string list)*int;;

module Private  = struct

let insert = "zi";;
let remove = "zr";;


let dictionary params=
  let nth=(fun k->
    if (k<1)||(k>(List.length params)) 
    then raise(Nth_exn(params,k))
    else List.nth params (k-1) ) in 
[
    insert^"a",Command_on_abbreviation_expander_t.Insert_adjustment(nth 1,nth 2,(nth 3,nth 4,nth 5));
    insert^"d",Command_on_abbreviation_expander_t.Insert_decompression(nth 1,nth 2);
    insert^"e",Command_on_abbreviation_expander_t.Insert_expansion(params);
    insert^"w",Command_on_abbreviation_expander_t.Insert_inert_word(nth 1);
    insert^"c",Command_on_abbreviation_expander_t.Insert_left_core_abbreviation(nth 1,nth 2);
    insert^"p",Command_on_abbreviation_expander_t.Insert_prefix_abbreviation(nth 1,nth 2);

    remove^"a",Command_on_abbreviation_expander_t.Remove_adjustment(nth 1,nth 2,(nth 3,nth 4,nth 5));
    remove^"d",Command_on_abbreviation_expander_t.Remove_decompression(nth 1,nth 2);
    remove^"e",Command_on_abbreviation_expander_t.Remove_expansion(params);
    remove^"w",Command_on_abbreviation_expander_t.Remove_inert_word(nth 1);
    remove^"c",Command_on_abbreviation_expander_t.Remove_left_core_abbreviation(nth 1,nth 2);
    remove^"p",Command_on_abbreviation_expander_t.Remove_prefix_abbreviation(nth 1,nth 2);
];;

end ;; 

let parse s=
   let temp1=Str.split (Str.regexp "[ \t\n\r]+") s in 
   if temp1=[] then Command_on_abbreviation_expander_t.Do_nothing else 
   let (main_cmd,params)=Listennou.ht temp1 in 
   match Option.seek (fun (cmd_name,_)->cmd_name=main_cmd) (Private.dictionary params) with 
   None->Command_on_abbreviation_expander_t.Add_words(temp1)
   |Some(_,full_cmd)->full_cmd;;








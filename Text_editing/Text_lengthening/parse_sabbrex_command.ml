(*

#use "Text_editing/Text_lengthening/parse_sabbrex_command.ml";;

*)

exception Nth_exn of (string list)*int;;


module Private  = struct

let checked_nth params k=
    if (k<1)||(k>(List.length params)) 
    then raise(Nth_exn(params,k))
    else List.nth params (k-1) ;;

let insert = "zi";;
let remove = "zr";;

let find_insertion_command c main_cmd params=
  let nth=checked_nth params in  
  match c with  
   'a'->Command_on_abbreviation_expander_t.Insert_adjustment(nth 1,nth 2,(nth 3,nth 4,nth 5))
  |'d'->Command_on_abbreviation_expander_t.Insert_decompression(nth 1,nth 2)
  |'e'->Command_on_abbreviation_expander_t.Insert_expansion(params)
  |'w'->Command_on_abbreviation_expander_t.Insert_inert_word(nth 1)
  |'c'->Command_on_abbreviation_expander_t.Insert_left_core_abbreviation(nth 1,nth 2)
  |'p'->Command_on_abbreviation_expander_t.Insert_prefix_abbreviation(nth 1,nth 2)
  |_->Command_on_abbreviation_expander_t.Add_words(main_cmd::params);;

let find_removal_command c main_cmd params=
  let nth=checked_nth params in  
  match c with  
   'a'->Command_on_abbreviation_expander_t.Insert_adjustment(nth 1,nth 2,(nth 3,nth 4,nth 5))
  |'d'->Command_on_abbreviation_expander_t.Insert_decompression(nth 1,nth 2)
  |'e'->Command_on_abbreviation_expander_t.Insert_expansion(params)
  |'w'->Command_on_abbreviation_expander_t.Insert_inert_word(nth 1)
  |'c'->Command_on_abbreviation_expander_t.Insert_left_core_abbreviation(nth 1,nth 2)
  |'p'->Command_on_abbreviation_expander_t.Insert_prefix_abbreviation(nth 1,nth 2)
  |_->Command_on_abbreviation_expander_t.Add_words(main_cmd::params);;


let find_command main_cmd params=
  let nth=(fun k->
    if (k<1)||(k>(List.length params)) 
    then raise(Nth_exn(params,k))
    else List.nth params (k-1) ) in 
  if String.length(main_cmd)<3
  then Command_on_abbreviation_expander_t.Add_words(main_cmd::params)
  else 
  let cth=(fun j->String.get main_cmd (j-1)) in
  let c2=cth 2 in 
  if ((cth 1)<>'z')||(not(List.mem c2 ['i';'r'] ))
  then Command_on_abbreviation_expander_t.Add_words(main_cmd::params)
  else 
  match cth 2 with
   'i'->find_insertion_command (cth 3) main_cmd params 
  |'r'->find_removal_command (cth 3) main_cmd params
  |_->Command_on_abbreviation_expander_t.Add_words(main_cmd::params);;
  

end ;; 

let parse s=
   let temp1=Str.split (Str.regexp "[ \t\n\r]+") s in 
   if temp1=[] then Command_on_abbreviation_expander_t.Do_nothing else 
   let (main_cmd,params)=Listennou.ht temp1 in 
   Private.find_command main_cmd params;;








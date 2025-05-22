(*

#use"lib/Text_editing/remove_hyphens.ml";;

*)

module Private = struct 

let char_is_in_range (a,b) c=
  let i = int_of_char c in 
  (a<=i)&&(i<=b);;  
(*
let is_a_digit             = char_is_in_range (48,57) ;;
let is_an_uppercase_letter = char_is_in_range (65,90) ;;
*)
let is_a_lowercase_letter  = char_is_in_range (97,122) ;;

end ;;

let in_text text =
    let n = String.length text in 
    let getchar = Strung.get text in 
    let dashes = List.filter (
        fun j-> 
          (Private.is_a_lowercase_letter(getchar(j-1)))
          &&
          (getchar(j)='-')
          &&
          (List.mem (getchar(j+1)) [' ';'\n'])
          &&
          (Private.is_a_lowercase_letter(getchar(j+2)))
    ) (Int_range.range 2 (n-2)) in 
    let replacements =Image.image (fun j->((j,j+1),"")) dashes in 
    Strung.replace_ranges_in replacements text;;

let in_file fn=
    let old_text=Io.read_whole_file fn in
    let new_text=in_text old_text  in
    Io.overwrite_with fn new_text;; 



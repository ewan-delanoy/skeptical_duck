(*

#use"Text_editing/remove_hyphens.ml";;

*)

module Private = struct 

let is_a_lowercase_letter c=
  let i = int_of_char c in 
  (97<=i)&&(i<=122);;

end ;;

let in_string text =
    let n = String.length text in 
    let getchar = Strung.get text in 
    let dashes = List.filter (
        fun j-> 
          (Private.is_a_lowercase_letter(getchar(j-1)))
          &&
          (getchar(j)='-')
          &&
          (getchar(j+1)='\n')
          &&
          (Private.is_a_lowercase_letter(getchar(j+2)))
    ) (Ennig.ennig 2 n) in 
    let replacements =Image.image (fun j->((j,j+1),"")) dashes in 
    Strung.replace_ranges_in replacements text;;

let in_file fn=
    let old_text=Io.read_whole_file fn in
    let new_text=in_string old_text  in
    Io.overwrite_with fn new_text;; 



(*

#use "Text_editing/Text_lengthening/dtu_print.ml";;

*)

module Private=struct

let pair horizontal_offset_length  (a,b)=
  let horizontal_offset=String.make horizontal_offset_length ' ' in 
  horizontal_offset^"("^(Strung.enclose a)^","^(Strung.enclose b)^")";;

let list_of_pairs horizontal_offset_length l=  
   let horizontal_offset=String.make horizontal_offset_length ' ' in 
   if l=[] then horizontal_offset^"[]" else 
   let temp1=Image.image (fun t->(pair (horizontal_offset_length+2) t)^";") l in 
   let temp2=("[")::temp1@["]"] in                            
   String.concat "\n" temp2;;


end;;

let print dtu=
  let l=Dtu_construct.deconstruct dtu in 
  "Dtu_"^"construct.construct( "^
   (Private.list_of_pairs 2 l)^
   ")" 
  ;;

(*

let base = [("a\"b\"g","cde");("fg","h\"i\"j")];;
let example = Dtu_construct.construct base;;
let text=print example;;
print_string text;;

*)



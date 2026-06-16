(*

#use"lib/charset.ml";;

*)

module Private = struct 

let i0 = int_of_char '0' ;;
let i9 = int_of_char '9' ;;
let is_a_digit c = 
   let i = int_of_char c in 
   (i0<=i) && (i<=i9) ;;

let is_not_a_digit c = not(is_a_digit c) ;;

end ;;  


let ocaml_modulename_nonfirst_letters=
  ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o';
 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'; 'A'; 'B'; 'C'; 'D';
 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S';
 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z'; '_'; '0'; '1'; '2'; '3'; '4'; '5'; '6';
 '7'; '8'; '9'];;

let is_a_digit = Private.is_a_digit ;;

let is_not_a_digit = Private.is_not_a_digit ;;
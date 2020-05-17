(*

#use"Test_directory6/Test_directory7/Test_directory2/tested_module_two.ml";;


*)

let f x y=
  let e=9+10 in
  let g=Tested_module_one.f 11 12 in
  e+g;;

let an_imported_value = Tested_module_one.A_Submodule.o_value + 2;;  

let snd_imported_value = Tested_module_one.o_deduced_value + 2;; 

type t = T of int ;;

let print_out (fmt:Format.formatter) (T x)=
   Format.fprintf fmt "@[%s@]" (string_of_int x);;     


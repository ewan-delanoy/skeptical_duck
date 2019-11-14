(*

#use"Test_directory6/Test_directory7/Test_directory2/tested_module_one.ml";;


*)

exception Hound;;


let f x y=
  if x=11 
  then raise(Hound)
  else x+y;;

module A_Submodule = struct 

let o_value = 1 ;;

end ;;

let a_deduced_value = A_Submodule.o_value + 2;;



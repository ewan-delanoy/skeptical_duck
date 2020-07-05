(*

#use"Depth_one_testdir/Depth_two_testdir/Depth_three_testdir/tested_module_one.ml";;


*)

exception Hound;;


let f x y=
  if x=11 
  then raise(Hound)
  else x+y;;

module A_Submodule = struct 

let o_value = 1 ;;

end ;;

let o_deduced_value = A_Submodule.o_value + 2;;

let lalala = 7;;

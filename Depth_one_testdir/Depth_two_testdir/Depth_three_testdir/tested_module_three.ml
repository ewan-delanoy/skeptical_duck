(*

#use"Depth_one_testdir/Depth_two_testdir/Depth_three_testdir/tested_module_three.ml";;


*)

let f x y=
  let c=5+6 in
  let d=Tested_module_two.f 7 8 in
  c+d;;

let from_module_two = Tested_module_two.print_out ;;  
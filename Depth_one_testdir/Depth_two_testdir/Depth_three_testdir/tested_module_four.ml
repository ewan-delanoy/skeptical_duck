(*

#use"Depth_one_testdir/Depth_two_testdir/Depth_three_testdir/tested_module_four.ml";;


*)

let f x y=
  let a=1+2 in
  let b=Tested_module_three.f 3 4 in
  a+b;;

    
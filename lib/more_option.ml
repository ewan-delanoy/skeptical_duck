(*

#use"lib/more_option.ml";;

*) 

 
let argument_on_the_right f x opt_y=match opt_y with
  None->x
  |Some(y)->f x y;;






 

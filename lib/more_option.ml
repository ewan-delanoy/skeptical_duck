(*

#use"lib/more_option.ml";;

*) 

 
let argument_on_the_right f x opt_y=match opt_y with
  None->x
  |Some(y)->f x y;;

let filter_and_unpack f l=
 let rec filter0=(function
  (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |x::peurrest->match f(x) with
		None->filter0(graet,peurrest)
		|Some(y)->filter0(y::graet,peurrest)
 ) in
 filter0([],l);;





 

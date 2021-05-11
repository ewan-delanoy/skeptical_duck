(*

#use"option.ml";;

*) 

exception Unpackable of string;;

module Private = struct 

let unpack_with_error_message s=function
None->raise(Unpackable(s))
|Some(x)->x;;

end ;;


let add_element_on_the_right l x=match x with
  None->l
  |Some(a)->l@[a];;
 
let rec filter_and_unpack f l=
 let rec filter0=(function
  (graet,da_ober)->match da_ober with
   []->List.rev(graet)
   |x::peurrest->match f(x) with
		None->filter0(graet,peurrest)
		|Some(y)->filter0(y::graet,peurrest)
 ) in
 filter0([],l);;

let  find_and_stop f l=
 let rec find_and_stop0=(function
  da_ober->match da_ober with
   []->None
   |a::peurrest->match f(a) with
		None->find_and_stop0(peurrest)
		|Some(x)->Some(x)
 ) in
 find_and_stop0(l);;

let propagate f=function
None->None
|Some(x)->Some(f(x));;

let rec seek f =function
[]->None
|a::b->if f(a) then Some(a) else seek(f)(b);;

let unpack x =Private.unpack_with_error_message "void is not unpackable" x;;





 

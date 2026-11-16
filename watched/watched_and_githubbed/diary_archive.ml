open Skeptical_duck_lib ;; 
open Needed_values ;;


(************************************************************************************************************************
 Entry 1 : Mock hash tables for debugging purposes
************************************************************************************************************************)
module Snip1 = struct 

  module Hushtbl = struct 

type ('a,'b) t =  HH of 'a * ((('a* 'b) list) ref) ;;

let create m forb= HH(forb,ref []) ;;

let add (HH (forb,l)) x y = 
  if x=forb
  then failwith("badbadbad !!!")  
  else (l:=(x,y)::(!l))  ;;

let find_opt (HH (_,l)) x = List.assoc_opt x (!l);;

end ;;  


end ;;
(*

#use"lib/Decomposed_filename/dfn_short.ml";;

*)

exception Of_line_exn of string;;

let of_line line = 
   let (mn,e)=Cull_string.split_wrt_rightmost line '.' in 
   if mn=""
   then raise(Of_line_exn(line))
   else Dfn_short_t.J(Dfa_module.of_line mn,Dfa_ending.of_line(e));;


let to_line (Dfn_short_t.J(m,e))=
   (Dfa_module.to_line m)^(Dfa_ending.connectable_to_modulename e);;

let to_module (Dfn_short_t.J(m,_e))=m;;
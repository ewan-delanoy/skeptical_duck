(*

 #use "Text_editing/Text_lengthening/dtu_excerpt_from_end.ml";;

 *)

 let max_line_size = ref(100);;
 let max_num_lines = ref (15);;
 let separator= " ";;
 

 module Private = struct

 let rec helper 
   (treated,treated_size,
    half_treated,half_treated_size,
    to_be_treated)=match to_be_treated with 
    []->half_treated::treated
   |new_bare_word::other_words ->
     if new_bare_word="\n"
     then (
            let (treated3,treated_size3)=
            (if half_treated_size=0
             then (""::treated,1+treated_size)
             else (""::half_treated::treated,2+treated_size)
            ) in 
            helper(treated3,treated_size3,"",0,other_words)
          )
     else 
     let new_word = new_bare_word ^ separator in  
     let new_half_treated = new_word^half_treated in
     let new_half_treated_size=(String.length new_word)+half_treated_size in 
     if new_half_treated_size <=(!(max_line_size)) 
     then helper(treated,treated_size,new_half_treated,new_half_treated_size,other_words)
     else
     let (treated2,to_be_treated2)=
     (if half_treated_size=0
      then (new_word::treated,other_words)
      else (half_treated::treated,to_be_treated)
     ) in 
     let treated_size2=1+treated_size in 
     if treated_size2 >= (!max_num_lines)
     then treated2
     else helper(treated2,treated_size2,"",0,to_be_treated2);;  

     let on_list l=String.concat "\n" (helper([],0,"",0,l));;

 end;;

let excerpt_from_end dtu=
   Private.on_list dtu.Double_tunnel_t.outcoming;;



 
(*
let h1=List.rev(Ennig.doyle (
  fun j->if j=631 then "\n" else Strung.left_completed_string_of_int 3 j
) 1 800);;

let h2=excerpt_from_end h1;;
*)

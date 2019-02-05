(*

#use"Text_editing/french_order.ml";;

*)

exception Unknown_first_char of string;;

module Private = struct

let indexed_chars=Ennig.index_everything French_data.chars_in_order;;

let get_next_french_char (s,s_treated)=
  match Option.seek (fun (j,chr)->
    Substring.is_a_substring_located_at chr s (s_treated+1) ) 
      indexed_chars with 
  None->let small_s=Cull_string.cobeginning s_treated s in 
        raise(Unknown_first_char(small_s))
  |Some(chr0)->chr0;;

let rec helper (s,s_length,s_treated,t,t_length,t_treated)=
  let s_finished=(s_treated>=s_length)
  and t_finished=(t_treated>=t_length) in 
  if s_finished 
  then if t_finished 
       then Total_ordering.Equal
       else Total_ordering.Lower
  else      
  if t_finished 
  then Total_ordering.Greater
  else
  let (idx_in_s,char_in_s)=get_next_french_char (s,s_treated) 
  and (idx_in_t,char_in_t)=get_next_french_char (t,t_treated) in 
  let attempt = Total_ordering.standard idx_in_s idx_in_t in 
  if attempt<>Total_ordering.Equal then attempt else 
  helper
  (s,s_length,s_treated+String.length(char_in_s),
   t,t_length,t_treated+String.length(char_in_t));;  

let main s t = 
  helper 
  (s,String.length s,0,
   t,String.length t,0);;

end;;

let cmp=( Private.main : string Total_ordering.t );;
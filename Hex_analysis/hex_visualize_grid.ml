(* 

#use"Hex_analysis/hex_visualize_grid.ml";;

*)

exception Label_out_of_range of int ;;

module Private = struct 


let get data (i,j)=
   try List.assoc (i,j) data with 
   _->Particular_string.triple_blank;;

let constant_left_margin = String.make 3 ' ';;

let variable_left_margin line_idx = 
   (* if line_idx<2 then "" else *)
   if line_idx=1
   then String.make 2 ' '
   else String.make (line_idx-1) ' ';;

let coordinate_mention line_idx = 
  if ((line_idx mod 2)=1)&&(line_idx>1) 
  then let i=(line_idx-1)/2 in
       let si=string_of_int i in  
       if i<10 then " "^si else si
  else " ";;


let first_row (Hex_dimension_t.D dimension)= 
 let temp1=Ennig.doyle (fun j->
     " "^(String.make 1 (char_of_int(j+96)))^"  "
  ) 1 dimension in 
  String.concat "" (" "::temp1)

let main_content_of_line (formal_dim,data) line_idx =
  let (Hex_dimension_t.D dim)=formal_dim in 
  if line_idx=1
  then first_row formal_dim 
  else 
  if (line_idx mod 2)=0
  then String.make (4*dim+2) '-'
  else 
  let i=(line_idx-1)/2 in 
  let temp1=Ennig.doyle (fun j->
     (get data (i,j))^"|"
  ) 1 dim in 
  String.concat "" ("|"::temp1);; 

let full_line grid line_idx = 
   constant_left_margin^
   (variable_left_margin line_idx)^
   (coordinate_mention line_idx)^ 
   (main_content_of_line grid line_idx);;


let visualization grid =
   let formal_dim = grid.Hex_ascii_grid_t.dimension 
   and data = grid.Hex_ascii_grid_t.data in 
   let (Hex_dimension_t.D dim) = formal_dim in 
   let player = grid.Hex_ascii_grid_t.beneficiary in
   "\n\n\n"^
   "Config benefitting Player "^(Hex_player.to_string player)^"\n\n\n"^
   (String.concat "\n" (Ennig.doyle (full_line (formal_dim,data)) 1 (2*dim+2)))^
   "\n\n\n";;

let int_in_cell j=
  let sj = string_of_int j in 
  match String.length sj with
  1 -> " "^sj^" "
  |2 -> " "^sj
  |_ -> sj ;;

let label_in_cell k=
  if (k<1)||(k>260) then raise(Label_out_of_range(k)) else 
  let r0=(k mod 26) in 
  let q0=(k-r0)/26 in 
  let (q1,r1) = (if r0=0 then (q0-1,26) else (q0,r0)) in 
  let main_char = String.make 1 (char_of_int (96+r1)) in 
  if q1=0 
  then " "^main_char^" "
  else " "^main_char^(string_of_int(q1));; 

(* let z1=Ennig.doyle label_in_cell 1 260;; *)

end ;;


let int_in_cell = Private.int_in_cell ;;
let label_in_cell = Private.label_in_cell ;;
let visualization = Private.visualization ;;





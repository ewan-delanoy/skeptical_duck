(* 

#use"Hex_analysis/hex_visualize_grid.ml";;

*)


module Private = struct 


let triple_blank = String.make 3 ' ';;

let get data (i,j)=
   try List.assoc (i,j) data with 
   _->triple_blank;;

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


let to_ascii_drawing grid =
   let formal_dim = grid.Hex_ascii_grid_t.dimension 
   and data = grid.Hex_ascii_grid_t.data in 
   let (Hex_dimension_t.D dim) = formal_dim in 
   let player = grid.Hex_ascii_grid_t.beneficiary in
   "\n\n\n"^
   "Config benefitting Player "^(Hex_player.to_string player)^"\n\n\n"^
   (String.concat "\n" (Ennig.doyle (full_line (formal_dim,data)) 1 (2*dim+2)))^
   "\n\n\n";;



end ;;



let to_ascii_drawing = Private.to_ascii_drawing ;;

let triple_blank = Private.triple_blank ;;




(* 

#use"Hex_analysis/hex_visualize_grid.ml";;

*)


module Private = struct 


let triple_blank = String.make 3 ' ';;

let get grid (i,j)=
   try List.assoc (i,j) grid.Hex_ascii_grid_t.data with 
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

let main_content_of_line grid line_idx =
  let formal_dim = grid.Hex_ascii_grid_t.dimension in 
  let (Hex_dimension_t.D dim)=formal_dim in 
  if line_idx=1
  then first_row formal_dim 
  else 
  if (line_idx mod 2)=0
  then String.make (4*dim+2) '-'
  else 
  let i=(line_idx-1)/2 in 
  let temp1=Ennig.doyle (fun j->
     (get grid (i,j))^"|"
  ) 1 dim in 
  String.concat "" ("|"::temp1);; 

let full_line grid line_idx = 
   constant_left_margin^
   (variable_left_margin line_idx)^
   (coordinate_mention line_idx)^ 
   (main_content_of_line grid line_idx);;

let to_ascii_drawing grid =
   let (Hex_dimension_t.D dim) = grid.Hex_ascii_grid_t.dimension in 
   let player = grid.Hex_ascii_grid_t.beneficiary in
   "Config benefitting Player "^(Hex_player.to_string player)^"\n\n\n"^
   (String.concat "\n" (Ennig.doyle (full_line grid) 1 (2*dim+2)));;

let visualize grid = print_string("\n\n\n"^(to_ascii_drawing grid)^"\n\n\n");;


let of_finished_game fgame =
   let winner = fgame.Hex_finished_game_t.winner in 
   let (fp_cells,sp_cells)=Listennou.split_list_in_half fgame.Hex_finished_game_t.sequence_of_moves in
   let (l_winner_cells,l_loser_cells)=(
       if winner=Hex_player_t.First_player
       then (fp_cells,sp_cells)
       else (sp_cells,fp_cells)
   ) in  
   let winner_ipairs = Image.image Hex_cell.to_int_pair l_winner_cells
   and loser_ipairs = Image.image Hex_cell.to_int_pair l_loser_cells in
   let associations1=Image.image (fun (i,j)->((i,j)," A ")) winner_ipairs
   and associations2=Image.image (fun (i,j)->((i,j),"EEE")) loser_ipairs in 
   {
    Hex_ascii_grid_t.beneficiary = winner;
    dimension = fgame.Hex_finished_game_t.dimension;
    data = associations1 @ associations2;
  };;


end ;;

let of_finished_game = Private.of_finished_game;;

let to_ascii_drawing = Private.to_ascii_drawing ;;

let triple_blank = Private.triple_blank ;;

let visualize = Private.visualize;;

let vg fgame = Hex_ascii_grid.visualize(Hex_ascii_grid.of_finished_game fgame);;

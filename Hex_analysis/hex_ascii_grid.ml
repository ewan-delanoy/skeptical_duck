(* 

#use"Hex_analysis/hex_ascii_grid.ml";;

*)

module Private = struct 


let triple_blank = String.make 3 ' ';;

let get grid (i,j)=
   try List.assoc (i,j) grid.Hex_ascii_grid_t.data with 
   _->triple_blank;;

let of_flattened_end_strategy dim ec =
  let square = Cartesian.square (Ennig.ennig 1 dim) in
  let tracer1 =  (fun (i,j)->
     let cell=Hex_ipair.to_cell (i,j) in 
     if Hex_cell_set.mem cell ec.Hex_flattened_end_strategy_t.active_part
     then " A "
     else 
     if Hex_cell_set.mem cell ec.Hex_flattened_end_strategy_t.passive_part
     then " P "
     else "   "
  ) in 
  let tracer2=(fun pair->
     let t=tracer1 pair in 
     if t="   "
     then None
     else Some(pair,tracer1 pair)) in 
  {
    Hex_ascii_grid_t.beneficiary = ec.Hex_flattened_end_strategy_t.beneficiary;
    dimension = dim;
    data = Option.filter_and_unpack tracer2 square;
  };;
   


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


let first_row dimension= 
 let temp1=Ennig.doyle (fun j->
     " "^(String.make 1 (char_of_int(j+96)))^"  "
  ) 1 dimension in 
  String.concat "" (" "::temp1)

let main_content_of_line grid line_idx =
  let dim = grid.Hex_ascii_grid_t.dimension in 
  if line_idx=1
  then first_row dim 
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
   let dim = grid.Hex_ascii_grid_t.dimension in 
   let player = grid.Hex_ascii_grid_t.beneficiary in
   "Config benefitting Player "^(Hex_player.to_string player)^"\n\n\n"^
   (String.concat "\n" (Ennig.doyle (full_line grid) 1 (2*dim+2)));;

let visualize grid = print_string("\n\n\n"^(to_ascii_drawing grid)^"\n\n\n");;

let make_ready_for_editing grid =
    let drawing = to_ascii_drawing grid in 
    let assignment = "\n\n\nlet z=\"\n\n\n"^drawing^"\n\n\n\";;\n\n\n" in 
    let ap=Absolute_path.of_string "Hex_analysis/hex_config_sheet.ml" in 
    Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string assignment) 
    ("(* Draft starts here *)","(* Draft ends here *)") ap;;



let read_player s=
  if Substring.is_a_substring_of "Player 1" s 
  then Hex_player_t.First_player
  else Hex_player_t.Second_player;;


let of_basic_linker dim player 
     (Hex_cell_set_t.S(actives),Hex_cell_pair_set_t.S(passives)) = 
   let active_ipairs = Image.image Hex_ipair.of_cell actives 
   and passive_ipairs = Image.image 
    (fun (cell1,cell2)->(Hex_ipair.of_cell cell1,Hex_ipair.of_cell cell2)) passives in 
   let temp1=Ennig.index_everything passive_ipairs in 
   let temp2=Image.image ( fun (k,(p1,p2))->
      let c=char_of_int(k+96) in 
      let s=" "^(String.make 1 c)^" " in 
      [p1,s;p2,s] )temp1 in 
   let associations1=Image.image (fun p->(p," A ")) active_ipairs 
   and associations2=List.flatten temp2 in  
{
   Hex_ascii_grid_t.beneficiary = player;
    dimension = dim;
    data = associations1 @ associations2;
};;


let of_finished_game fgame =
   let winner = fgame.Hex_finished_game_t.winner in 
   let (fp_cells,sp_cells)=Listennou.split_list_in_half fgame.Hex_finished_game_t.sequence_of_moves in
   let (l_winner_cells,l_loser_cells)=(
       if winner=Hex_player_t.First_player
       then (fp_cells,sp_cells)
       else (sp_cells,fp_cells)
   ) in 
   let winner_cells = Hex_cell_set.safe_set l_winner_cells 
   and loser_cells = Hex_cell_set.safe_set l_loser_cells in  
   let all_cells = Hex_common.all_cells 11 in 
   let (Hex_cell_set_t.S l_empty_cells) = Hex_cell_set.setminus (Hex_cell_set.setminus all_cells winner_cells) loser_cells in 
   let winner_ipairs = Image.image Hex_ipair.of_cell l_winner_cells
   and empty_ipairs = Image.image Hex_ipair.of_cell l_empty_cells in
   let associations1=Image.image (fun (i,j)->((i,j)," A ")) winner_ipairs
   and associations2=Image.image (fun (i,j)->((i,j)," P ")) empty_ipairs in 
   {
    Hex_ascii_grid_t.beneficiary = winner;
    dimension = fgame.Hex_finished_game_t.dimension;
    data = associations1 @ associations2;
  };;

exception Unbalanced_label of string * (Hex_cell_t.t list);;

let to_basic_linker grid=
  let temp1= grid.Hex_ascii_grid_t.data in  
  let temp2=Image.image (fun ((i,j),content)->(Hex_ipair.to_cell (i,j),content)) temp1 in 
  let all_used_labels=Listennou.nonredundant_version(Image.image snd temp2) in 
  let temp3=Image.image (fun c0->
     (c0,Option.filter_and_unpack (fun (cell,c)->if c=c0 then Some(cell) else None) temp2) 
  ) all_used_labels in 
  let (temp4,temp5)=List.partition (fun (c,l)->c=" A ") temp3 in 
  let active_ones = Hex_cell_set.safe_set  (snd (List.hd temp4)) in 
  let list_of_passive_pairs = Image.image (fun (c,l)->
     if List.length(l)<>2
     then raise(Unbalanced_label(c,l))
     else let tf=(fun j->List.nth l j) in
          (tf 0,tf 1)
     ) temp5  in  
 let passive_pairs = Hex_cell_pair_set.constructor list_of_passive_pairs in    
 (active_ones,passive_pairs);; 


let read_row_in_drawing row =
  let temp1=Str.split (Str.regexp_string "|") row in 
  let i=int_of_string(List.hd(temp1)) in 
  let temp2=Ennig.index_everything (List.tl temp1) in 
  Option.filter_and_unpack(
    fun (j,t)->
      if t=triple_blank
      then None
      else Some((i,j),t)
  ) temp2;;

let read_ascii_drawing s=
  let temp1= Lines_in_string.core s in 
  let temp2=Image.image (fun (_,line)->Cull_string.trim_spaces line) temp1 in 
  let temp3=List.filter (fun line->
    if line="" then false else (String.get line 0)<>'-'
  ) temp2 in 
  let temp4=Image.image read_row_in_drawing (Listennou.big_tail 2 temp3) in 
  let associations=List.flatten temp4 in 
  let diagonal_names=Cull_string.extract_intervals_in_wrt_separator (List.nth temp3 1) " " in 
  {
    Hex_ascii_grid_t.beneficiary = read_player (List.nth temp3 0);
    dimension = List.length (diagonal_names);
    data = associations ;
  };; 

let list_for_macros=[
   ("ddd", Hex_ipair.support_for_downwards_pyramid);
   ("lll", Hex_ipair.support_for_leftwards_pyramid);
   ("rrr", Hex_ipair.support_for_downwards_pyramid);
   ("uuu", Hex_ipair.support_for_leftwards_pyramid);
   
   ("bds", Hex_ipair.support_for_bs_downwards_claw);
   ("bls", Hex_ipair.support_for_bs_leftwards_claw);
   ("brs", Hex_ipair.support_for_bs_rightwards_claw);
   ("bus", Hex_ipair.support_for_bs_upwards_claw); 

   ("sdb", Hex_ipair.support_for_sb_downwards_claw);
   ("slb", Hex_ipair.support_for_sb_leftwards_claw);
   ("srb", Hex_ipair.support_for_sb_rightwards_claw);
   ("sub", Hex_ipair.support_for_sb_upwards_claw); 

];;

let list_of_default_labels = Ennig.doyle (fun j->
  let c=char_of_int(123-j) in " "^(String.make 1 c)^" "
) 1 56;; 

(*
let preprocess grid =
   let temp1=Image.image (fun p->let ((i,j),s)=p in 
     (p,Option.seek (fun (macro_text,support)->macro_text = s) list_for_macros)
   ) grid.Hex_ascii_grid_t.data in 
   let (non_macros1,macros1)=List.partition (fun (_,opt)->opt=None) temp1 in 
   let non_macros2=Image.image fst non_macros1 in 
   let macros2=Image.image (fun (q,opt)->let p=fst q in (p,(Option.unpack opt) p) ) macros1 in 
   let labels_used_by_nonmacros = Tidel.safe_set(Option.filter_and_unpack
     (fun (_,s)->if s=triple_blank then None else Some(s)) non_macros2) in 
   let unused_labels = List.filter (fun x->Tidel.nelfenn x labels_used_by_nonmacros) list_of_default_labels in 
   let fourtuples = List.flatten(Image.image snd macros2) in
   let labeled_fourtuples = Listennou.unequal_combine_where_fst_is_smallest fourtuples unused_labels in 
   let overrider1=List.flatten(Image.image (fun ((i1,j1,i2,j2),s)->[((i1,j1),s);((i2,j2),s)]) labeled_fourtuples)
   and overrider2=Image.image (fun (p,_)->(p," A ")) macros1 in 
   let overrider=overrider1@overrider2 in 
   let final_map=Associative_list.override_with non_macros2 overrider in 
   {
     grid with 
     data = final_map
   }

   and  
*)

end ;;

let make_ready_for_editing = Private.make_ready_for_editing;;
let of_basic_linker = Private.of_basic_linker;;
let of_finished_game = Private.of_finished_game;;
let read_ascii_drawing = Private.read_ascii_drawing ;;
let to_basic_linker = Private.to_basic_linker;;
let visualize = Private.visualize;;


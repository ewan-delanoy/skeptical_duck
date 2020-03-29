(* 

#use"Hex_analysis/hex_ascii_grid.ml";;

*)


exception Cell_too_wide of string;;


module Private = struct 


let triple_blank = String.make 3 ' ';;

let get grid (i,j)=
   try List.assoc (i,j) grid.Hex_ascii_grid_t.data with 
   _->triple_blank;;

let of_flattened_end_strategy fles =
  let formal_dim=fles.Hex_flattened_end_strategy_t.dimension in 
  let (Hex_dimension_t.D dim)=formal_dim in 
  let square = Cartesian.square (Ennig.ennig 1 dim) in
  let tracer1 =  (fun (i,j)->
     let cell=Hex_cell.of_int_pair (i,j) in 
     if Hex_cell_set.mem cell (Hex_flattened_end_strategy_field.active_part fles)
     then " A "
     else 
     if Hex_cell_set.mem cell (Hex_flattened_end_strategy_field.passive_part fles)
     then " P "
     else "   "
  ) in 
  let tracer2=(fun pair->
     let t=tracer1 pair in 
     if Cull_string.trim_spaces(t)=""
     then None
     else Some(pair,tracer1 pair)) in 
  {
    Hex_ascii_grid_t.beneficiary = (Hex_flattened_end_strategy_field.beneficiary fles);
    dimension = formal_dim;
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

let path_for_sheet = Absolute_path.of_string "Hex_analysis/hex_config_sheet.txt";;

let print_on_sheet_for_editing grid =
    let drawing = to_ascii_drawing grid in 
    let assignment = "\n\n\n"^drawing^"\n\n\n" in 
    Io.overwrite_with path_for_sheet assignment;;



let read_player s=
  if Substring.is_a_substring_of "Player 1" s 
  then Hex_player_t.First_player
  else Hex_player_t.Second_player;;


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




exception Unbalanced_label of string * (Hex_cell_t.t list);;


let to_molecular_linker_with_active_points grid =
    let temp6=List.filter (fun (p,s)->not(List.mem s ["eee";"EEE"])) grid.Hex_ascii_grid_t.data in
    let temp7=Image.image (fun (p,s) -> 
       (Hex_atomic_linker.opt_eyed grid.Hex_ascii_grid_t.dimension (p,s),(p,s)) ) temp6 in 
    let (temp8,temp9) = List.partition (fun (opt,_)->opt=None) temp7 in 
    let eyed_claws = Image.image (fun (opt,_)->Option.unpack opt) temp9 in 
    let temp2=Image.image (fun (_,((i,j),content))->
        (Hex_cell.of_int_pair (i,j),Cull_string.trim_spaces content)) temp8 in 
    let all_used_labels=Listennou.nonredundant_version(Image.image snd temp2) in 
    let temp3=Image.image (fun c0->
      (c0,Option.filter_and_unpack (fun (cell,c)->if c=c0 then Some(cell) else None) temp2) 
    ) all_used_labels in 
    let (temp4,temp5)=List.partition (fun (c,l)->c="A") temp3 in 
    let active_ones_from_eyed_claws = Image.image (
        fun (_,(p,_))->Hex_cell.of_int_pair p
    ) temp9 
    and mainstream_active_ones = snd (List.hd temp4) in 
    let active_ones = Hex_cell_set.safe_set ( mainstream_active_ones @ active_ones_from_eyed_claws) in 
    let pairs = Image.image (fun (c,l)->
     if List.length(l)<>2
     then raise(Unbalanced_label(c,l))
     else let tf=(fun j->List.nth l j) in
          Hex_atomic_linker.pair (tf 0,tf 1)
     ) temp5  in    
     (Hex_molecular_linker.constructor(pairs@eyed_claws),active_ones);;   




let read_row_in_drawing row =
  let temp1=Str.split (Str.regexp_string "|") row in 
  let i=int_of_string(List.hd(temp1)) in 
  let temp2=Ennig.index_everything (List.tl temp1) in 
  Option.filter_and_unpack(
    fun (j,t)->
      if (Cull_string.trim_spaces t)=""
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
    dimension = (Hex_dimension_t.D(List.length (diagonal_names)));
    data = associations ;
  };; 

let down = Hex_cardinal_direction_t.Down;;
let left = Hex_cardinal_direction_t.Left;;
let right = Hex_cardinal_direction_t.Right;;
let up = Hex_cardinal_direction_t.Up;;
let bs = Hex_double_hump_qualifier_t.Big_followed_by_small;;
let sb = Hex_double_hump_qualifier_t.Small_followed_by_big;;

let bridges_in_noneyed_claw dh d apex=
   let ipairs = Hex_connector_data.advanced_noneyed_claw dh d apex in 
   Listennou.extract_successive_pairs_from_even_list ipairs;;

let bridges_in_pyramid d apex=
   let ipairs = Hex_connector_data.advanced_pyramid d apex in 
   Listennou.extract_successive_pairs_from_even_list ipairs;;



let list_for_macros=[
   ("ddd", bridges_in_pyramid down);
   ("lll", bridges_in_pyramid left);
   ("rrr", bridges_in_pyramid right);
   ("uuu", bridges_in_pyramid up);
   
   ("bds", bridges_in_noneyed_claw bs down);
   ("bls", bridges_in_noneyed_claw bs left);
   ("brs", bridges_in_noneyed_claw bs right);
   ("bus", bridges_in_noneyed_claw bs up); 

   ("sdb", bridges_in_noneyed_claw sb down);
   ("slb", bridges_in_noneyed_claw sb down);
   ("srb", bridges_in_noneyed_claw sb down);
   ("sub", bridges_in_noneyed_claw sb down); 
];;

let list_of_default_labels = Ennig.doyle (fun j->
  let c=char_of_int(123-j) in " "^(String.make 1 c)^" "
) 1 56;; 

let trim = Cull_string.trim_spaces;;

let force_length_three s =
   let t=trim s in 
   match String.length(t) with 
   0->triple_blank
   |1->" "^t^" " 
   |2->t^" "
   |3->t
   |_->raise(Cell_too_wide(s));;

let detect_eyed_claws dim l=
   Option.filter_and_unpack (
      fun (p,s)->match Hex_atomic_linker.opt_eyed dim (p,s) with 
      None -> None 
      |Some(claw)->Some(claw,p)
   ) l ;;

let preprocess grid =
   let data1 = grid.Hex_ascii_grid_t.data in
   let data2 = Image.image (fun (p,s)->(p,force_length_three s) ) data1 in 
   let data3 = List.filter (fun (p,s)-> (s<>"EEE") ) data2 in 
   let claws = detect_eyed_claws grid.Hex_ascii_grid_t.dimension data3 in 
   let temp1=Image.image (fun p->let ((i,j),s)=p in 
     (p,List.assoc_opt (trim s) list_for_macros)
   ) data3 in 
   let (non_macros1,macros1)=List.partition (fun (_,opt)->opt=None) temp1 in 
   if (macros1=[])&&(claws=[]) then {grid with Hex_ascii_grid_t.data=data3} else 
   let non_macros2=Image.image fst non_macros1 in 
   let macros2=Image.image (fun (q,opt)->let p=fst q in (p,(Option.unpack opt) p) ) macros1 in 
   let labels_used_by_nonmacros = Set_of_polys.safe_set(Option.filter_and_unpack
     (fun (_,s)->if Cull_string.trim_spaces s="" then None else Some(s)) non_macros2) in 
   let powder_from_claws =List.flatten (Image.image (fun (claw,p)->Hex_atomic_linker.ipair_support claw) claws) in   
   let unused_labels = List.filter (fun x->Set_of_polys.nmem x labels_used_by_nonmacros) list_of_default_labels in 
   let fourtuples = List.flatten(Image.image snd macros2) in
   let labeled_fourtuples = Listennou.unequal_combine_where_fst_is_smallest fourtuples unused_labels in 
   let overrider1=List.flatten(Image.image (fun (((i1,j1),(i2,j2)),s)->[((i1,j1),s);((i2,j2),s)]) labeled_fourtuples)
   and overrider2=Image.image (fun (p,_)->(fst p," A ")) (macros1) 
   and overrider3=Image.image (fun p->(p,"eee")) powder_from_claws in 
   let overrider=overrider1@overrider2@overrider3 in 
   let final_map=Associative_list.override_with non_macros2 overrider in 
   {
     grid with 
     data = final_map
   };;

let empty_one = {
   Hex_ascii_grid_t.beneficiary = Hex_player_t.First_player ;
   dimension = Hex_dimension.eleven ;
   data = [];
};;

let ref_for_sheet_processing_error=ref(empty_one);;

let clear_sheet ()=
  print_on_sheet_for_editing (empty_one);;

let process_sheet ()=
   let old_drawing = Io.read_whole_file path_for_sheet in 
   let old_grid = read_ascii_drawing old_drawing in 
   let _=(ref_for_sheet_processing_error:=old_grid) in 
   let new_grid = preprocess old_grid in 
   let _ = print_on_sheet_for_editing new_grid in 
   new_grid;;

let recover_unprocessed_grid ()=
   let old_grid = (!ref_for_sheet_processing_error) in 
   let _ = print_on_sheet_for_editing old_grid in 
   old_grid;;

let read_sheet ()=   read_ascii_drawing (Io.read_whole_file path_for_sheet);;


let name_for_eyed_claw d1 d2 =
   (Hex_cardinal_direction.for_eye_description d1)^"e"^
     (Hex_cardinal_direction.for_ground_description d2) ;;

let of_extended_molecular (dim,winner) extmol =
   let (Hex_molecular_linker_t.M  l)=extmol.Hex_extended_molecular_t.molecular_part 
   and (Hex_cell_set_t.S actv)=extmol.Hex_extended_molecular_t.active_part 
   and (Hex_cell_set_t.S passv)=extmol.Hex_extended_molecular_t.nonmolecular_passive_part in 
   let cti = Hex_cell.to_int_pair in 
   let pairs1 = Option.filter_and_unpack ( function
        (Hex_atomic_linker_t.Pair(cell1,cell2)) -> Some(cti cell1,cti cell2)
       |_->None
   ) l in 
   let pairs2 = Ennig.index_everything pairs1 in 
   let pairs = List.flatten(Image.image (fun
      (j,(ipair1,ipair2))->
        let label = " "^(String.make 1 (char_of_int(96+j)))^" " in 
        [ipair1,label;ipair2,label]
   ) pairs2) in 
   let eyes1 = Option.filter_and_unpack ( function
        (Hex_atomic_linker_t.Eyed_claw(d1,d2,cell)) -> Some(d1,d2,cell)
       |_->None
   ) l in 
   let eyes2 = Image.image (
      fun (d1,d2,cell) -> 
         let ipair = cti cell in 
         (ipair,name_for_eyed_claw d1 d2)::
         (Image.image (fun p->(p,"eee")) (Hex_connector_data.advanced_eyed_claw d1 d2 ipair ))
   ) eyes1 in 
   let eyes = List.flatten eyes2 in 
   let actives = Image.image (fun cell->(cti cell," A ")) actv 
   and passives = Image.image (fun cell->(cti cell,"ppp")) passv in 
   {
      Hex_ascii_grid_t.beneficiary = winner ;
      dimension = dim ;
      data = pairs@eyes@actives@passives;
   };; 

let see_flesh fles= 
    let dim = fles.Hex_flattened_end_strategy_t.dimension 
    and winner = fles.Hex_flattened_end_strategy_t.beneficiary in 
    visualize (of_extended_molecular (dim,winner) fles.Hex_flattened_end_strategy_t.data);;

end ;;

let clear_sheet = Private.clear_sheet;;

let of_finished_game = Private.of_finished_game;;
let process_sheet = Private.process_sheet;;
let print_on_sheet_for_editing = Private.print_on_sheet_for_editing;;
let read_ascii_drawing = Private.read_ascii_drawing ;;
let read_sheet = Private.read_sheet;;
let recover_unprocessed_grid = Private.recover_unprocessed_grid;;
let see_flesh = Private.see_flesh ;;
let to_molecular_linker_with_active_points = Private.to_molecular_linker_with_active_points;;
let visualize = Private.visualize;;

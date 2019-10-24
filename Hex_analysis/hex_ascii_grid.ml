(* 

#use"Hex_analysis/hex_ascii_grid.ml";;

*)

module Private = struct 

let ipair_of_string s=
  let j=(int_of_char(String.get s 0))-96
  and i=int_of_string(Cull_string.cobeginning 1 s) in 
  (i,j);;

(* ipair_of_string "b5" = (5,2);; *)

let string_of_ipair (i,j)=
  (String.make 1 (char_of_int(j+96)))^(string_of_int i);;

let triple_blank = String.make 3 ' ';;

let get grid (i,j)=
   try List.assoc (i,j) grid.Hex_ascii_grid_t.data with 
   _->triple_blank;;

let of_flattened_end_strategy dim ec =
  let square = Cartesian.square (Ennig.ennig 1 dim) in
  let tracer1 =  (fun (i,j)->
     let cell=Hex_cell.of_string (string_of_ipair (i,j)) in 
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
     (List.assoc (i,j) grid.Hex_ascii_grid_t.data)^"|"
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

exception Unbalanced_label of string * (Hex_cell_t.t list);;

let to_basic_linker grid=
  let temp1= grid.Hex_ascii_grid_t.data in  
  let temp2=Image.image (fun ((i,j),content)->(Hex_cell.of_string (string_of_ipair (i,j)),content)) temp1 in 
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
    if line="" then false else
    let c=int_of_char(String.get line 0) in 
    (49<=c)&&(c<=57) (* meaning that c is a digit *)
  ) temp2 in 
  let temp4=Image.image read_row_in_drawing temp3 in 
  let associations=List.flatten temp4 in 
  let diagonal_names=Cull_string.extract_intervals_in_wrt_separator (List.nth temp3 1) " " in 
  {
    Hex_ascii_grid_t.beneficiary = read_player (List.nth temp3 0);
    dimension = List.length (diagonal_names);
    data = associations ;
  };; 


end ;;

let make_ready_for_editing = Private.make_ready_for_editing;;
let read_ascii_drawing = Private.read_ascii_drawing ;;
let visualize = Private.visualize;;


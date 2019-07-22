(* 

#use"Hex_analysis/hex_ascii_display.ml";;

*)

module Private = struct 

let ipair_of_string s=
  let j=(int_of_char(String.get s 0))-96
  and i=int_of_string(Cull_string.cobeginning 1 s) in 
  (i,j);;

(* ipair_of_string "b5" = (5,2);; *)

let string_of_ipair (i,j)=
  (String.make 1 (char_of_int(i+96)))^(string_of_int i);;


let trace_at_point ec (i,j) =
  let cell=Hex_cell.of_string (string_of_ipair (i,j)) in 
  if List.mem cell ec.Hex_end_configuration_t.active_part
  then "A"
  else 
  if List.mem cell ec.Hex_end_configuration_t.passive_part
  then "P"
  else " ";;

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

let dim = 11;;

let first_row = 
 let temp1=Ennig.doyle (fun j->
     " "^(String.make 1 (char_of_int(j+96)))^"  "
  ) 1 dim in 
  String.concat "" (" "::temp1)

let main_content_of_line ec line_idx =
  if line_idx=1
  then first_row
  else 
  if (line_idx mod 2)=0
  then String.make (4*dim+2) '-'
  else 
  let i=(line_idx-1)/2 in 
  let temp1=Ennig.doyle (fun j->
     " "^(trace_at_point ec (i,j))^" |"
  ) 1 dim in 
  String.concat "" ("|"::temp1);; 

let full_line ec line_idx = 
   constant_left_margin^
   (variable_left_margin line_idx)^
   (coordinate_mention line_idx)^ 
   (main_content_of_line ec line_idx);;

let to_ascii_drawing ec =
   let player = ec.Hex_end_configuration_t.beneficiary in
   "Config benefitting Player "^(Hex_player.to_string player)^"\n\n\n"^
   (String.concat "\n" (Ennig.doyle (full_line ec) 1 (2*dim+2)));;

let visualize ec = print_string("\n\n\n"^(to_ascii_drawing ec)^"\n\n\n");;

let make_ready_for_editing ec =
    let drawing = to_ascii_drawing ec in 
    let assignment = "\n\n\nlet z=\"\n\n\n"^drawing^"\n\n\n\";;\n\n\n" in 
    let ap=Absolute_path.of_string "Hex_analysis/hex_config_sheet.ml" in 
    Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string assignment) 
    ("(* Draft starts here *)","(* Draft ends here *)") ap;;



let read_player s=
  if Substring.is_a_substring_of "Player 1" s 
  then Hex_player_t.First_player
  else Hex_player_t.Second_player;;

let read_row_in_drawing row =
  let temp1=Str.split (Str.regexp_string "|") row in 
  let sj=List.hd(temp1) in 
  let temp2=Ennig.index_everything (List.tl temp1) in 
  Option.filter_and_unpack(
    fun (i,t)->
      let si=String.make 1 (char_of_int(96+i)) in 
      let cell_name=si^sj in 
      let cleaned_t=Cull_string.trim_spaces t in 
      if cleaned_t="A" then Some(cell_name,true) else 
      if cleaned_t="P" then Some(cell_name,false) else 
      None
  ) temp2;;

let gather_cells l=
   let temp1=Image.image (fun (cell_name,_)->Hex_cell.of_string cell_name) l in 
   Ordered.diforchan_plaen Hex_cell.cmp temp1;;

let read_ascii_drawing s=
  let temp1= Lines_in_string.core s in 
  let temp2=Image.image (fun (_,line)->Cull_string.trim_spaces line) temp1 in 
  let temp3=List.filter (fun line->
    if line="" then false else
    let c=int_of_char(String.get line 0) in 
    (49<=c)&&(c<=57)
  ) temp2 in 
  let temp4=Image.image read_row_in_drawing temp3 in 
  let temp5=List.flatten temp4 in 
  let (active1,passive1)=List.partition snd temp5 in 
  {
      Hex_end_configuration_t.beneficiary = (read_player s);
      Hex_end_configuration_t.active_part = gather_cells active1;
      Hex_end_configuration_t.passive_part = gather_cells passive1;
      Hex_end_configuration_t.index = 0
  };; 

end ;;

let make_ready_for_editing = Private.make_ready_for_editing;;
let read_ascii_drawing = Private.read_ascii_drawing;;



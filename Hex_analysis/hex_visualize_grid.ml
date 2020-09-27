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
   "Config benefitting Player "^(Hex_player.to_string player)^"\n\n\n"^
   (String.concat "\n" (Ennig.doyle (full_line (formal_dim,data)) 1 (2*dim+2)));;

let visualize grid = print_string("\n\n\n"^(to_ascii_drawing grid)^"\n\n\n");;


let name_for_eyed_claw d1 d2 =
   (Hex_cardinal_direction.for_eye_description d1)^"e"^
     (Hex_cardinal_direction.for_ground_description d2) ;;

let data_for_extended_molecular (dim,winner) extmol =
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

      

end ;;

let grid_of_extended_molecular = Private.data_for_extended_molecular ;;

let to_ascii_drawing = Private.to_ascii_drawing ;;

let triple_blank = Private.triple_blank ;;

let visualize = Private.visualize;;


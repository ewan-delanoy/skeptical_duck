(* 

#use"Hex_analysis/hex_ascii_grid.ml";;

*)


exception Cell_too_wide of string;;


module Private = struct 


let print_on_sheet_for_editing grid =
    let drawing = Hex_visualize_grid.visualization grid in 
    let assignment = "\n\n\n"^drawing^"\n\n\n" in 
    Hex_readable_and_writable_sheet.write  assignment;;

let read_player s=
  if Substring.is_a_substring_of "Player 1" s 
  then Hex_player_t.First_player
  else Hex_player_t.Second_player;;


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


let compute_translated_coordinates tbc side new_apex =
   let nc = Hex_typical_border_connector_name.full_constructor
       tbc side new_apex in 
   let ju = nc.Hex_connector_t.junction in     
   Listennou.extract_successive_pairs_from_even_list ju ;;  

let pyramid = Hex_typical_border_connector_name_t.Pyramid ;;
let byssus = Hex_typical_border_connector_name_t.Byssus ;;
let sybil = Hex_typical_border_connector_name_t.Sybil ;;

let list_for_macros=Image.image (fun (label,(tbc,side))->
   (label,compute_translated_coordinates tbc side)
) [
   ("ddd", (pyramid,down));
   ("lll", (pyramid,left));
   ("rrr", (pyramid,right));
   ("uuu", (pyramid,up));
   
   ("bds", (sybil,down));
   ("bls", (sybil,left));
   ("brs", (byssus,right));
   ("bus", (byssus,up)); 

   ("sdb", (byssus,down));
   ("slb", (byssus,left));
   ("srb", (sybil,right));
   ("sub", (sybil,up)); 
];;


let list_of_default_labels = Ennig.doyle (fun j->
  let c=char_of_int(123-j) in " "^(String.make 1 c)^" "
) 1 56;; 

let trim = Cull_string.trim_spaces;;

let force_length_three s =
   let t=trim s in 
   match String.length(t) with 
   0->Particular_string.triple_blank
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


let process_sheet ()=
   let old_drawing = Hex_readable_and_writable_sheet.read ()  in 
   let old_grid = read_ascii_drawing old_drawing in 
   let _=(ref_for_sheet_processing_error:=old_grid) in 
   let new_grid = preprocess old_grid in 
   let _ = print_on_sheet_for_editing new_grid in 
   new_grid;;

let recover_unprocessed_grid ()=
   let old_grid = (!ref_for_sheet_processing_error) in 
   let _ = print_on_sheet_for_editing old_grid in 
   old_grid;;

let read_sheet ()=   read_ascii_drawing (Hex_readable_and_writable_sheet.read ());;



end ;;


let process_sheet = Private.process_sheet;;
let to_molecular_linker_with_active_points = Private.to_molecular_linker_with_active_points;;


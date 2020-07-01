(*

#use"Text_editing/reindex_readiris_scans.ml";;

A readiris scan is rarely completely correct the first time.
The bad scans need to be redone. Hence the reindexing.



*)

let main_dir_path =ref(
  "/Users/ewandelanoy/Teuliou/html_files/PDF_files/"^
"Captured/From_IRIS/"
);;

let prefix_for_readiris_scan="IMAG";;
let suffix_for_readiris_scan=".PDF";;

let prefix_for_renamed_scan="u";;
let suffix_for_renamed_scan=".pdf";;


module Private = struct

let ap1=Absolute_path.of_string (!main_dir_path);;


let index_of_nonrenamed_file fn=
  if not(Supstring.begins_with fn prefix_for_readiris_scan) then None else
  if not(Supstring.ends_with fn suffix_for_readiris_scan) then None else
  let core = Cull_string.two_sided_cutting (prefix_for_readiris_scan,suffix_for_readiris_scan) fn in 
  try Some(int_of_string core) with
  _->None;;

let indices_of_nonrenamed_files ()=
  let temp1=More_unix.quick_beheaded_complete_ls (!main_dir_path) in 
  let temp2=Option.filter_and_unpack index_of_nonrenamed_file temp1 in 
  Set_of_polys.forget_order(Set_of_polys.sort(temp2));;

let index_of_renamed_file fn=
  if not(Supstring.begins_with fn prefix_for_renamed_scan) then None else
  if not(Supstring.ends_with fn suffix_for_renamed_scan) then None else
  let core = Cull_string.two_sided_cutting (prefix_for_renamed_scan,suffix_for_renamed_scan) fn in 
  try Some(int_of_string core) with
  _->None;;

let indices_of_renamed_files ()=
  let temp1=More_unix.quick_beheaded_complete_ls (!main_dir_path) in 
  let temp2=Option.filter_and_unpack index_of_renamed_file temp1 in 
  Set_of_polys.forget_order(Set_of_polys.sort(temp2));;

let missing_indices_in_renamed_files ()=
   let temp1=Set_of_polys.safe_set (indices_of_renamed_files ())
   and whole=Set_of_polys.safe_set (Ennig.ennig 1 500) in 
   let temp2=Set_of_polys.setminus whole temp1 in 
   Set_of_polys.forget_order temp2;;

module IndexPairList = struct

let append_pages_at_the_end ()=
  let l1=indices_of_nonrenamed_files()
  and l2=missing_indices_in_renamed_files () in
  let i1=List.hd l1
  and i2=List.hd l2 in 
  let d=i2-i1 in 
  Image.vorstellung (fun k->(k,k+d)) l1;;
  
let add_pages_in_between ()=
  let l1=indices_of_nonrenamed_files()
  and l2=missing_indices_in_renamed_files () in
  let n1=List.length l1 in 
  let shorter_l2=Listennou.big_head n1 l2 in 
  List.combine l1 shorter_l2;;
  
end;;


module Command=struct

let  put_index_on_nonrenamed_file (i,j)=
  let si=Strung.left_completed_string_of_int 4 i
  and sj=string_of_int j in
  "mv "^(!main_dir_path)^prefix_for_readiris_scan^si^suffix_for_readiris_scan^" "^
        (!main_dir_path)^prefix_for_renamed_scan^sj^suffix_for_renamed_scan;;

let from_list=Image.vorstellung put_index_on_nonrenamed_file ;;

let append_pages_at_the_end ()= from_list (IndexPairList.append_pages_at_the_end());;

let add_pages_in_between ()= from_list (IndexPairList.add_pages_in_between());;

end;;

let append_pages_at_the_end()=Image.vorstellung Sys.command (Command.append_pages_at_the_end());;

let add_pages_in_between()=Image.vorstellung Sys.command (Command.add_pages_in_between());;

let command_for_rm i=
   let si=Strung.left_completed_string_of_int 4 i in
  "rm "^(!main_dir_path)^"IMAG"^si^".PDF";;

let rm i=Sys.command(command_for_rm i);;

end;;

let append_pages_at_the_end = Private.append_pages_at_the_end;;
let add_pages_in_between = Private.add_pages_in_between;;
let remove_non_renamed_file =Private.rm;;


(*

put_index_on_raw_files 9 (Ennig.ennig 1 50);;

let s1=Strung.left_completed_string_of_int 4 1;;

let g1=More_unix.quick_beheaded_complete_ls (!main_dir_path);;
let g2=List.nth g1 32;;
let g3=String.sub g2 4 4;;
let g4=index_of_nonrenamed_file g2;;


*)

(*

let g1= indices_of_renamed_files();;
let g2= List.filter (fun x->x>=70) g1;;
let g3= List.rev_map (fun x->
   let si=Strung.left_completed_string_of_int 4 x
  and sj=Strung.left_completed_string_of_int 4 (x+1) in
  "mv "^(!main_dir_path)^"(!main_dir_path)_"^si^".pdf "^
        (!main_dir_path)^"(!main_dir_path)_"^sj^".pdf"
) g2;;
let g4=Image.vorstellung Sys.command g3;;


*)






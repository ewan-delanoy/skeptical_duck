(*

#use"find_suitable_ending.ml";;

*)

(*

Note that the order in Ocaml_ending.correspondances is important

*)

exception No_suitable_location of Dfa_root_t.t*(Dfa_subdirectory_t.t list)*string;;

let find_file_location dir l_subdir old_x=
  let x=String.uncapitalize_ascii old_x in
  let s_dir=Dfa_root.connectable_to_subpath(dir) in
  let original_endings=Image.image Dfa_ending.connectable_to_modulename Dfa_ending.all_ocaml_endings in
  let endings=(
     if List.exists (fun edg->Supstring.ends_with x edg) original_endings
     then [""]
     else original_endings
  ) in
  let temp1=Cartesian.product(l_subdir) endings in
  let tempf=(fun (sd,edg)->
  	let s1=s_dir^(Dfa_subdirectory.connectable_to_subpath sd)^x^edg in
  	if Sys.file_exists s1
  	then Some(Absolute_path.of_string s1)
  	else None
  ) in
  let opt=Option.force_find_and_stop tempf temp1 in
  if opt=None
  then raise(No_suitable_location(dir ,l_subdir,x))
  else  Option.unpack(opt);;           
(*

#use"mlx_ended_absolute_path.ml";;

*)

type t=MLX of Dfa_ending.t*string*Dfa_root_t.t;;

exception Unknown_ending of string;;
exception Unpointed_filename of string;;

exception Inexistent_filename of string;;

let short_path (MLX(edg,s,_))=match edg with
   Dfa_ending.Ml->  s^".ml"
  |Dfa_ending.Mli-> s^".mli"
  |Dfa_ending.Mll-> s^".mll"
  |Dfa_ending.Mly-> s^".mly";;

let to_string=short_path;;

let of_string_and_root s dir= 
  if not(String.contains s '.') then raise(Unpointed_filename(s)) else
  let (core,ending)=Cull_string.split_wrt_rightmost s '.' in
  let s_dir=Dfa_root.connectable_to_subpath dir in
  if (not(Sys.file_exists(s_dir^s)))
  then raise(Inexistent_filename(s_dir^s))
  else
  if ending="ml"  then MLX (Dfa_ending.ml,core,dir) else
  if ending="mli" then MLX (Dfa_ending.mli,core,dir) else
  if ending="mll" then MLX (Dfa_ending.mll,core,dir) else
  if ending="mly" then MLX (Dfa_ending.mly,core,dir) else
  raise(Unknown_ending(s));;

let try_from_string_and_root s dir=
  try (Some(of_string_and_root s dir)) with _->None;;


let root (MLX(_,_,dir))=dir;;

exception FileOutsideDirectory of Absolute_path.t*Dfa_root_t.t;;


let of_path_and_root ap dir=
   if (not(Supstring.begins_with (Absolute_path.to_string ap)
         (Dfa_root.connectable_to_subpath dir)))
    then raise(FileOutsideDirectory(ap,dir))
    else 
    let s_dir=Dfa_root.connectable_to_subpath dir in
    let n_dir=String.length s_dir in
    let subpath=Cull_string.cobeginning n_dir (Absolute_path.to_string ap) in
    of_string_and_root subpath dir;;    

let try_from_path_and_root ap dir=
    try (Some(of_path_and_root ap dir)) with _->None;;

let decompose (MLX(edg,s,dir))=
  (Dfn_with_ending_removed.of_string_and_root s dir,edg);;

let half_dressed_core mlx=fst(decompose mlx);;
let ending mlx=snd(decompose mlx);;


let to_path mlx=
  let (hm,edg)=decompose mlx in
  let dir=root mlx in
  let s_hm=Dfn_with_ending_removed.uprooted_version hm 
  and s_dir=Dfa_root.connectable_to_subpath dir in
  Absolute_path.of_string( s_dir^s_hm^(Dfa_ending.to_string edg) );;

let join hs ending=
  let (s,dir)=Dfn_with_ending_removed.unveil hs in
  MLX(ending,s,dir);;

  
exception Failed_File_Renaming of t*string;;  
  
(*

notice that the ending is preserved below. If the user
unwittingly puts a wrong ending, this will have no effect.

*)  
  
let do_file_renaming mlx new_name=
  let core=Cull_string.before_rightmost_possibly_all (No_slashes.to_string new_name) '.' in
  let checked_name=No_slashes.of_string(core^(Dfa_ending.to_string(ending mlx))) in
  let ap=to_path mlx in
  let new_ap=Rename_file.rename ap checked_name in
  of_path_and_root new_ap (root mlx);;   
  
let do_file_displacing mlx new_subdir=
  let s_new_subdir=Dfa_subdirectory.connectable_to_subpath new_subdir
  and dir=root mlx in
  let s_dir=Dfa_root.connectable_to_subpath dir in
  let new_dir=Dfa_root.of_string(s_dir^s_new_subdir) in
  let ap=to_path mlx in
  let new_ap=Dfa_root.relocate ap new_dir in
  of_path_and_root new_ap (root mlx);;  
  
let rename_endsubdirectory (subdir,newdirname) (MLX(edg,s,dir))=
  MLX(edg,Rename_endsubdirectory.re (subdir,newdirname) s,dir);;
  

let to_absolute_path mlx=
  let s=short_path mlx
  and dir=root mlx in
 let s_dir=Dfa_root.connectable_to_subpath dir in
 Absolute_path.of_string(s_dir^s);;   



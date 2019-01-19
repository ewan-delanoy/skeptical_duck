
(* 

#use"Country/Alaska/alaskan_write_merlinfile.ml";;


*)

let instructions main_root dirs=
  let s_root=Root_directory.connectable_to_subpath main_root in
  let temp1=Image.image 
   (fun sdir->"S "^s_root^(Subdirectory.connectable_to_subpath sdir) )
  (Coma_constant.automatically_generated_subdir::dirs) in
  let temp2=("B "^s_root^"_build/")::temp1 in
  "\n\n\n"^(String.concat "\n" temp2)^"\n\n\n";; 

           
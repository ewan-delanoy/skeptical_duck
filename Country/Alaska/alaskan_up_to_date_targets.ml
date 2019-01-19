
(* 

#use"Country/Alaska/alaskan_up_to_date_targets.ml";;


*)


let loadings (main_root,name_for_loadingsfile) (dirs,hms)=
  let s_root=Root_directory.connectable_to_subpath main_root in
  let part1="\n(*\n #use\""^s_root^(name_for_loadingsfile)^"\";"^";\n*)\n\n" in
  let temp5=Image.image (
     fun sd->
     "#directory\""^s_root^(Subdirectory.connectable_to_subpath sd)^"\";"^";"
  ) ((Subdirectory.of_string "_build")::dirs) in
  let part2=String.concat "\n" temp5 
  and part3="\n\n#load\"str.cma\";"^";\n#load\"unix.cma\";"^";\n\n\n" in
  let temp2=Image.image (
    function hm->
      let s=Father_and_son.son (Half_dressed_module.uprooted_version hm) '/' in
      "#load\""^s^".cmo\";"^";"
  ) hms in
  let temp3="\n\n\n"::temp2 in
  let part4=String.concat "\n" temp3 
  and part5="\n\n\n" in
  part1^part2^part3^part4^part5;; 
          
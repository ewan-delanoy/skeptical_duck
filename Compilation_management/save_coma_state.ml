
(* 

#use"Compilation_management/save_coma_state.ml";;

*)

module Private=struct
  
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
          let s=Cull_string.after_rightmost (Half_dressed_module.uprooted_version hm) '/' in
          "#load\""^s^".cmo\";"^";"
      ) hms in
      let temp3="\n\n\n"::temp2 in
      let part4=String.concat "\n" temp3 
      and part5="\n\n\n" in
      part1^part2^part3^part4^part5;; 
          
    
    let instructions_for_merlinfile main_root dirs=
      let s_root=Root_directory.connectable_to_subpath main_root in
      let temp1=Image.image 
        (fun sdir->"S "^s_root^(Subdirectory.connectable_to_subpath sdir) )
      (Coma_constant.automatically_generated_subdir::dirs) in
      let temp2=("B "^s_root^"_build/")::temp1 in
      "\n\n\n"^(String.concat "\n" temp2)^"\n\n\n";; 

    let instructions_for_printersfile printer_equipped_types=
        let temp2=List.rev_map (
          function (x,compiled_correctly)->
          if compiled_correctly 
          then "#install_printer "^(Half_dressed_module.capitalized_module_name x)^".print_out;"^";"
          else ""
        ) printer_equipped_types in
        let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
        let part2=String.concat "\n" temp3 in
        part2;;  

    let save_loadingsfile (root,location_for_loadingsfile) (dirs,hms)=
       let path_for_loadingsfile=
           (Subdirectory.connectable_to_subpath Coma_constant.automatically_generated_subdir)^
           location_for_loadingsfile in
       let s=loadings (root,location_for_loadingsfile)
        (dirs,hms)
       and lm=Root_directory.force_join root  path_for_loadingsfile in
       Io.overwrite_with (Absolute_path.of_string lm) s;;
    
    let save_merlinfile (root,location_for_merlinfile) dirs=
        let s=instructions_for_merlinfile root dirs 
        and lm=Root_directory.force_join root  location_for_merlinfile in
        Io.overwrite_with (Absolute_path.of_string lm) s;;
  
    let save_printersfile (root,location_for_printersfile) printer_equipped_types=
       let init_dir=
        Subdirectory.connectable_to_subpath 
        (Coma_constant.automatically_generated_subdir) in
       let s=instructions_for_printersfile printer_equipped_types
       and lm=Root_directory.force_join root  (init_dir^location_for_printersfile) in
       let beg_mark="(*Registered printers start here *)"
       and end_mark="(*Registered printers end here *)" in
       Replace_inside.overwrite_between_markers_inside_file
       (Overwriter.of_string s)
       (beg_mark,end_mark)
       (Absolute_path.of_string lm);;
    
    
  
    let save_targetfile location_for_targetfile cs=
      let root_dir = Coma_state.root cs in 
      let s1=Coma_state_field.archive cs in
      let lt=Root_directory.force_join root_dir location_for_targetfile in
      Io.overwrite_with (Absolute_path.of_string lt) s1;;
    
    
    
    let write_all 
    (
      location_for_targetfile,
      location_for_loadingsfile,
      location_for_printersfile
      )
      uple= 
      let (cs,directories,printer_equipped_types)=uple in
      let root_dir = Coma_state.root cs in  
      let hms=Coma_state.up_to_date_hms cs in 
       (
        save_merlinfile (root_dir,Coma_constant.name_for_merlinfile) directories;
        save_loadingsfile (root_dir,location_for_loadingsfile) (directories,hms);
        save_targetfile location_for_targetfile cs;
        save_printersfile (root_dir,location_for_printersfile) printer_equipped_types;
       );;
    
    let save_all cs=write_all 
      (
        Coma_constant.name_for_targetfile,
        Coma_constant.name_for_loadingsfile,
        Coma_constant.name_for_printersfile
      )
      (
	      Coma_state.uple_form cs
      );;

end;;  

let save = Private.save_all;;
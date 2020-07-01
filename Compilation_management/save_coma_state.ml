
(* 

#use"Compilation_management/save_coma_state.ml";;

*)

module Private=struct
  
    let loadings (main_root,rootless_path_for_loadingsfile) (dirs,hms)=
      let path_for_loadingsfile = Dfn_rootless.to_line rootless_path_for_loadingsfile in 
      let s_root=Dfa_root.connectable_to_subpath main_root in
      let part1="\n(*\n #use\""^s_root^(path_for_loadingsfile)^"\";"^";\n*)\n\n" in
      let temp5=Image.image (
        fun sd->
        "#directory\""^s_root^(Dfa_subdirectory.connectable_to_subpath sd)^"\";"^";"
      ) ((Dfa_subdirectory.of_line "_build")::dirs) in
      let part2=String.concat "\n" temp5 
      and part3="\n\n#load\"str.cma\";"^";\n#load\"unix.cma\";"^";\n\n\n" in
      let temp2=Image.image (
        function hm->
          let nm = Dfn_endingless.to_module hm in  
          let s=Cull_string.after_rightmost (Dfa_module.to_line nm) '/' in
          "#load\""^s^".cmo\";"^";"
      ) hms in
      let temp3="\n\n\n"::temp2 in
      let part4=String.concat "\n" temp3 
      and part5="\n\n\n" in
      part1^part2^part3^part4^part5;; 
          
    
    let instructions_for_merlinfile main_root dirs=
      let s_root=Dfa_root.connectable_to_subpath main_root in
      let temp1=Image.image 
        (fun sdir->"S "^s_root^(Dfa_subdirectory.connectable_to_subpath sdir) )
      (Coma_constant.automatically_generated_subdir::dirs) in
      let temp2=("B "^s_root^"_build/")::temp1 in
      "\n\n\n"^(String.concat "\n" temp2)^"\n\n\n";; 

    let instructions_for_printersfile printer_equipped_types=
        let temp2=List.rev_map (
          function (x,compiled_correctly)->
          if compiled_correctly 
          then let modname=Dfn_endingless.to_module x in 
               "#install_printer "^(Dfa_module.capitalized_form modname)^".print_out;"^";"
          else ""
        ) printer_equipped_types in
        let temp3="\n\n\n"::(List.rev ("\n\n\n"::temp2)) in
        let part2=String.concat "\n" temp3 in
        part2;;  

    let save_loadingsfile (root,rootless_path_for_loadingsfile) (dirs,hms)=
       let s=loadings (root,rootless_path_for_loadingsfile)
        (dirs,hms)
       and lm=Dfn_join.root_to_rootless root rootless_path_for_loadingsfile in
       Io.overwrite_with (Dfn_full.to_absolute_path lm) s;;
    
    let save_merlinfile (root,rootless_path_for_merlinfile) dirs=
        let s=instructions_for_merlinfile root dirs 
        and lm=Dfn_join.root_to_rootless root rootless_path_for_merlinfile in
        Io.overwrite_with (Dfn_full.to_absolute_path lm) s;;
  
    let save_printersfile (root,rootless_path_for_printersfile) printer_equipped_types=
       let s=instructions_for_printersfile printer_equipped_types
       and lm=Dfn_join.root_to_rootless root rootless_path_for_printersfile in
       let beg_mark="(*Registered printers start here *)"
       and end_mark="(*Registered printers end here *)" in
       Replace_inside.overwrite_between_markers_inside_file
       (Overwriter.of_string s)
       (beg_mark,end_mark)
       (Dfn_full.to_absolute_path lm);;
    
    
  
    let save_targetfile rootless_path_for_targetfile cs=
      let root_dir = Coma_state.root cs in 
      let s1=Crobj_parsing.unparse(Coma_state_field.to_concrete_object cs) in
      let lt=Dfn_join.root_to_rootless root_dir rootless_path_for_targetfile in
      Io.overwrite_with (Dfn_full.to_absolute_path lt) s1;;
    
    
    
    let write_all 
    (
      rootless_path_for_targetfile,
      rootless_path_for_loadingsfile,
      rootless_path_for_printersfile
      )
      uple= 
      let (cs,directories,printer_equipped_types)=uple in
      let root_dir = Coma_state.root cs in  
      let hms=Coma_state.up_to_date_elesses cs in 
       (
        save_loadingsfile (root_dir,rootless_path_for_loadingsfile) (directories,hms);
        save_targetfile rootless_path_for_targetfile cs;
        save_printersfile (root_dir,rootless_path_for_printersfile) printer_equipped_types;
       );;
    
    let save_all cs=write_all 
      (
        Coma_constant.rootless_path_for_targetfile,
        Coma_constant.rootless_path_for_loadingsfile,
        Coma_constant.rootless_path_for_printersfile
      )
      (
	      Coma_state.uple_form cs
      );;

end;;  

let save = Private.save_all;;
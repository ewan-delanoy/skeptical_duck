(* 

#use"lib/Filewatching/fw_persisting.ml";;

*)

module Private=struct

  let building_site =  Fw_constant.usual_build_subdir ;;

  let loadings (main_root,rootless_path_for_loadingsfile) (dirs,hms)=
      let path_for_loadingsfile = Dfn_rootless.to_line rootless_path_for_loadingsfile in 
      let s_root=Dfa_root.connectable_to_subpath main_root in
      let part1="\n(*\n #use\""^s_root^(path_for_loadingsfile)^"\";"^";\n*)\n\n" in
      let temp5=Image.image (
        fun sd->
        "#directory\""^s_root^(Dfa_subdirectory.connectable_to_subpath sd)^"\";"^";"
      ) (building_site::dirs) in
      let part2=String.concat "\n" temp5 
      and part3="\n\n#load\"str.cma\";"^";\n#load\"unix.cma\";"^";\n#load\"zarith.cma\";"^";\n\n\n" in
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
      dirs in
      let temp2=("B "^s_root^(Dfa_subdirectory.connectable_to_subpath building_site))::temp1 in
      "\n\n\n"^(String.concat "\n" temp2)^"\n\n\n";; 

    let instructions_for_printersfile registered_printers=
      let temp2=List.rev_map (
        fun (_idx,printer_path) ->
        "#install_printer "^printer_path^";"^";"
      ) registered_printers in
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
  
    let save_printersfile (root,rootless_path_for_printersfile) registered_printers=
       let s=instructions_for_printersfile registered_printers
       and lm=Dfn_join.root_to_rootless root rootless_path_for_printersfile in
       let beg_mark="(*Registered printers start here *)"
       and end_mark="(*Registered printers end here *)" in
       Replace_inside.overwrite_between_markers_inside_file
       ~overwriter:s
       (beg_mark,end_mark)
       (Dfn_full.to_absolute_path lm);;
    
  
    let save_targetfile rootless_path_for_targetfile root_dir crobj_form=
      let s1=Crobj_parsing.unparse crobj_form in
      let lt=Dfn_join.root_to_rootless root_dir rootless_path_for_targetfile in
      Io.overwrite_with (Dfn_full.to_absolute_path lt) s1;;
    
    
    
    let write_all 
    (
      rootless_path_for_targetfile,
      rootless_path_for_loadingsfile,
      rootless_path_for_printersfile
      )
      (root_dir,elesses,crobj_form,directories,registered_printers) = 
       (
        save_loadingsfile (root_dir,rootless_path_for_loadingsfile) (directories,elesses);
        save_targetfile rootless_path_for_targetfile root_dir crobj_form;
        save_printersfile (root_dir,rootless_path_for_printersfile) registered_printers;
       );;

      
    let save_all fw=
      let root_dir = Fwc_with_githubbing.Inherited.root fw 
      and elesses = Image.image 
      (Fwc_with_githubbing.Inherited.endingless_at_module fw)
      (Fwc_with_githubbing.Inherited.dep_ordered_modules fw)
      and crobj_form = Fwc_with_githubbing.Crobj.to_concrete_object fw 
      and directories = Fwc_with_githubbing.Inherited.all_subdirectories fw 
      and registered_printers = Fwc_with_githubbing.Inherited.registered_printers fw 
        in
       write_all 
      (
        Fw_constant.rootless_path_for_targetfile,
        Fw_constant.rootless_path_for_loadingsfile,
        Fw_constant.rootless_path_for_printersfile
      )
	      (root_dir,elesses,crobj_form,directories,registered_printers)
      ;;

    let load_persisted_version fw=
      let full_path=Dfn_join.root_to_rootless (Fwc_with_githubbing.Inherited.root fw)  Fw_constant.rootless_path_for_targetfile in
      let ap= Dfn_full.to_absolute_path full_path in
      let the_archive=Io.read_whole_file ap in
      let archived_object = Crobj_parsing.parse the_archive in 
      Fwc_with_githubbing.Crobj.of_concrete_object archived_object;;   

end;;  


let load_persisted_version = Private.load_persisted_version ;;
let persist = Private.save_all;;



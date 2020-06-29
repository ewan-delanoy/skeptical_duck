(*

#use"modularize.ml";;

*)


let module_name_from_path ap=
      let s_ap=Absolute_path.to_string ap in
      let pointed_name=Cull_string.after_rightmost s_ap '/' in
      let name=Cull_string.before_rightmost_possibly_all pointed_name '.' in
      Dfa_module.of_line name;;


let modularize prefix ap=
      let naked_name=module_name_from_path ap in
      let name=Dfa_module.to_line naked_name in  
      let content=Io.read_whole_file(ap) in
      let new_name=String.capitalize_ascii(prefix^name) in
      let new_content=
       "\n\nmodule "^new_name^"=struct\n\n"^content^"\n\nend;"^";\n\n" in
     new_content;;
     
let modularize_several prefix l_ap=     
    let temp1=Image.imagination module_name_from_path l_ap in
    let replacements = Image.imagination (
        fun x->
        (x,Dfa_module.add_prefix_and_capitalize prefix x)
    ) temp1 in
    let temp2=Explicit.image (modularize prefix) l_ap in
    let unreplaced_text=String.concat "\n\n\n" temp2 in
    let walker=ref(unreplaced_text) 
    and m=List.length(replacements) in
    let _=(for k=1 to m do
      let (a,b)=List.nth replacements (k-1) in
      let message=(string_of_int k)^" of "^(string_of_int m)^" done \n" in
      walker:=Look_for_module_names.change_module_name_in_ml_ocamlcode
       a b (!walker);
      print_string(message);
      flush stdout 
    done) in
    (!walker);;  
 

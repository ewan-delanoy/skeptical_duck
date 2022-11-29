(*

#use"lib/Ocaml_analysis/compute_all_ocaml_items.ml";;

if the string argument has a dot inside it, we interpret it
as a value inside a module.
Otherwise we interpret it as a mere string.

*)


let caoi mdata=
   Read_ocaml_files.read_ocaml_files 
   (Fw_with_dependencies.all_moduled_ml_absolute_paths mdata)
  ;;
   
  
           
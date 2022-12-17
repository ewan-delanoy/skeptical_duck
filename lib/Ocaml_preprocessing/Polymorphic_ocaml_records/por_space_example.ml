(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_space_example.ml";;

*)

let filewatching = 
  let home = Sys.getenv "HOME" in 
  let file_there = (fun s-> 
    Absolute_path.create_file_if_absent(home^"/Teuliou/OCaml/skeptical_duck/lib/Filewatching/"^s^".ml")) in 
  ref({
   Por_space_t.main_type_name = "t" ;
   module_name = "gw_poly" ;
   subclasses = [] ;
   type_signature_file = (file_there "gw_poly_t") ;
   implementation_file = (file_there "gw_poly") ;
   has_crobj_conversion = true ;
   incomplete_extensions = [];
  }) ;;  

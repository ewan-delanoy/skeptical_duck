(*

#use"lib/Ocaml_preprocessing/Polymorphic_ocaml_records/por_space_example.ml";;

*)


let filewatching = 
      let home = Sys.getenv "HOME" in 
      let file_there = (fun s-> 
            Absolute_path.create_file_if_absent(home^
            "/Teuliou/OCaml/skeptical_duck/lib/Filewatching/"^s^".ml")) in       
      
{
      Por_types.space_name = "Filewatching" ;
      Por_types.fields = [] ;
      Por_types.records = [] ;
      Por_types.type_signature_file = (file_there "gw_poly_t") ;
      Por_types.implementation_file = (file_there "gw_poly") ;
      Por_types.has_crobj_conversion = true ;
} ;;
   

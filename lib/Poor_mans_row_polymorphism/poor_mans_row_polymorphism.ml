(*

#use"lib/Poor_mans_row_polymorphism/poor_mans_row_polymorphism.ml";;

*)

let ml_content config= 
  "\n\n\n"^  
  "type t = {\n" ^
  (
   String.concat "\n" (Image.image
   (fun (Pmrp_field_t.F(s),t)->"   "^s^" : "^t^" ;")
   config.Pmrp_config_t.fields_with_their_types)
  ) ^
  "\n} ;;\n"^
  "type 'a method_t = M of (Pmrp_field_t.t list) * ( t -> 'a) ;; " ^
  "\n\n\n";;


let write_ml_content config ap = 
  let _ = Io.overwrite_with ap (ml_content config) in 
  Use_directive_in_initial_comment.prepend_or_replace_with_usual
   Coma_big_constant.This_World.root (config.Pmrp_config_t.receiving_file) ;;


let config1 = {
  Pmrp_config_t.fields_with_their_types = [
  Pmrp_field_t.F "apple", "int";
  Pmrp_field_t.F "pear", "string";
  ];
  fieldsets_with_their_names=[];
  receiving_file = (Absolute_path.of_string 
  "lib/Poor_mans_row_polymorphism/pmrp_guinea_pig.ml");
} ;; 

let act1 () = write_ml_content config1 ;; 


  
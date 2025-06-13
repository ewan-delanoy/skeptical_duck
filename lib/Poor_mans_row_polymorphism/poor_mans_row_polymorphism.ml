(*

#use"lib/Poor_mans_row_polymorphism/poor_mans_row_polymorphism.ml";;

*)

let ds = ";" ^ ";\n\n" ;;

let protect t = 
    if String.contains t ' ' then "("^t^")" else t
;;

let code_for_getters config =
  let lines = Image.image (
    fun (Pmrp_field_t.F(s),t)->"let "^s^
    " = { \n"^
  " sto_input_fieldset = [ Pmrp_field_t.F \""^s^"\" ] ;\n"^
  " sto_additional_info = Some \"(getter)\" ;\n"^
  " sto_output_type = \""^t^"\" ;\n"^
  " sto_actor = (fun x ->x."^s^");\n"^
  "} " ^ds
  ) config.Pmrp_config_t.fields_with_their_types in 
  String.concat "" lines ;;

(* let code_for_setters config =
  let lines = Image.image (
      fun (Pmrp_field_t.F(s),_)->"let "^s^" = Me([ (Pmrp_field_t.F \""^s^"\") ],(fun x ->x."^s^")) "^ds
  ) config.Pmrp_config_t.mutable_fields in 
  String.concat "" lines ;; *)
  


let ml_content config= 
  
  "\n\n\n"^  
  "type t = {\n" ^
  (
   String.concat "\n" (Image.image
   (fun (Pmrp_field_t.F(s),t)->"   "^s^" : "^t^" ;")
   config.Pmrp_config_t.fields_with_their_types)
  ) ^
  "\n} "^ds^
  "type 'a outside_to_self_mapper_t   = { \n"^
  " ots_input_fieldset : string ;\n"^
  " ots_output_fieldset : Pmrp_field_set_t.t ;\n"^
  " ots_additional_info : string option ;\n"^
  " ots_actor: 'a -> t; "^
  "} " ^ds^
  "type 'b self_to_outside_mapper_t   = { \n"^
  " sto_input_fieldset : Pmrp_field_set_t.t ;\n"^
  " sto_output_type : string ;\n"^
  " sto_additional_info : string option ;\n"^
  " sto_actor: t -> 'b; "^
  "} " ^ds^
  "type self_to_self_mapper_t   = { \n"^
  " sts_input_fieldset : Pmrp_field_set_t.t ;\n"^
  " sts_output_fieldset : Pmrp_field_set_t.t ;\n"^
  " sts_additional_info : string option ;\n"^
  " sts_actor: t -> t; "^
  "} " ^ds^
  "let apply_ots f x   = f.ots_actor x " ^ds^
  "let apply_sto f x   = f.sto_actor x " ^ds^
  "let apply_sts f x   = f.sts_actor x " ^ds^
  (code_for_getters config)^
  "\n\n\n";;


let write_ml_content config = 
  let  ap = config.Pmrp_config_t.receiving_file in
  let _ = Io.overwrite_with ap (ml_content config) in 
  Use_directive_in_initial_comment.prepend_or_replace_with_usual
   Coma_big_constant.This_World.root ap ;;


let config1 = {
  Pmrp_config_t.fields_with_their_types = [
  Pmrp_field_t.F "apple", "int";
  Pmrp_field_t.F "pear", "string";
  Pmrp_field_t.F "cranberry", "float";
  Pmrp_field_t.F "strawberry", "int list";
  ];
  fieldsets=[];
  mutable_fields=[];
  receiving_file = (Absolute_path.of_string 
  "lib/Poor_mans_row_polymorphism/pmrp_guinea_pig.ml");
} ;; 

let act1 () = write_ml_content config1 ;; 


  
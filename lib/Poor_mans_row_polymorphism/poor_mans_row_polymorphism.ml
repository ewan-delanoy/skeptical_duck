(*

#use"lib/Poor_mans_row_polymorphism/poor_mans_row_polymorphism.ml";;

*)

let ds = ";" ^ ";\n\n" ;;

let protect t = 
    if String.contains t ' ' then "("^t^")" else t
;;

let code_for_getters config =
  let lines = Image.image (
    fun (Pmrp_field_t.F(s),_)->"let "^s^" = Me([ (Pmrp_field_t.F \""^s^"\") ],(fun x ->x."^s^")) "^ds
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
  "type 'b method_t   = { me_active_fields: Pmrp_field_t.t list ; me_actor: t -> 'b } " ^ds^
  "type modifier_t    = { mo_active_fields: Pmrp_field_t.t list ; mo_actor: t -> t } " ^ds^
  "type 'a extender_t = { ex_fields_before: Pmrp_field_t.t list ; ex_added_fields: Pmrp_field_t.t list ; ex_actor: t -> 'a -> t } " ^ds^
  "type shortener_t   = { sh_removed_fields: Pmrp_field_t.t list ; sh_actor: t -> t } " ^ds^
  "let apply_method    mthd x   = mthd.me_actor x " ^ds^
  "let apply_modifier  mdfr x   = mdfr.mo_actor x " ^ds^
  "let apply_extender  extr x y = extr.ex_actor x y " ^ds^
  "let apply_shortener shnr x   = shnr.sh_actor x " ^ds^
  (* "let apply_extender (Ex(Pmrp_field_t.F(s),f=v)) x = {x with "^s^" = v} " ^ds^ *)
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
  fieldsets_with_their_names=[];
  mutable_fields=[];
  receiving_file = (Absolute_path.of_string 
  "lib/Poor_mans_row_polymorphism/pmrp_guinea_pig.ml");
} ;; 

let act1 () = write_ml_content config1 ;; 


  
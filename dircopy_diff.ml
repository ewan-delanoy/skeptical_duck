(*

#use"dircopy_diff.ml";;

*)


let recently_deleted x=x.Dircopy_diff_t.recently_deleted;;
let recently_created x=x.Dircopy_diff_t.recently_created;;
let recently_changed x=x.Dircopy_diff_t.recently_changed;;

let constructor a b c={
   Dircopy_diff_t.recently_deleted =Recently_deleted.to_string_list a;
   Dircopy_diff_t.recently_changed =Recently_changed.to_string_list b;
   Dircopy_diff_t.recently_created =Recently_created.to_string_list c;
};;

let display x=
   let tempf=(fun msg l->
   "\n"::msg::(Image.image(fun w->"\t\t"^w) l)
   ) in
   let temp1=tempf "Deleted : " (x.Dircopy_diff_t.recently_deleted)
   and temp2=tempf "Created : " (x.Dircopy_diff_t.recently_created)
   and temp3=tempf "Changed : " (x.Dircopy_diff_t.recently_changed) in
   let temp4=String.concat "\n" (temp1@temp2@temp3) in
   (print_string temp4;
    flush stdout);;

module Private=struct

let summarize_short_path s=
   String.capitalize_ascii(Cull_string.after_rightmost (Cull_string.before_rightmost_possibly_all s '.') '/');;
 
let summarize_short_path_list l=
    let temp1=Image.image summarize_short_path l in
    Ordered.sort Total_ordering.silex_for_strings temp1;;


let salt = "Dircopy_"^"diff_t.";;

let recently_deleted_label = salt ^ "recently_deleted";;
let recently_changed_label = salt ^ "recently_changed";;
let recently_created_label = salt ^ "recently_created";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Dircopy_diff_t.recently_deleted = Concrete_object_field.to_string_list (g recently_deleted_label);
      recently_changed = Concrete_object_field.to_string_list (g recently_changed_label);
      recently_created = Concrete_object_field.to_string_list (g recently_created_label);
   };; 

let to_concrete_object dirdiff=
   let items= 
   [
    recently_deleted_label, Concrete_object_field.of_string_list dirdiff.Dircopy_diff_t.recently_deleted;
    recently_changed_label, Concrete_object_field.of_string_list dirdiff.Dircopy_diff_t.recently_changed;
    recently_created_label, Concrete_object_field.of_string_list dirdiff.Dircopy_diff_t.recently_created;
   ]  in
   Concrete_object_t.Record items;;

end;;

let change_with_no_creations_or_deletions l=
  {
      Dircopy_diff_t.recently_deleted = [];
      recently_changed = l;
      recently_created = [];
   };; 

let empty_one  = 
   {
      Dircopy_diff_t.recently_deleted = [];
      recently_changed = [];
      recently_created = [];
   };; 

let explain x=
   let tempf=(fun (msg,l)->
     if l=[]
     then None
     else Some(msg^" "^(String.concat "," l)^".")
   ) in
   let temp1=Option.filter_and_unpack tempf
   (* we use infinitives for github format *)
   [
     "Delete",Private.summarize_short_path_list(x.Dircopy_diff_t.recently_deleted);
     "Create",Private.summarize_short_path_list(x.Dircopy_diff_t.recently_created);
     "Modify",Private.summarize_short_path_list(x.Dircopy_diff_t.recently_changed);
   ] in
   if temp1=[] then "" else
   let temp2=(String.uncapitalize_ascii (List.hd temp1))::(List.tl temp1) in
   String.concat " " temp2;; 
   
let is_empty x=
  (x.Dircopy_diff_t.recently_deleted,x.Dircopy_diff_t.recently_created,x.Dircopy_diff_t.recently_changed)=
   ([],[],[]);;   
   
let of_concrete_object = Private.of_concrete_object ;;
let to_concrete_object = Private.to_concrete_object ;;   
   
   
   
              
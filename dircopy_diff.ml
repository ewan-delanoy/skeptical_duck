(*

#use"dircopy_diff.ml";;

*)

module Private=struct

let summarize_rootless_path rl=
   if List.mem (Dfn_rootless.to_ending rl) Dfa_ending.endings_for_compilable_files
   then String.capitalize_ascii(Cull_string.after_rightmost 
   (Cull_string.before_rightmost_possibly_all (Dfn_rootless.to_line rl) '.') '/')
   else Dfn_rootless.to_line rl;;
 
let summarize_rootless_path_list  l=
    let temp1=Image.image summarize_rootless_path l in
    Ordered.sort Total_ordering.silex_for_strings temp1;;

    

let salt = "Dircopy_"^"diff_t.";;

let recently_deleted_label = salt ^ "recently_deleted";;
let recently_changed_label = salt ^ "recently_changed";;
let recently_created_label = salt ^ "recently_created";;

let of_concrete_object ccrt_obj = 
   let g=Concrete_object_field.get_record ccrt_obj in
   {
      Dircopy_diff_t.recently_deleted = Dfn_rootless.list_of_concrete_object (g recently_deleted_label);
      recently_changed = Dfn_rootless.list_of_concrete_object (g recently_changed_label);
      recently_created = Dfn_rootless.list_of_concrete_object (g recently_created_label);
   };; 

let to_concrete_object dirdiff=
   let items= 
   [
    recently_deleted_label, Dfn_rootless.list_to_concrete_object dirdiff.Dircopy_diff_t.recently_deleted;
    recently_changed_label, Dfn_rootless.list_to_concrete_object dirdiff.Dircopy_diff_t.recently_changed;
    recently_created_label, Dfn_rootless.list_to_concrete_object dirdiff.Dircopy_diff_t.recently_created;
   ]  in
   Concrete_object_t.Record items;;

let is_empty x=
  (x.Dircopy_diff_t.recently_deleted,x.Dircopy_diff_t.recently_created,x.Dircopy_diff_t.recently_changed)=
   ([],[],[]);; 

let to_string x=
   if is_empty x then "{}" else 
   let tempf=(fun msg l->
   "\n"::msg::(Image.image(fun w->"\t\t"^(Dfn_rootless.to_line w)) l)
   ) in
   let temp1=tempf "Deleted : " (x.Dircopy_diff_t.recently_deleted)
   and temp2=tempf "Created : " (x.Dircopy_diff_t.recently_created)
   and temp3=tempf "Changed : " (x.Dircopy_diff_t.recently_changed) in
   String.concat "\n" (temp1@temp2@temp3) ;;

end;;



let add_changes diff l= 
  {
      diff with 
      Dircopy_diff_t.recently_changed = (diff.Dircopy_diff_t.recently_changed)@ l;
   };; 

let constructor a b c={
   Dircopy_diff_t.recently_deleted =a;
   Dircopy_diff_t.recently_changed =b;
   Dircopy_diff_t.recently_created =c;
};;


let create diff created_ones= 
  {
      diff with 
      Dircopy_diff_t.recently_created = (diff.Dircopy_diff_t.recently_created)@ created_ones;
   };; 

let destroy diff destroyed_ones= 
  {
      diff with 
      Dircopy_diff_t.recently_deleted = (diff.Dircopy_diff_t.recently_deleted)@ destroyed_ones;
   };; 


let empty_one  = 
   {
      Dircopy_diff_t.recently_deleted = [];
      recently_changed = [];
      recently_created = [];
   };; 


let explain  x=
   let tempf=(fun (msg,l)->
     if l=[]
     then None
     else Some(msg^" "^(String.concat "," l)^".")
   ) in
   let temp1=Option.filter_and_unpack tempf
   (* we use infinitives for github format *)
   [
     "Delete",Private.summarize_rootless_path_list  (x.Dircopy_diff_t.recently_deleted);
     "Create",Private.summarize_rootless_path_list  (x.Dircopy_diff_t.recently_created);
     "Modify",Private.summarize_rootless_path_list  (x.Dircopy_diff_t.recently_changed);
   ] in
   if temp1=[] then "" else
   let temp2=(String.uncapitalize_ascii (List.hd temp1))::(List.tl temp1) in
   String.concat " " temp2;; 
   


let is_empty = Private.is_empty ;;

let of_concrete_object = Private.of_concrete_object ;;

let print_out (fmt:Format.formatter) x=
   Format.fprintf fmt "@[%s@]" (Private.to_string x);;     


let recently_deleted x=x.Dircopy_diff_t.recently_deleted;;
let recently_created x=x.Dircopy_diff_t.recently_created;;
let recently_changed x=x.Dircopy_diff_t.recently_changed;;



let replace diff replacements= 
   let l_deleted = Image.image fst replacements 
   and l_created = Image.image snd replacements  in
  {
      diff with 
      Dircopy_diff_t.recently_created = (diff.Dircopy_diff_t.recently_created)@ l_created;
      Dircopy_diff_t.recently_deleted = (diff.Dircopy_diff_t.recently_deleted)@ l_deleted;
   };; 


let to_concrete_object = Private.to_concrete_object ;;   
   
   
   
              
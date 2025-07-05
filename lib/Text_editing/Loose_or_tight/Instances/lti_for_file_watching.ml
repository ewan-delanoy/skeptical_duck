(*

#use"lib/Text_editing/Loose_or_tight/Instances/lti_for_file_watching.ml";;

lti is shortcut for "Loose-tight instances"

*)

module Private = struct 
let the_list = 
  {
    Loose_or_tight.purpose = "file watching";
    files =Image.image (
   fun fn ->
    Absolute_path.of_string("lib/Filewatching/Fw_classes/fwg_"^fn^".ml")
) [
   "github_configuration";
   "with_batch_compilation";
   "with_githubbing"
] 
  };;

end ;;  


let set = Loose_or_tight.set_in_file_list Private.the_list ;;
let toggle () = Loose_or_tight.toggle_in_file_list Private.the_list ;;
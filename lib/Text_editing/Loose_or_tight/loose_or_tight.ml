(*

#use"lib/Text_editing/Loose_or_tight/loose_or_tight.ml";;

In certain parts of Ocaml Code, some "tight" snippet may
be replaced by a "loose" snippet. One possiblity is to store
both snippets near each other in the same file, commenting 
out the unused snippet.

Nodes in this module are objects that allow to store the
configuration and manage the loose/tight pair.

*)

exception Content_state_exn of string ;;

type t = Loose |Tight ;;

type list_t = {
   purpose : string ;
   files : Absolute_path.t list;
} ;;

module Private =struct

type configuration_t = {
  markers_for_loose_version : string * string ;
  markers_for_tight_version : string * string ;
  the_marked_file : Absolute_path.t ;
} ;; 



type state = Commented |Uncommented ;;

let markers_from_purpose content = 
  (
    ("(* Beginning of loose version of "^content^" *)",
     "(* End"^   " of loose version of "^content^" *)"),
    ("(* Beginning of tight version of "^content^" *)",
     "(* End"^   " of tight version of "^content^" *)")
  ) ;; 

let compute_configuration  ?(purpose="the unknown thing") marked_file= 
  let (mrkrs_for_loose,mrkrs_for_tight) = markers_from_purpose purpose in 
  {
    markers_for_loose_version = mrkrs_for_loose ;
    markers_for_tight_version = mrkrs_for_tight ;
    the_marked_file = marked_file ;
  } ;; 

let get_marker config = function  
   Loose -> config.markers_for_loose_version
  |Tight -> config.markers_for_tight_version ;;

let get_content_for_chosen_marker config loose_or_tight=
  let markers = get_marker config loose_or_tight in 
  Cull_string.between_markers markers 
  (Io.read_whole_file config.the_marked_file) ;; 

let content_state content = 
  let (starter,ender) = Replace_inside.pair_for_commenting_or_uncommenting in
  let indicator1 = (String.starts_with content ~prefix:starter)
  and indicator2= (String.ends_with content ~suffix:ender) in 
  if indicator1 <> indicator2 
  then raise (Content_state_exn "")
  else if indicator1
       then Commented 
       else Uncommented;;    

let get_state_for_chosen_marker config loose_or_tight=
 content_state(get_content_for_chosen_marker config loose_or_tight);;

let contrary_state = function 
  Uncommented -> Commented 
 |Commented-> Uncommented ;;

 let unsafe_set_content_state content new_state = 
  (*
  note that this is weaker than an ordinary setter,
  only works correctly when the state is changed. 
  The "safe, working in all cases" version is just below
  *)
  let pair = Replace_inside.pair_for_commenting_or_uncommenting in
  match new_state with 
   Uncommented -> Cull_string.two_sided_cutting pair content 
  |Commented->
    let (starter,ender) = pair in
    starter^content^ender ;;    

let set_content_state old_content new_state = 
  let old_state = content_state old_content in
  if old_state = new_state 
  then old_content 
  else unsafe_set_content_state old_content new_state ;;


let set_state_for_chosen_marker config loose_or_tight new_state= 
  let old_content = get_content_for_chosen_marker config loose_or_tight in 
  let new_content = unsafe_set_content_state old_content new_state 
  and markers = get_marker config loose_or_tight in
  Replace_inside.overwrite_between_markers_inside_file 
   ~overwriter:new_content markers config.the_marked_file ;; 

let set_state_for_both_markers config new_state = 
  (
    set_state_for_chosen_marker config Loose new_state;
    set_state_for_chosen_marker config Tight (contrary_state new_state)
  )

let set_loosetight_biparagraph config new_state = 
    set_state_for_both_markers config new_state ;;

let toggle_loosetight_biparagraph config = 
  let old_state = get_state_for_chosen_marker config Loose in 
  let new_state = contrary_state old_state in 
  set_state_for_both_markers config new_state ;;

let toggle marked_file ~purpose_name=      
  let config = compute_configuration   ~purpose:purpose_name marked_file in 
  toggle_loosetight_biparagraph config ;;

let set marked_file ~purpose_name wanted_state=      
   let config = compute_configuration   ~purpose:purpose_name marked_file in 
   set_loosetight_biparagraph config wanted_state ;;

let write_loose_tight_template_in_file_at_line ap ~purpose_name ~line_number = 
   let ((beg1,end1),(beg2,end2)) = markers_from_purpose purpose_name in 
   let text =
    String.concat "\n"
    [beg1;"";"(* ... Put loose version here ... *)";"";end1;"";
     beg2;"";"(* ... Put tight version here ... *)";"";end2] in 
   Lines_in_text.insert_after_line_inside_file ap ~line_number
      ~inserted_snippet:text ;;  

let global_state_for_file_list fl =
   (* We just look at the first file in the list. 
    This is arbitrary but convenient *)
   let file = List.hd fl.files in 
   let config = compute_configuration ~purpose:fl.purpose file in 
   get_state_for_chosen_marker config Loose ;;

let set_in_file_list fl wanted_state = 
  List.iter (
    fun file -> set ~purpose_name:fl.purpose file wanted_state
  )  fl.files ;;
let toggle_in_file_list fl = 
  let old_state = global_state_for_file_list fl in 
  let new_state = contrary_state old_state in 
  set_in_file_list fl new_state ;;


end ;;  


let set = Private.set ;;
let set_in_file_list = Private.set_in_file_list ;;
let toggle = Private.toggle ;; 
let toggle_in_file_list = Private.toggle_in_file_list ;;
let write_loose_tight_template_in_file_at_line = Private.write_loose_tight_template_in_file_at_line ;;
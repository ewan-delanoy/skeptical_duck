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
exception Toggle_exn of Absolute_path.t ;;
exception Node_global_state_exn of Absolute_path.t ;;

type t = Loose |Tight ;;

module Private =struct

type node_t = {
  markers_for_loose_version : string * string ;
  markers_for_tight_version : string * string ;
  the_marked_file : Absolute_path.t ;
} ;; 



type state = Commented |Uncommented ;;

let markers_from_content content = 
  (
    ("(* Beginning of loose version of "^content^" *)",
     "(* End"^   " of loose version of "^content^" *)"),
    ("(* Beginning of tight version of "^content^" *)",
     "(* End"^   " of tight version of "^content^" *)")
  ) ;; 

let compute_node  
  ?markers_for_loose_followed_by_tight ?(content="the unknown thing") 
  marked_file= 
  let (mrkrs_for_loose,mrkrs_for_tight) = (
  match markers_for_loose_followed_by_tight with 
  None -> markers_from_content content    
  |Some(loose,tight) -> (loose,tight)
  ) in 
  {
    markers_for_loose_version = mrkrs_for_loose ;
    markers_for_tight_version = mrkrs_for_tight ;
    the_marked_file = marked_file ;
  } ;; 

let get_marker node = function  
   Loose ->node.markers_for_loose_version
  |Tight ->node.markers_for_tight_version ;;

let get_content_for_chosen_marker node loose_or_tight=
  let markers = get_marker node loose_or_tight in 
  Cull_string.between_markers markers 
  (Io.read_whole_file node.the_marked_file) ;; 

let content_state content = 
  let (starter,ender) = Replace_inside.pair_for_commenting_or_uncommenting in
  let indicator1 = (String.starts_with content ~prefix:starter)
  and indicator2= (String.ends_with content ~suffix:ender) in 
  if indicator1 <> indicator2 
  then raise (Content_state_exn "")
  else if indicator1
       then Commented 
       else Uncommented;;    

let get_state_for_chosen_marker node loose_or_tight=
 content_state(get_content_for_chosen_marker node loose_or_tight);;

 let unsafe_set_content_state content new_state = 
  (*
  note that this is weaker than an ordinary setter,
  only works correctly when the state is changed. Hence the "unsafe" prefix
  *)
  let pair = Replace_inside.pair_for_commenting_or_uncommenting in
  match new_state with 
   Uncommented -> Cull_string.two_sided_cutting pair content 
  |Commented->
    let (starter,ender) = pair in
    starter^content^ender ;;    

let contrary_state = function 
   Uncommented -> Commented 
  |Commented-> Uncommented ;;

let change_state_for_chosen_marker node loose_or_tight = 
  let old_content = get_content_for_chosen_marker node loose_or_tight in 
  let old_state = content_state old_content in 
  let new_state = contrary_state old_state in
  let new_content = unsafe_set_content_state old_content new_state 
  and markers = get_marker node loose_or_tight in
  Replace_inside.overwrite_between_markers_inside_file 
   ~overwriter:new_content markers node.the_marked_file ;; 

let toggle_node node = 
  let state1 = get_state_for_chosen_marker node Loose 
  and state2 = get_state_for_chosen_marker node Tight in 
  if state1 = state2 
  then raise(Toggle_exn node.the_marked_file) 
  else
    
    (change_state_for_chosen_marker node Loose ;
     change_state_for_chosen_marker node Tight );;

let toggle marked_file ~purpose_name=      
  let node = compute_node   ~content:purpose_name marked_file in 
  toggle_node node ;;

let global_node_state node = 
    let state1 = get_state_for_chosen_marker node Loose 
    and state2 = get_state_for_chosen_marker node Tight in 
    if state1 = state2 
    then raise(Node_global_state_exn node.the_marked_file) 
    else
    if state1 = Uncommented 
    then Loose    
    else Tight ;;

let set_node_state node wanted_state = 
   let old_state = global_node_state node in 
   if wanted_state = old_state 
   then ()
   else toggle_node node ;;

let set marked_file ~purpose_name wanted_state=      
   let node = compute_node   ~content:purpose_name marked_file in 
   set_node_state node wanted_state ;;


end ;;  

let set = Private.set ;;
let toggle = Private.toggle ;; 
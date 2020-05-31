
exception Open_in_exn of string ;;
exception Open_out_exn of string ;;
exception Dangerous_command_reading of string ;;

let max_size_for_reasonable_in_channel = ref(1000000);;

module Private = struct 

let make_filename_complete s=
  let home=Sys.getenv("HOME") in
  if s="" then Absolute_path.of_string(home) else
  let c=String.get s 0 in
  if c='/' then Absolute_path.of_string(s) else
  if c='~' then Absolute_path.of_string(home^(String.sub s 1 (String.length(s)-1))) else
  Absolute_path.of_string((Sys.getcwd ())^"/"^s);;

let open_in_locally x=try open_in(x) with 
_->raise(Open_in_exn(x));;

let open_out_locally x=try open_out(x) with 
_->raise(Open_out_exn(x));;  

let put_whole_content_of_file_in_buffer s=
  let x=Absolute_path.to_string(make_filename_complete(s)) in
  let janet=open_in_locally(x) in
  let n=in_channel_length(janet) in
  let b=Buffer.create(n) in
  let _=Buffer.add_channel(b)(janet)(n) in
  let _=close_in janet in
  b;;
  
type filename=string;;
  
let erase_file_and_fill_it_with_contents_of_buffer (fn:filename) b=
   let x=Absolute_path.to_string(make_filename_complete(fn)) in
  let john=open_out_locally(x) in
  (Buffer.output_buffer(john)(b);close_out john);;
  
let overwrite_with ap s=
   let fn=Absolute_path.to_string ap in
   let n=String.length(s) in
   let b=Buffer.create(n) in
   let _=Buffer.add_string b s in
   erase_file_and_fill_it_with_contents_of_buffer fn b;;
   
let read_whole_file ap=   
   let s=Absolute_path.to_string ap in
   let b=put_whole_content_of_file_in_buffer(s) in
   Buffer.contents b;;

let append_string_to_file s ap=
  let new_content=(read_whole_file ap)^s in
  overwrite_with ap new_content;; 
   
let read_reasonable_command cmd =
   let chan = Unix.open_process_in cmd in 
   let max_reasonable_size = (!max_size_for_reasonable_in_channel) in 
   let buf = Bytes.create max_reasonable_size  in 
   let final_size = input chan buf 0 max_reasonable_size  in 
   let _ = Unix.close_process_in chan in 
   if final_size >= max_reasonable_size 
   then raise(Dangerous_command_reading(cmd))
   else 
   Bytes.sub_string buf 0 final_size ;;

end ;; 

let overwrite_with = Private.overwrite_with ;;
let read_reasonable_command = Private.read_reasonable_command ;;
let read_whole_file = Private.read_whole_file ;;
   
  
             
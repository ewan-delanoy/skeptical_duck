(*

Directories name, with the trailing slash removed.

#use"directory_name.ml";;

*)



exception Non_directory of string;;
exception File_not_found of string * (Directory_name_t.t list);;

let find_file_with_directory_list fname l=
  match Option.find_and_stop (
     fun (Directory_name_t.D s_dir) ->
      let full_path = s_dir^"/"^fname in 
      if Sys.file_exists full_path 
      then Some(Absolute_path.of_string full_path)
      else None
  ) l with 
  None -> raise(File_not_found(fname,l))
  |Some(ap) -> ap;;


let of_string s=
  let temp1=Tools_for_absolute_path.of_string s in
  if Sys.is_directory temp1
  then Directory_name_t.D(Tools_for_absolute_path.remove_trailing_slash temp1)
  else raise(Non_directory(s));;

let connectable_to_subpath (Directory_name_t.D s)=s^"/";;


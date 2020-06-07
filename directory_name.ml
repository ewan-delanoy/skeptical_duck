(*

Directories name, with the trailing slash removed.

#use"directory_name.ml";;

*)



exception Non_directory of string;;

let of_string s=
  let temp1=Tools_for_absolute_path.of_string s in
  if Sys.is_directory temp1
  then Directory_name_t.D(Tools_for_absolute_path.remove_trailing_slash temp1)
  else raise(Non_directory(s));;

let connectable_to_subpath (Directory_name_t.D s)=s^"/";;

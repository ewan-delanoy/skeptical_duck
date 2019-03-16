(*

#use"industrial_separator.ml";;

Separators used in encoding of data types by strings.
The main key should not appear in any string inside the data types,
it is to be changed if necessary.

*)

module Private = struct
  let key="mpdykru"^"vueaoqhkt";;
  let counter=ref(0);;
  let new_key ()=
     let v=(!counter)+1 in 
     let _=(counter:=v) in 
     key^(Strung.left_completed_string_of_int 3 v);;
end;;     

let alaskan_data=Private.new_key();;
let alaskan_save_all1=Private.new_key();;
let alaskan_save_all2=Private.new_key();;
let double_tunnel_inner=Private.new_key();;
let double_tunnel_outer=Private.new_key();;
let half_dressed_module=Private.new_key();;
let half_dressed_module_times_boolean=Private.new_key();;
let ocaml_target1=Private.new_key();;
let ocaml_target2=Private.new_key();;
let mlx_ended_absolute_path=Private.new_key();;
let modulesystem_data1=Private.new_key();;
let modulesystem_data2=Private.new_key();;
let slow_copy_task1=Private.new_key();;
let slow_copy_task2=Private.new_key();;
let string_times_string=Private.new_key();;
let small_array=Private.new_key();;
let text_lengthener_decompression=Private.new_key();;
let text_lengthener_field=Private.new_key();;
let text_lengthener_level_one_list=Private.new_key();;
let text_lengthener_level_two_list=Private.new_key();;


             
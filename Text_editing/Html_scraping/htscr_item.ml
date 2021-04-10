(*

#use"Text_editing/Html_scraping/htscr_item.ml";;

*)

exception Too_many_question_marks_exn of string ;;
exception No_http_in_url of string ;;
exception Compute_ending_exn of string ;;
exception Forgotten_php_file of string ;;

module Private = struct 

let salt = "Htscr_"^"_item_t.";;

let category_label         = salt ^ "category";;
let original_request_label = salt ^ "original_request";;
let polished_request_label = salt ^ "polished_request";;
let location_label         = salt ^ "location";;
let filename_label         = salt ^ "filename";;


let of_concrete_object  crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
      Htscr_item_t.category = Htscr_item_category.of_concrete_object (g category_label);
      original_request = Concrete_object_field.unwrap_string  (g original_request_label);
      polished_request = Concrete_object_field.unwrap_string  (g polished_request_label);
      location = Dfa_subdirectory.of_concrete_object (g location_label);
      filename = Concrete_object_field.unwrap_string (g filename_label);
   };;

let to_concrete_object item =
 
   Concrete_object_t.Record([
      category_label, Htscr_item_category.to_concrete_object (item.Htscr_item_t.category);
      original_request_label, Concrete_object_field.wrap_string  (item.Htscr_item_t.original_request);
      polished_request_label, Concrete_object_field.wrap_string  (item.Htscr_item_t.polished_request);
      location_label, Dfa_subdirectory.to_concrete_object (item.Htscr_item_t.location);
      filename_label, Concrete_object_field.wrap_string (item.Htscr_item_t.filename);
   ]);;

let list_of_concrete_object crobj = Concrete_object_field.to_list of_concrete_object crobj;;
let list_to_concrete_object l = Concrete_object_field.of_list to_concrete_object l;;
   
    
let remove_question_mark fn = 
       let temp1 = Substring.occurrences_of_in "?" fn in 
       let d = List.length temp1 in 
       if d>1 then raise(Too_many_question_marks_exn fn) else 
       if d=0 then fn else 
       let idx = List.hd temp1 in 
       Cull_string.beginning (idx-1) fn ;;  
   
let compute_triple config request=
       let list_of_allowed_endings = config.Htscr_config_t.list_of_allowed_endings in 
       let k = Substring.rightmost_index_of_in "/" request in 
       if k<0 then None else
       let subdir =Cull_string.beginning k request 
       and qualified_name = Cull_string.cobeginning k request in 
       let name =remove_question_mark qualified_name in
       if  List.exists(Supstring.ends_with name) list_of_allowed_endings 
       then Some(subdir,name,qualified_name)
       else None ;;
   
let decide_category config  (a,b,c) =  
        let list_of_proxies = config.Htscr_config_t.list_of_proxies 
        and endings_for_dynamic_homemades = config.Htscr_config_t.endings_for_dynamic_homemades in 
        if List.mem b list_of_proxies 
        then Htscr_item_category_t.Proxy 
        else
        if List.exists (fun (x,_)->x=b) endings_for_dynamic_homemades     
        then Htscr_item_category_t.Dynamic_homemade 
        else Htscr_item_category_t.Static_homemade ;;    
  
let decode_url =     
          Replace_inside.replace_several_inside_string 
          ["%3A",":";"%2C",",";"&amp;","&";"%2F","/";"%3F","?";"%3D","="] ;;
        
let polish_url line =
              let i1 = Substring.leftmost_index_of_in "http" line in 
              if i1 < 0 then raise(No_http_in_url(line)) else
              let pre_i2 = Substring.leftmost_index_of_in_from "&amp;" line i1 in 
              let i2 = (if pre_i2<1 then (String.length line)+1 else pre_i2) in 
              let pre_i3 = Substring.leftmost_index_of_in_from "%3F" line i1 in 
              let i3 = (if pre_i3<1 then (String.length line)+1 else pre_i3) in 
              let i4 = min i2 i3 in 
              let part = Cull_string.interval line i1 (i4-1) in 
              decode_url  part ;;

let compute_ending config  fn =
    let list_of_allowed_endings = config.Htscr_config_t.list_of_allowed_endings 
    and endings_for_dynamic_homemades = config.Htscr_config_t.endings_for_dynamic_homemades in 
    match List.assoc_opt fn  endings_for_dynamic_homemades with 
      Some(ending)-> ending 
      | None -> (
        match Option.seek (fun edg->Supstring.ends_with fn edg) list_of_allowed_endings with 
        Some(ending2) -> if ending2 =".php" then raise(Forgotten_php_file(fn)) else ending2 
        | None -> raise(Compute_ending_exn(fn))
      ) ;;

let compute_proxy_candidate config old_proxy_count request (a,b,c) = 
    let static_subdir_name = config.Htscr_config_t.static_subdir_name in 
    let polished_url = polish_url c in 
    let j1 = Substring.rightmost_index_of_in "/" polished_url in 
    {
      Htscr_item_t.category = Htscr_item_category_t.Proxy ;
      original_request = request ;
      polished_request = polished_url;
      location = static_subdir_name ;
      filename = "asset"^(string_of_int (old_proxy_count+1))^"_"^
                          (Cull_string.cobeginning j1 polished_url) ;
    } ;;    

let compute_static_homemade_candidate config request (a,b,c) = 
    let website = config.Htscr_config_t.website in 
    let local_path =  (Cull_string.cobeginning 1 a)^b in 
    let j1 = Substring.rightmost_index_of_in "/" local_path in 
    {
      Htscr_item_t.category = Htscr_item_category_t.Static_homemade ;
      original_request = request ;
      polished_request = website^a^c ;
      location = Dfa_subdirectory.of_line (Cull_string.beginning (j1-1) local_path) ;
      filename = Cull_string.cobeginning j1 local_path ;
    } ;;              

let compute_dynamic_homemade_candidate config  old_dynamic_count request (a,b,c) = 
    let website = config.Htscr_config_t.website 
    and static_subdir_name = config.Htscr_config_t.static_subdir_name  in 
    let ending = compute_ending config b in 
    {
      Htscr_item_t.category = Htscr_item_category_t.Dynamic_homemade ;
      original_request = request ;
      polished_request = website^"/"^(decode_url c) ;
      location = static_subdir_name ;
      filename = "dynamic"^(string_of_int (old_dynamic_count+1))^ending ;
    } ;;  


let compute_candidate config ~old_proxy_count ~old_dynamic_count ~request =
   let opt = compute_triple config request in 
   if opt = None then None else 
   let (a,b,c) = Option.unpack opt in  
   match (decide_category config (a,b,c)) with 
    Htscr_item_category_t.Proxy -> 
          Some(compute_proxy_candidate config old_proxy_count request (a,b,c))
   |Static_homemade -> 
          Some(compute_static_homemade_candidate config request (a,b,c))
   |Dynamic_homemade -> 
          Some(compute_dynamic_homemade_candidate config old_dynamic_count request (a,b,c));;


let command_and_replacement config item = 
   let building_site = config.Htscr_config_t.building_site in 
   let new_path = (Dfa_subdirectory.connectable_to_subpath item.Htscr_item_t.location)^(item.Htscr_item_t.filename) in 
  ("curl -L \""^(item.Htscr_item_t.polished_request)^"\" > "
   ^building_site^"/"^new_path,
   (item.Htscr_item_t.polished_request,new_path)) ;;

let directory_for_one_item config item =
    if item.Htscr_item_t.category <> Htscr_item_category_t.Static_homemade 
    then None 
    else let dir = (config.Htscr_config_t.building_site)^"/"^
    (Dfa_subdirectory.without_trailing_slash item.Htscr_item_t.location) in 
    Some dir ;;

let directories_for_several_items config items =
    let unsorted = Option.filter_and_unpack (directory_for_one_item config) items in 
    Ordered.sort Total_ordering.lex_for_strings  unsorted ;;
 
let commands_and_replacements config items =
    let static_dir = Htscr_config.full_static_subdir_path config
    and other_dirs = directories_for_several_items config items 
    and temp1 = Image.image (command_and_replacement config) items in 
    let mkdir_commands = Image.image (fun dir->"mkdir -p "^dir) 
       (static_dir :: other_dirs) 
    and other_commands = Image.image fst temp1 in 
    (mkdir_commands@other_commands,Image.image snd temp1) ;;
    

end ;;


let commands_and_replacements = Private.commands_and_replacements ;;
let compute_candidate = Private.compute_candidate ;;
let list_of_concrete_object = Private.list_of_concrete_object ;;
let list_to_concrete_object = Private.list_to_concrete_object ;;



  
  
  
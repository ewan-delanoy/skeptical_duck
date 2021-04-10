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

let compute_proxy_item config ~asset_idx request (a,b,c) = 
    let static_subdir_name = config.Htscr_config_t.static_subdir_name in 
    let polished_url = polish_url c in 
    let j1 = Substring.rightmost_index_of_in "/" polished_url in 
    {
      Htscr_item_t.category = Htscr_item_category_t.Proxy ;
      original_request = request ;
      polished_request = polished_url;
      location = static_subdir_name ;
      filename = "asset"^(string_of_int asset_idx)^"_"^
                          (Cull_string.cobeginning j1 polished_url) ;
    } ;;    

let compute_static_homemade_item config request (a,b,c) = 
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

let compute_dynamic_homemade_item config  ~asset_idx request (a,b,c) = 
    let website = config.Htscr_config_t.website 
    and static_subdir_name = config.Htscr_config_t.static_subdir_name  in 
    let ending = compute_ending config b in 
    {
      Htscr_item_t.category = Htscr_item_category_t.Dynamic_homemade ;
      original_request = request ;
      polished_request = website^"/"^(decode_url c) ;
      location = static_subdir_name ;
      filename = "dynamic"^(string_of_int asset_idx)^ending ;
    } ;;  


let compute_item config ~asset_idx request =
   let opt = compute_triple config request in 
   if opt = None then None else 
   let (a,b,c) = Option.unpack opt in  
   match (decide_category config (a,b,c)) with 
    Htscr_item_category_t.Proxy -> Some(compute_proxy_item config ~asset_idx request (a,b,c))
   |Static_homemade -> Some(compute_static_homemade_item config request (a,b,c))
   |Dynamic_homemade -> Some(compute_dynamic_homemade_item config ~asset_idx request (a,b,c));;
   
let reset_index_in_proxy ~old_proxy_count (new_idx,item) = 
    let polished_request = item.Htscr_item_t.polished_request in 
    let j1 = Substring.rightmost_index_of_in "/" polished_request in 
    {
      item with   
      Htscr_item_t.filename = "asset"^(string_of_int (old_proxy_count+new_idx))^"_"^
                          (Cull_string.cobeginning j1 polished_request) ;
    } ;; 

let reset_index_in_dynamic_homemade ~old_dynamic_count (new_idx,item) = 
    let old_filename = item.Htscr_item_t.filename in 
    let j1 = Substring.rightmost_index_of_in "." old_filename in 
    let ending = Cull_string.cobeginning (j1-1) old_filename  in 
    {
      item with   
      Htscr_item_t.filename = "dynamic"^(string_of_int (old_dynamic_count+new_idx))^ending ;
    } ;;  



let command_and_replacement config item = 
   let building_site = config.Htscr_config_t.building_site in 
   let new_path = (Dfa_subdirectory.connectable_to_subpath item.Htscr_item_t.location)^(item.Htscr_item_t.filename) in 
  ("curl -L \""^(item.Htscr_item_t.polished_request)^"\" > "
   ^building_site^"/"^new_path,
   (item.Htscr_item_t.polished_request,new_path)) ;;

let extract_items_from_text config ~old_proxy_count ~old_dynamic_count text = 
  let requests = Htscr_extract_requests_from_text.main config  ~text  in 
  let temp1 = Image.image (fun request->(request,compute_item config ~asset_idx:1 request)) requests in 
  let (temp2,temp3) =List.partition (fun (_,opt)->opt<>None)  temp1 in 
  let temp4 = Image.image (fun (_,opt)->Option.unpack opt) temp2 in 
  let rejects = Image.image fst temp3 in 
  let (proxies_with_bad_indices,nonproxies) = List.partition (fun item -> 
       item.Htscr_item_t.category = Htscr_item_category_t.Proxy ) temp4 in  
  let (dynamics_with_bad_indices,statics) = List.partition (fun item -> 
        item.Htscr_item_t.category = Htscr_item_category_t.Dynamic_homemade ) nonproxies in
  let indexed_proxies = Ennig.index_everything proxies_with_bad_indices 
  and indexed_dynamics = Ennig.index_everything dynamics_with_bad_indices in 
  let proxies = Image.image (reset_index_in_proxy ~old_proxy_count) indexed_proxies 
  and dynamics = Image.image (reset_index_in_dynamic_homemade ~old_dynamic_count) indexed_dynamics in 
  let new_proxy_count = old_proxy_count+(List.length proxies)
  and new_dynamic_count = old_dynamic_count+(List.length dynamics) in 
  (proxies,statics,dynamics,new_proxy_count,new_dynamic_count,rejects) ;;
  
end ;;


let command_and_replacement = Private.command_and_replacement ;;
let extract_items_from_text = Private.extract_items_from_text ;;
let list_of_concrete_object = Private.list_of_concrete_object ;;
let list_to_concrete_object = Private.list_to_concrete_object ;;



  
  
  
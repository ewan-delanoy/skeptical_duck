(*

#use"Text_editing/Html_scraping/htscr_config.ml";;

*)



let salt= "Htscr_" ^ "config_t.";;

let source_tags_label                   = salt ^ "source_tags";;
let list_of_allowed_endings_label       = salt ^ "list_of_allowed_endings";;
let static_subdir_name_label            = salt ^ "static_subdir_name";;
let list_of_proxies_label               = salt ^ "list_of_proxies";;
let endings_for_dynamic_homemades_label = salt ^ "endings_for_dynamic_homemades";;
let endings_for_special_files_label     = salt ^ "endings_for_special_files";;
let website_label                       = salt ^ "website";;


let of_concrete_object  crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
           Htscr_config_t.source_tags = Concrete_object_field.to_string_list(g source_tags_label);
              list_of_allowed_endings = Concrete_object_field.to_string_list(g list_of_allowed_endings_label);
                   static_subdir_name = Dfa_subdirectory.of_concrete_object(g static_subdir_name_label);
                      list_of_proxies = Concrete_object_field.to_string_list(g list_of_proxies_label);
        endings_for_dynamic_homemades = Concrete_object_field.to_string_pair_list(g endings_for_dynamic_homemades_label);
            endings_for_special_files = Concrete_object_field.to_string_pair_list(g endings_for_special_files_label);
                              website = Concrete_object_field.unwrap_string(g website_label);
   };;


let to_concrete_object  item= 
   Concrete_object_t.Record([ 
                      source_tags_label , Concrete_object_field.of_string_list(item.Htscr_config_t.source_tags);
          list_of_allowed_endings_label , Concrete_object_field.of_string_list(item.Htscr_config_t.list_of_allowed_endings);
               static_subdir_name_label , Dfa_subdirectory.to_concrete_object(item.Htscr_config_t.static_subdir_name);
                  list_of_proxies_label , Concrete_object_field.of_string_list(item.Htscr_config_t.list_of_proxies);
    endings_for_dynamic_homemades_label , Concrete_object_field.of_string_pair_list(item.Htscr_config_t.endings_for_dynamic_homemades);
        endings_for_special_files_label , Concrete_object_field.of_string_pair_list(item.Htscr_config_t.endings_for_special_files);
                          website_label , Concrete_object_field.wrap_string(item.Htscr_config_t.website);
   ]);;

(*

#use"Text_editing/Html_scraping/htscr_config.ml";;

*)

module Private = struct

  let salt= "Htscr_" ^ "config_t.";;
  
  let building_site_label                 = salt ^ "building_site";;
  let endings_for_dynamic_homemades_label = salt ^ "endings_for_dynamic_homemades";;
  let list_of_allowed_endings_label       = salt ^ "list_of_allowed_endings";;
  let list_of_proxies_label               = salt ^ "list_of_proxies";;
  let source_tags_label                   = salt ^ "source_tags";;
  let static_subdir_name_label            = salt ^ "static_subdir_name";;
  let website_label                       = salt ^ "website";;
  
  let string_pair_of_concrete_object = 
      Crobj_converter_combinator.to_pair
      Crobj_converter.string_of_concrete_object Crobj_converter.string_of_concrete_object ;;
  
  let string_pair_to_concrete_object = 
      Crobj_converter_combinator.of_pair
      Crobj_converter.string_to_concrete_object Crobj_converter.string_to_concrete_object ;;
  
  let string_list_of_concrete_object = 
      Crobj_converter_combinator.to_list
      Crobj_converter.string_of_concrete_object ;;
  
  let string_list_to_concrete_object = 
      Crobj_converter_combinator.of_list
      Crobj_converter.string_to_concrete_object ;;

  let string_pair_list_of_concrete_object = 
    Crobj_converter_combinator.to_list
    string_pair_of_concrete_object ;;
  
  let string_pair_list_to_concrete_object = 
    Crobj_converter_combinator.of_list
    string_pair_to_concrete_object ;;  

  let of_concrete_object  crobj= 
     let g = Concrete_object.get_record crobj in 
     {
           Htscr_config_t.building_site = Crobj_converter.string_of_concrete_object(g building_site_label);
          endings_for_dynamic_homemades = string_pair_list_of_concrete_object(g endings_for_dynamic_homemades_label);
                list_of_allowed_endings = string_list_of_concrete_object(g list_of_allowed_endings_label);
                        list_of_proxies = string_list_of_concrete_object(g list_of_proxies_label);
                            source_tags = string_list_of_concrete_object(g source_tags_label);
                     static_subdir_name = Dfa_subdirectory.of_concrete_object(g static_subdir_name_label);
                                website = Crobj_converter.string_of_concrete_object(g website_label);
     };;
  
  
     let to_concrete_object  config= 
     Concrete_object_t.Record([ 
                      building_site_label , Crobj_converter.string_to_concrete_object(config.Htscr_config_t.building_site);
      endings_for_dynamic_homemades_label , string_pair_list_to_concrete_object(config.Htscr_config_t.endings_for_dynamic_homemades);
            list_of_allowed_endings_label , string_list_to_concrete_object(config.Htscr_config_t.list_of_allowed_endings);
                    list_of_proxies_label , string_list_to_concrete_object(config.Htscr_config_t.list_of_proxies);
                        source_tags_label , string_list_to_concrete_object(config.Htscr_config_t.source_tags);
                 static_subdir_name_label , Dfa_subdirectory.to_concrete_object(config.Htscr_config_t.static_subdir_name);
                            website_label , Crobj_converter.string_to_concrete_object(config.Htscr_config_t.website);
     ]);;
  
  let full_static_subdir_path config =
      (config.Htscr_config_t.building_site)^"/"^
        (Dfa_subdirectory.without_trailing_slash config.Htscr_config_t.static_subdir_name);;
  
  
end ;;   

let full_static_subdir_path = Private.full_static_subdir_path ;;
let of_concrete_object = Private.of_concrete_object ;;
let to_concrete_object = Private.to_concrete_object ;;
  
  
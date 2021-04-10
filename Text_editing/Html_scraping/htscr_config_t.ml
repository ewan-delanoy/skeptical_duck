(*

#use"Text_editing/Html_scraping/htscr_config_t.ml";;

*)

type t= 
  {
    building_site:string;
    endings_for_dynamic_homemades:(string * string) list;
    endings_for_special_files:(string * string) list;
    list_of_allowed_endings:string list ;
    list_of_proxies:string list;
    source_tags:string list ;
    static_subdir_name:Dfa_subdirectory_t.t ;
    website:string;
  } ;;


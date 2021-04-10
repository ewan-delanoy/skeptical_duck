(*

#use"Text_editing/Html_scraping/htscr_state_t.ml";;

*)

type t= 
  {
    stored_proxies : Htscr_item_t.t list;
    stored_static_homemades : Htscr_item_t.t list;
    stored_dynamic_homemades : Htscr_item_t.t list;
    proxy_count : int;
    dynamic_count : int;
  } ;;


(*

#use"Text_editing/Html_scraping/html_scraping_item_t.ml";;

*)

type t= 
  {
    category : Htscr_item_category_t.t ;
    original_request : string ;
    location : Dfa_subdirectory_t.t ;
    filename : string ;
  } ;;

(*

#use"Text_editing/Html_scraping/htscr_item_t.ml";;

*)

type t= 
  {
    category : Htscr_item_category_t.t ;
    original_request : string ;
    polished_request : string ;
    location : Dfa_subdirectory_t.t ;
    filename : string ;
  } ;;

(*

#use"Text_editing/Html_scraping/html_scraping_item_category.ml";;

*)

module Private = struct 

  let salt = "Html_"^"scraping_item_category_t.";;

  let crobj_correspondences = 
      [
         Htscr_item_category_t.Proxy            , salt ^ "Proxy" ;
         Htscr_item_category_t.Static_homemade  , salt ^ "Static_homemade"  ;
         Htscr_item_category_t.Dynamic_homemade , salt ^ "Dynamic_homemade" ;
      ];;  

end ;;   


let of_concrete_object = Concrete_object_field.unwrap_lonely_variant Private.crobj_correspondences;;

let to_concrete_object = Concrete_object_field.wrap_lonely_variant Private.crobj_correspondences;; 
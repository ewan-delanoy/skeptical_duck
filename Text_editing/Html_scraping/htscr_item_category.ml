(*

#use"Text_editing/Html_scraping/htscr_item_category.ml";;

*)

module Private = struct 

  let salt = "Htscr_"^"item_category_t.";;

  let crobj_correspondences = 
      [
         Htscr_item_category_t.Proxy            , salt ^ "Proxy" ;
         Htscr_item_category_t.Static_homemade  , salt ^ "Static_homemade"  ;
         Htscr_item_category_t.Dynamic_homemade , salt ^ "Dynamic_homemade" ;
      ];;  

end ;;   


let of_concrete_object = Concrete_object.unwrap_lonely_variant Private.crobj_correspondences;;

let to_concrete_object = Concrete_object.wrap_lonely_variant Private.crobj_correspondences;; 
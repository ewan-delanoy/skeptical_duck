(*

#use"Text_editing/Html_scraping/htscr_state.ml";;

*)

module Private = struct 

let salt= "Htscr_" ^ "state_t.";;

let stored_proxies_label           = salt ^ "stored_proxies";;
let stored_static_homemades_label  = salt ^ "stored_static_homemades";;
let stored_dynamic_homemades_label = salt ^ "stored_dynamic_homemades";;
let proxy_count_label              = salt ^ "proxy_count";;
let dynamic_count_label            = salt ^ "dynamic_count";;


let of_concrete_object  crobj= 
   let g = Concrete_object_field.get_record crobj in 
   {
     Htscr_state_t.stored_proxies = Htscr_item.list_of_concrete_object(g stored_proxies_label);
          stored_static_homemades = Htscr_item.list_of_concrete_object(g stored_static_homemades_label);
         stored_dynamic_homemades = Htscr_item.list_of_concrete_object(g stored_dynamic_homemades_label);
                      proxy_count = Concrete_object_field.unwrap_int(g proxy_count_label);
                    dynamic_count = Concrete_object_field.unwrap_int(g dynamic_count_label);
   };;


let to_concrete_object  item= 
   Concrete_object_t.Record([ 
              stored_proxies_label , Htscr_item.list_to_concrete_object(item.Htscr_state_t.stored_proxies);
     stored_static_homemades_label , Htscr_item.list_to_concrete_object(item.Htscr_state_t.stored_static_homemades);
    stored_dynamic_homemades_label , Htscr_item.list_to_concrete_object(item.Htscr_state_t.stored_dynamic_homemades);
                 proxy_count_label , Concrete_object_field.wrap_int(item.Htscr_state_t.proxy_count);
               dynamic_count_label , Concrete_object_field.wrap_int(item.Htscr_state_t.dynamic_count);
   ]);;

end ;;

let of_concrete_object = Private.of_concrete_object ;;
let to_concrete_object = Private.to_concrete_object ;;


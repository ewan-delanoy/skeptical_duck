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
   let g = Concrete_object.get_record crobj in 
   {
     Htscr_state_t.stored_proxies = Htscr_item.list_of_concrete_object(g stored_proxies_label);
          stored_static_homemades = Htscr_item.list_of_concrete_object(g stored_static_homemades_label);
         stored_dynamic_homemades = Htscr_item.list_of_concrete_object(g stored_dynamic_homemades_label);
                      proxy_count = Crobj_converter.int_of_concrete_object(g proxy_count_label);
                    dynamic_count = Crobj_converter.int_of_concrete_object(g dynamic_count_label);
   };;


let to_concrete_object  st = 
   Concrete_object_t.Record([ 
              stored_proxies_label , Htscr_item.list_to_concrete_object(st.Htscr_state_t.stored_proxies);
     stored_static_homemades_label , Htscr_item.list_to_concrete_object(st.Htscr_state_t.stored_static_homemades);
    stored_dynamic_homemades_label , Htscr_item.list_to_concrete_object(st.Htscr_state_t.stored_dynamic_homemades);
                 proxy_count_label , Crobj_converter.int_to_concrete_object(st.Htscr_state_t.proxy_count);
               dynamic_count_label , Crobj_converter.int_to_concrete_object(st.Htscr_state_t.dynamic_count);
   ]);;

let check_for_already_registered_request st req= 
    let stored_ones = st.Htscr_state_t.stored_proxies @ st.Htscr_state_t.stored_static_homemades @ st.Htscr_state_t.stored_dynamic_homemades in 
    Option.seek (fun item->item.Htscr_item_t.original_request = req) stored_ones ;;

let treat_one_request config st request =
    match check_for_already_registered_request st request with 
    Some(_) -> None
    |None ->
    (  
    let old_proxy_count = st.Htscr_state_t.proxy_count 
    and old_dynamic_count = st.Htscr_state_t.dynamic_count in  
    match Htscr_item.compute_candidate config  ~old_proxy_count ~old_dynamic_count ~request with 
     None -> None 
    |Some(candidate) ->
      let new_st = (match candidate.Htscr_item_t.category with 
      Htscr_item_category_t.Proxy -> 
        {st with 
          Htscr_state_t.stored_proxies = (candidate :: st.Htscr_state_t.stored_proxies);
          Htscr_state_t.proxy_count = 1+(st.Htscr_state_t.proxy_count);
          }
      |Static_homemade -> 
        {st with 
        Htscr_state_t.stored_static_homemades = (candidate :: st.Htscr_state_t.stored_static_homemades);
        }
      |Dynamic_homemade -> 
        {st with 
          Htscr_state_t.stored_dynamic_homemades = (candidate :: st.Htscr_state_t.stored_dynamic_homemades);
          Htscr_state_t.dynamic_count = 1+(st.Htscr_state_t.dynamic_count);
          }
      ) in 
      Some(candidate,new_st)  
    );;     

let rec treat_several_requests config (accu,st,requests) = match requests with 
  [] -> (accu,st)
  |req :: other_requests -> 
     (
         match treat_one_request config st req with 
         None -> treat_several_requests config (accu,st,other_requests)
         |Some(item,new_st) ->  treat_several_requests config (item::accu,new_st,other_requests)
     ) ;;

let read_text config st ~text =
    let requests = Htscr_extract_requests_from_text.main config ~text in 
    treat_several_requests config ([],st,requests) ;;     

let empty_one =     
    {
      Htscr_state_t.stored_proxies = [];
           stored_static_homemades = [];
          stored_dynamic_homemades = [];
                       proxy_count = 0;
                     dynamic_count = 0;
    };;

end ;;

let empty_one = Private.empty_one ;;
let of_concrete_object = Private.of_concrete_object ;;
let read_text = Private.read_text ;;
let to_concrete_object = Private.to_concrete_object ;;


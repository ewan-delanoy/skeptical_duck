(*

#use"Text_editing/Html_scraping/htscr_extract_requests_from_text.ml";;

*)



module Private = struct 

let enumerate_requests_for_one_source_tag text source_tag =
    let temp1 = Substring.occurrences_of_in source_tag text in 
    Image.image (fun idx->
      let idx2 = idx + (String.length source_tag) in 
      let idx3 = Substring.leftmost_index_of_in_from "\"" text idx2 in 
      Cull_string.interval text idx2 (idx3-1)
    ) temp1 ;;

let enumerate_requests_for_several_source_tags text source_tags =   
      let temp1 = Image.image (enumerate_requests_for_one_source_tag text) source_tags in 
      List.flatten  temp1 ;;

end ;;


let main config ~text = 
   Private.enumerate_requests_for_several_source_tags text config.Htscr_config_t.source_tags ;;



  
  
  
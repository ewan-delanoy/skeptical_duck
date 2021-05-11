(*

#use"Text_editing/control_pdf_size.ml";;

*)

exception Uninitialized_system ;; 

module Private = struct 

let main_ref = ref None ;;

let get_ref () = match (!(main_ref)) with 
  None -> raise Uninitialized_system
  |Some(main_name,num_of_pages) -> (main_name,num_of_pages) ;;

end ;;  

let append_on_the_right (new_elt,new_elt_size) = 
  let (main_name,num_of_pages) = Private.get_ref () in  
  let answer=Coherent_pdf.append_on_the_right main_name new_elt   in 
  let _=(Private.main_ref:=Some(main_name,num_of_pages+new_elt_size)) in 
  answer ;;

let get_ref = Private.get_ref ;;

let insert_just_after inserted_arg page_idx = 
    let (main_name,num_of_pages) = get_ref () in  
    let answer=Coherent_pdf.insert_in_just_after 
    ~inserted_one: inserted_arg ~receiving_one: main_name
     ~page_number:page_idx ~initial_total_length:num_of_pages in 
    let _=(Private.main_ref:=Some(main_name,num_of_pages+1)) in 
    answer ;;

let initialize main_name num_of_pages = (Private.main_ref := Some(main_name,num_of_pages));;

let remove_page_range a b = 
    let (main_name,num_of_pages) = get_ref () in  
    let answer=Coherent_pdf.remove_page_range_in_in_a_total_of 
    ~range_start:a ~range_end:b ~deflated_one:main_name
    ~total_length:num_of_pages in 
    let _=(Private.main_ref:=Some(main_name,num_of_pages-(b-a+1))) in 
    answer ;;

let transfer_range_to_rightmost a b receiver= 
      let (main_name,num_of_pages) = get_ref () in  
      let answer=Coherent_pdf.transfer_range_to_rightmost
      ~range_start:a ~range_end:b ~deflated_one:main_name 
      ~total_length:num_of_pages ~receiving_one:receiver in 
      let _=(Private.main_ref:=Some(main_name,num_of_pages-(b-a+1))) in 
      answer ;;

  
  


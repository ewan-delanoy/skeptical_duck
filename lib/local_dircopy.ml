(*

#use"lib/local_dircopy.ml";;

*)



exception Error_during_fixes ;;
exception Waiting_for_fixes_confirmation ;;


module Private = struct 

let ready_to_be_initialized_ref = ref ([]: Local_dircopy_config_t.t list) ;; 

let display_fixes fixes = 
   let all_lines = ("The following fixes will be applied : " :: fixes)
    @["If you agree, do initialize() a second time."]
   in 
   let full_text = "\n\n\n" ^ (String.concat "\n\n" all_lines) ^ "\n\n\n" in 
   print_string full_text; flush stdout  ;; 

let is_a_digit c = 
   let i = int_of_char c in 
   (48<=i) && (i<=57) ;; 

let is_not_a_digit c = not(is_a_digit c) ;; 

let is_not_a_filler c = not(List.mem c [' ';'\t';'\r';'\n';'-';'_']) ;;


exception Add_next_index_to_name_exn of string ;;

let add_next_index_in_filename ldc next_idx_opt filename = 
   match next_idx_opt with 
   None -> raise (Add_next_index_to_name_exn(filename))
   | (Some next_idx) -> 
      let str = string_of_int next_idx in 
      let str2 = Strung.insert_repetitive_offset_on_the_left '0' ldc.Local_dircopy_config_t.allowed_number_of_digits str in 
      "v"^str2^"_"^filename ;;


exception Empty_subpath of string ;;

let standardized_name_opt ldc next_idx_opt filename = 
   if not(String.starts_with ~prefix:"v" filename)
   then Some(add_next_index_in_filename ldc next_idx_opt filename)
   else 
   let i1_opt = Strung.char_finder_from_inclusive_opt is_not_a_digit filename 2 in 
   if i1_opt = None
   then Some(add_next_index_in_filename ldc next_idx_opt filename)
   else 
   let i1 = Option.get i1_opt in
   let v_string = Cull_string.interval filename 2 (i1-1) in 
   let v_number = int_of_string v_string in
   let standardized_v_string = Strung.insert_repetitive_offset_on_the_left '0' ldc.Local_dircopy_config_t.allowed_number_of_digits (string_of_int v_number) in 
   let standardized_start = "v"^standardized_v_string^"_" in 
   let i2_opt = Strung.char_finder_from_inclusive_opt is_not_a_filler filename (i1+1) in 
   if i2_opt = None
   then raise(Empty_subpath(filename))
   else 
   let i2 = Option.get i2_opt in   
   let standardized_name = standardized_start ^ (Cull_string.cobeginning (i2-1) filename) in 
   if  filename <> standardized_name
   then Some(standardized_name)
   else None ;;
   

let formal_remote_dir ldc = Directory_name.of_string ldc.Local_dircopy_config_t.remote_dir ;; 

let remote_files ldc = Unix_again.beheaded_simple_ls (formal_remote_dir ldc) ;; 
 
let compute_fixes ldc = 
   let rem_loc = ldc.Local_dircopy_config_t.remote_dir in 
   let temp1 = Image.image (fun fn ->(fn,standardized_name_opt ldc None fn) ) (remote_files ldc) in 
   List.filter_map (
      fun (old_fn,new_fn_opt) -> match new_fn_opt with 
         None -> None 
       | Some new_fn -> 
         let old_ap = rem_loc ^ old_fn 
         and new_ap = rem_loc ^ new_fn in 
         Some( Filename.quote_command "mv" [old_ap;new_ap])
  ) temp1 ;; 

let redundancies ldc = 
   let p = ldc.Local_dircopy_config_t.allowed_number_of_digits in 
   let common_base = Image.image (fun s ->
     (int_of_string(Cull_string.interval s 2 (p+1)),
     Cull_string.cobeginning (p+2) s) ) (remote_files ldc) in 
   let nonredundant_filenames = 
      Ordered.sort Total_ordering.lex_for_strings 
       (Image.image snd common_base) in 
   let temp2 =
   Image.image (fun s->(s,List.filter_map(fun (idx,t)->
      if t=s then Some idx else None   
   ) common_base) ) nonredundant_filenames in 
   let redundant_filenames =List.filter (fun (_s,indices)->(List.length indices)>1) temp2 in 
   let nonredundant_indices = 
      Ordered.sort Total_ordering.for_integers 
       (Image.image fst common_base) in 
   let temp2 =
   Image.image (fun idx->(idx,List.filter_map(fun (idx2,t)->
      if idx2=idx then Some t else None   
   ) common_base) ) nonredundant_indices in 
   let redundant_indices =List.filter (fun (_idx,names)->
   (List.length names)>1) temp2 in  
   let n = List.hd(List.rev nonredundant_indices) in 
   let missing_indices = Ordered.setminus
     Total_ordering.for_integers (Int_range.range 1 n) 
       nonredundant_indices in
   (n,missing_indices,redundant_indices,redundant_filenames);; 

let ordered_filenames ldc = 
   let temp0 = Image.image (fun s ->
     (Cull_string.cobeginning (ldc.Local_dircopy_config_t.allowed_number_of_digits+2) s,s) ) (remote_files ldc) in 
   let temp1 = Image.image fst temp0 in 
   let temp2 = Ordered.sort Total_ordering.lex_for_strings temp1 in 
   Image.image (fun s->List.assoc s temp0 ) temp2;; 

let ordered_filenames_without_their_prefixed_indices ldc = 
   let temp1 = Image.image (fun s ->
     Cull_string.cobeginning 
     (ldc.Local_dircopy_config_t.allowed_number_of_digits+2) s ) (remote_files ldc) in 
   Ordered.sort Total_ordering.lex_for_strings temp1 ;; 

let formal_frontier_dir ldc = Directory_name.of_string ldc.Local_dircopy_config_t.frontier_dir ;; 


let files_to_be_transferred ldc = Unix_again.beheaded_simple_ls (formal_frontier_dir ldc) ;;
        
let dated_files ldc = Image.image (
           fun fn ->
              let full_fn = (ldc.Local_dircopy_config_t.frontier_dir) ^ fn in 
              ((Unix.stat full_fn).Unix.st_mtime, fn)
) (files_to_be_transferred ldc) ;;
        
        
let order_for_floats = ((fun (fl1:float) (fl2:float) ->
   Total_ordering.standard fl1 fl2   
) : float Total_ordering_t.t) ;; 

let order_for_dated_files = Total_ordering.product 
   order_for_floats Total_ordering.lex_for_strings ;; 

let transfer_message data = 
   if data =  [] 
   then "\n\n There are no files to update the remote with. \n\n"
   else   
   "\n\n\n"^(String.concat "\n" 
   ("The remote will be updated with the following files :\n"::
   data))^"\n\n\n" ;; 

let detect_redundant_dated_files ldc all_dated_files = 
   let older_files = ordered_filenames_without_their_prefixed_indices ldc in 
   let (temp1,new_dated_files) = List.partition (
      fun (_mtime,fn)->
          Ordered.mem Total_ordering.lex_for_strings fn older_files
   ) all_dated_files in 
   (Image.image snd temp1,new_dated_files) ;;

let message_on_redundant_files redundant_dated_files = 
   "\n\nThe following files are already in the database and "^
   "will not be transferred again : "^
   (String.concat ", " redundant_dated_files)^"\n\n\n" ;;

let transfer_commands ldc = 
   let all_dated_files = dated_files ldc in 
   let (redundant_dated_files,new_dated_files) = 
      detect_redundant_dated_files ldc all_dated_files in 
   let temp1 = Ordered.sort order_for_dated_files new_dated_files in 
   let temp2 = Image.image snd temp1 in 
   let n = List.length (remote_files ldc) in 
   let temp3 = Int_range.index_everything temp2 in 
   let temp4 = Image.image (fun (idx,fn) ->
     let new_fn = add_next_index_in_filename ldc (Some(n+idx)) fn in 
     let old_ap = (ldc.Local_dircopy_config_t.frontier_dir)  ^ fn 
     and new_ap = (ldc.Local_dircopy_config_t.remote_dir)^ new_fn in 
      Filename.quote_command "mv" [old_ap;new_ap]
   ) temp3 in 
   let temp5 = Image.image (fun (idx,fn) ->
      fn ^" -> v"^(string_of_int (n+idx)) 
    ) temp3 in 
   let _ = (
      if redundant_dated_files <> []
      then print_string(message_on_redundant_files redundant_dated_files); flush stdout) in 
   let _ = (print_string(transfer_message temp5); flush stdout) in 
   temp4 ;; 

let transfer_to_remote ldc = Image.image Sys.command (transfer_commands ldc);; 
        
let persisted_file_listing_dir_content ldc = 
   let s_ap_for_persistence =  
      (Dfa_root.connectable_to_subpath Fw_big_constant.This_World.root)^
      ldc.Local_dircopy_config_t.persisted_file_listing_dir_content in 
    Absolute_path.create_file_if_absent s_ap_for_persistence ;;
    
let persist_dir_content_listing ldc =     
   let filelist = String.concat "\n" (ordered_filenames ldc) in 
    let ap_for_persistence = persisted_file_listing_dir_content ldc in 
    Io.overwrite_with ap_for_persistence filelist ;;  

let update_from_config ldc = 
    let _ = transfer_to_remote ldc in 
    persist_dir_content_listing ldc ;;   

        
let initialize_when_there_are_no_fixes ldc = 
   let _ = persist_dir_content_listing ldc in 
   let filenames = ordered_filenames ldc in 
{
   Local_dircopy_t.config = ldc;
   remote_files = filenames;
   number_of_remote_files = List.length filenames ;
} ;;

  

let initialize ldc = 
   let fixes = compute_fixes ldc in 
   if fixes = []
   then initialize_when_there_are_no_fixes ldc  
   else 
   if List.mem ldc (!ready_to_be_initialized_ref) 
   then let results =  Image.image Sys.command fixes in 
        let _ = (ready_to_be_initialized_ref := 
          List.filter (fun x -> x<> ldc) (!ready_to_be_initialized_ref)
        )     in 
        if List.for_all (fun x -> x = 0) results  
        then initialize_when_there_are_no_fixes ldc
        else  raise Error_during_fixes
   else  let _ = (ready_to_be_initialized_ref := 
            ldc :: (!ready_to_be_initialized_ref)
        )     in 
        let _ = display_fixes fixes in 
        raise  Waiting_for_fixes_confirmation ;;        
        
let reload ldc = 
   let ap_for_persistence = persisted_file_listing_dir_content ldc in 
   let text = Io.read_whole_file ap_for_persistence in 
   let filenames = Lines_in_text.lines text in 
   {
      Local_dircopy_t.config = ldc;
      remote_files = filenames;
      number_of_remote_files = List.length filenames ;
   } ;;
      
let update locdir = 
   let ldc = locdir.Local_dircopy_t.config in 
   let _ = update_from_config ldc in 
initialize_when_there_are_no_fixes ldc ;;   

let show_files locdir = 
   let shortened_names = Image.image (Cull_string.shortened_version 60) locdir.Local_dircopy_t.remote_files in 
   let line_for_number = "    Total : "^(string_of_int(locdir.Local_dircopy_t.number_of_remote_files))^" files" in 
   let msg = "\n\n\n" ^ (String.concat "\n" shortened_names) ^ "\n\n" ^ line_for_number ^"\n\n\n" in 
   print_string msg ;;


end ;;

let initialize = Private.initialize ;; 

let redundancies = Private.redundancies ;;

let reload = Private.reload ;;

let show_files = Private.show_files ;;

let update = Private.update ;; 

        


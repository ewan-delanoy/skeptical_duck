(*

#use"lib/Text_editing/coherent_pdf.ml";;

*)


let workspace_directory=ref("");;
exception Incorrect_page_range of string*int*int;;
exception Number_of_pages_in_pdf_exn of string*string ;;
exception Bad_range_in_transfer of int * int ;;
exception Bad_range_in_removal of int * int ;;
exception Pdf_file_cannot_be_emptied of string ;;

module Helper = struct

  let cpdf = "/Applications/cpdf ";;

  let generic_extract_page_range pdfname (i,j) output_name=
    if i>j then raise(Incorrect_page_range(pdfname,i,j)) else 
    let si=string_of_int i and sj=string_of_int j in
    cpdf^pdfname^".pdf "^si^"-"^sj^" -o "^output_name
  ;;

  let usual_name_in_extract_page_range pdfname (i,j)=
    let si=string_of_int i and sj=string_of_int j in
    pdfname^"_from_"^si^"_to_"^sj^".pdf" ;;

  let small_pieces (i,j) max_piece_size=
         let num_of_pieces = Basic.frac_ceiling (j-i+1) max_piece_size in 
         Int_range.scale (
            fun k->
            let start=i+(k-1)*max_piece_size in
            let ending=min j (start+max_piece_size-1) in 
            (start,ending)
      ) 1 num_of_pieces;;  

  let intertwining_decomposition num_odd num_even =
    let min_num=min num_odd num_even 
    and max_num=max num_odd num_even in 
    let common_part=List.flatten(Int_range.scale (
       fun k->[(true,k);(false,k)]
    ) 1 min_num) 
    and comparison=num_odd>num_even in 
    let last_part=Int_range.scale (
        fun k->(comparison,k)
    ) (min_num+1) max_num in 
    common_part@last_part;;

  let number_of_pages_in_pdf full_pdfname =
    let temp = Io.read_reasonable_command ("mdls -name kMDItemNumberOfPages -raw  "^full_pdfname) in 
    try int_of_string(temp) with 
    _ -> raise(Number_of_pages_in_pdf_exn(temp,full_pdfname));;

  let pagesize_in_pdf full_pdfname =
    (  
    (Io.read_reasonable_command ("mdls -name kMDItemPageWidth -raw  "^full_pdfname)),
    (Io.read_reasonable_command ("mdls -name kMDItemPageHeight -raw  "^full_pdfname))
    );;


  let wrap_list_inside_workspace  l=
    let old_dir=Sys.getcwd() in 
    [Unix_command.cd (!workspace_directory)]@l@[Unix_command.cd old_dir];;
  
  let wrap_univar_inside_workspace f x1=wrap_list_inside_workspace(f x1);;
  let wrap_bivar_inside_workspace f x1 x2=wrap_list_inside_workspace(f x1 x2);;
  let wrap_trivar_inside_workspace f x1 x2 x3=wrap_list_inside_workspace(f x1 x2 x3);;
  let wrap_quadrivar_inside_workspace f x1 x2 x3 x4=wrap_list_inside_workspace(f x1 x2 x3 x4);;
  let wrap_quintivar_inside_workspace f x1 x2 x3 x4 x5=wrap_list_inside_workspace(f x1 x2 x3 x4 x5);;

end;;


module Bare = struct 


  let extract_page_range pdfname (i,j)=
    let output_name = Helper.usual_name_in_extract_page_range pdfname (i,j) in 
    [Helper.generic_extract_page_range pdfname (i,j) output_name];;

  let extract_even_pages pdfname=
    [Helper.cpdf^pdfname^".pdf even -o "^pdfname^"_even.pdf"];;

  let extract_odd_pages pdfname=
    [Helper.cpdf^pdfname^".pdf odd -o "^pdfname^"_odd.pdf"];;

  let explode (pdf_name_start,pdf_name_end) num_of_pages=
    let full_name=pdf_name_start^pdf_name_end 
    and ending=pdf_name_end^".pdf" in 
    Int_range.scale(
       fun i->
       let si=string_of_int i in 
       Helper.cpdf^full_name^".pdf "^si^"-"^si^" -o "^pdf_name_start^si^ending;
    ) 1 num_of_pages;;

  let implode (pdf_name_start,pdf_name_end)=
      let temp1=More_unix.quick_beheaded_complete_ls (!workspace_directory) 
      and ending=pdf_name_end^".pdf" in 
      let temp2=List.filter(
          fun fn->
            (Supstring.begins_with fn pdf_name_start)&&
            (Supstring.ends_with fn ending)
      ) temp1 in 
      let temp3=Option.filter_and_unpack (
         fun fn->
           let temp3=Cull_string.two_sided_cutting (pdf_name_start,ending) fn in 
           try (fun i->Some(i,fn))(int_of_string temp3) with 
           _->None
      ) temp2 in 
      let temp4=Ordered.sort  Total_ordering.standard2 temp3 in 
      let all_pages=String.concat " " (Image.image snd temp4) in 
      [Helper.cpdf^all_pages^" -o "^pdf_name_start^ending];;

  let implode_following_a_special_order (pdf_name_start,pdf_name_end) special_order=
      let ending = pdf_name_end^".pdf" in 
      let temp1=Option.filter_and_unpack (
         fun k->
          let full_filename = pdf_name_start^(string_of_int k)^ending in 
          let full_path = (!workspace_directory)^"/"^full_filename in 
          if Sys.file_exists full_path
          then Some full_filename 
          else None 
      ) special_order in 
      let all_pages=String.concat " " temp1 in 
      [Helper.cpdf^all_pages^" -o "^pdf_name_start^ending];;
  
  let cleanup_after_special_order (pdf_name_start,pdf_name_end) special_order=
      let ending = pdf_name_end^".pdf" in 
      Option.filter_and_unpack (
         fun k->
          let full_filename = pdf_name_start^(string_of_int k)^ending in 
          let full_path = (!workspace_directory)^"/"^full_filename in 
          if Sys.file_exists full_path 
          then Some ("rm "^full_filename) 
          else None 
      ) special_order ;;
   
  let prepare_recto_verso pdfname (i,j)=
        let excerpt_name = Helper.usual_name_in_extract_page_range pdfname (i,j)  in 
        let even_pages = excerpt_name^"_even.pdf"  
        and odd_pages = excerpt_name^"_odd.pdf" in 
        (extract_page_range pdfname (i,j))
        @
        (extract_even_pages excerpt_name)
        @
        (extract_odd_pages excerpt_name)
        @
        [
          "open -a /Applications/Preview.app "^(!workspace_directory)^"/"^odd_pages;
          "open -a /Applications/Preview.app "^(!workspace_directory)^"/"^even_pages;
        ];;

  let finish_recto_verso pdfname =
      [
         "rm "^pdfname^"_from_*.pdf";
      ];;

  let append_on_the_right file1 file2 =
     let fulle_filename1 =  (!workspace_directory)^"/"^file1^".pdf" in 
     if Sys.file_exists fulle_filename1 
     then [
          Helper.cpdf^file1^".pdf "^file2^".pdf -o wghartnjklmiopfwhhokuuu.pdf";
          "mv wghartnjklmiopfwhhokuuu.pdf "^file1^".pdf" ;
         ]
     else ["cp "^file2^".pdf "^file1^".pdf"]  ;;

  let cut_in_two ~pdfname ~first_half_length ~total_length =
        let half1=pdfname^"_half1.pdf"
        and half2=pdfname^"_half2.pdf" in 
        [
        (Helper.generic_extract_page_range pdfname (1,first_half_length) half1);
        (Helper.generic_extract_page_range pdfname (first_half_length+1,total_length) half2)
        ];;

  let unlabeled_cut_in_two pdfname first_half_length total_length =
     cut_in_two 
       ~pdfname:pdfname 
        ~first_half_length:first_half_length 
         ~total_length:total_length;;

  let merge parts whole=
      let parts=Image.image (fun name->name^".pdf") parts in 
      let joined_parts=String.concat " " parts in 
      [Helper.cpdf^joined_parts^" -o "^whole^".pdf"];;

  let insert_in_just_after ~inserted_one ~receiving_one ~page_number ~initial_total_length=
        (cut_in_two ~pdfname:receiving_one ~first_half_length:page_number ~total_length:initial_total_length)
        @
        (merge [receiving_one^"_half1";inserted_one;receiving_one^"_half2"] receiving_one )
        @
        [
           "rm "^receiving_one^"_half1.pdf";
           "rm "^receiving_one^"_half2.pdf"; 
        ];;
  
  let unlabeled_insert_in_just_after inserted_one receiving_one page_number initial_total_length=
    insert_in_just_after
      ~inserted_one:inserted_one 
       ~receiving_one:receiving_one 
        ~page_number:page_number 
        ~initial_total_length:initial_total_length;;

  let replace_first_page_number_in_by  ~receiving_one ~inserted_one  ~total_length=
        (cut_in_two ~pdfname:receiving_one ~first_half_length:1 ~total_length:total_length)
        @
        (merge [inserted_one;receiving_one^"_half2"] receiving_one )
        @
        [
           "rm "^receiving_one^"_half1.pdf";
           "rm "^receiving_one^"_half2.pdf"; 
        ];;

  let replace_nonfirst_page_number_in_by ~page_number ~receiving_one ~inserted_one  ~total_length=
        let temp=receiving_one^"_half2" in 
        (cut_in_two ~pdfname:receiving_one ~first_half_length:(page_number-1) ~total_length:total_length)
        @
        (cut_in_two ~pdfname:temp ~first_half_length:1 ~total_length:(total_length-page_number+1))
        @
        (merge [receiving_one^"_half1";inserted_one;temp^"_half2"] receiving_one )
        @
        [
           "rm "^receiving_one^"_half1.pdf";
           "rm "^receiving_one^"_half2.pdf"; 
           "rm "^receiving_one^"_half2_half1.pdf"; 
           "rm "^receiving_one^"_half2_half2.pdf"; 
        ];;

  let replace_last_page_number_in_by  ~receiving_one ~inserted_one  ~total_length=
        (cut_in_two ~pdfname:receiving_one ~first_half_length:(total_length-1) ~total_length:total_length)
        @
        (merge [receiving_one^"_half1";inserted_one] receiving_one )
        @
        [
           "rm "^receiving_one^"_half1.pdf";
           "rm "^receiving_one^"_half2.pdf"; 
        ];;      

  let replace_page_number_in_by ~page_number ~receiving_one ~inserted_one  ~total_length=
    if page_number=1
    then replace_first_page_number_in_by  ~receiving_one ~inserted_one  ~total_length
    else 
    if page_number=total_length
    then replace_last_page_number_in_by  ~receiving_one ~inserted_one  ~total_length
    else 
    replace_nonfirst_page_number_in_by ~page_number ~receiving_one ~inserted_one  ~total_length;;

  let unlabeled_replace_page_number_in_by page_number receiving_one inserted_one  total_length=
     replace_page_number_in_by
    ~page_number:page_number 
     ~receiving_one:receiving_one
      ~inserted_one:inserted_one  
       ~total_length:total_length;;

  let remove_initial_range_in_a_total_of range_length deflated_one old_total_length = 
       (cut_in_two ~pdfname:deflated_one ~first_half_length:range_length ~total_length:old_total_length)
        @
        (["mv "^deflated_one^"_half2"^".pdf "^deflated_one^".pdf"]  )
        @
        [
           "rm "^deflated_one^"_half1.pdf";
        ];;

  let remove_nonborder_range_in_a_total_of (range_start,range_end) deflated_one old_total_length=
        let range_length=range_end-range_start+1
        and h2=deflated_one^"_half2" in 
        (cut_in_two ~pdfname:deflated_one ~first_half_length:(range_start-1) ~total_length:old_total_length)
        @
        (cut_in_two ~pdfname:h2 ~first_half_length:range_length ~total_length:(old_total_length-range_start+1))
        @
        (merge [deflated_one^"_half1";h2^"_half2"] deflated_one )
        @
        [
           "rm "^deflated_one^"_half1.pdf";
           "rm "^deflated_one^"_half2.pdf"; 
           "rm "^deflated_one^"_half2_half1.pdf"; 
           "rm "^deflated_one^"_half2_half2.pdf"; 
        ];;

  let remove_final_range_in_a_total_of range_length deflated_one  old_total_length=
        (cut_in_two ~pdfname:deflated_one ~first_half_length:(old_total_length-range_length) ~total_length:old_total_length)
        @
        ["mv "^deflated_one^"_half1"^".pdf "^deflated_one^".pdf"]  
        @
        [
           "rm "^deflated_one^"_half2.pdf";
        ];;          

  let remove_page_range_in_a_total_of (range_start,range_end) deflated_one old_total_length =
      let range_length = range_end - range_start +1 in 
      if (range_length < 1) || (range_start < 1) || (range_end > old_total_length) 
      then raise(Bad_range_in_removal(range_start,range_end))  
      else if range_start = 1
      then remove_initial_range_in_a_total_of range_length deflated_one old_total_length  
      else if range_end = old_total_length 
      then remove_final_range_in_a_total_of range_length deflated_one  old_total_length
      else remove_nonborder_range_in_a_total_of (range_start,range_end) deflated_one old_total_length ;;     


  let cut_into_small_pieces pdfname (i,j) max_piece_size=
         let ranges = Helper.small_pieces (i,j) max_piece_size in 
         let base  = Image.image (
            fun (k,pair)->(k,pair,Helper.usual_name_in_extract_page_range pdfname pair)
         )(Int_range.index_everything ranges) in  
         let part1=Image.image (fun (_idx,pair,name)->
            Helper.generic_extract_page_range pdfname pair name
         ) base
         and part2=Image.image(
            fun (idx,_pair,name)->
            "mv "^name^" part"^(string_of_int idx)^".pdf"
         ) base in  
         part1@part2 ;;

  let rename old_pdfname new_pdfname=
       ["mv "^old_pdfname^".pdf "^new_pdfname^".pdf"];;
      
  let upside_down pdfname =  
       [
          Helper.cpdf^" -rotate-contents 180 "^pdfname^".pdf -o wghartnjklmiopfwhhokuuu.pdf";
          "mv wghartnjklmiopfwhhokuuu.pdf "^pdfname^".pdf" ;  
        ];;

  let lay_down pdfname =  
       [
          Helper.cpdf^" -rotate-contents 90 "^pdfname^".pdf -o wghartnjklmiopfwhhokuuu.pdf";
          "mv wghartnjklmiopfwhhokuuu.pdf "^pdfname^".pdf" ;
        ];;
  
  let import pdfname=
     ["cp "^pdfname^".pdf ."];;

  let export ~pdfname ~new_location=
     ["mv "^pdfname^".pdf "^new_location];;  

  let unlabeled_export  pdfname new_location=
     export ~pdfname:pdfname ~new_location:new_location;;   

  let intertwine ~odd_pages ~even_pages ~num_odd ~num_even ~final_name=
    let temp1=Helper.intertwining_decomposition num_odd num_even in 
    let pages=Image.image (
      fun (is_odd,idx)->
        let s_idx=string_of_int idx in 
        if is_odd 
        then odd_pages^s_idx
        else even_pages^s_idx
    ) temp1 in 
    let removals=Image.image (fun page->"rm "^page^".pdf") pages in  
    (explode (odd_pages,"") num_odd)@
    (explode (even_pages,"") num_even)@
    (merge pages final_name)@
    removals;;

  let unlabeled_intertwine odd_pages even_pages num_odd num_even final_name=
    intertwine 
      ~odd_pages:odd_pages 
        ~even_pages:even_pages 
          ~num_odd:num_odd 
           ~num_even:num_even 
             ~final_name:final_name;;

  let delete_file pdfname=["rm "^pdfname^".pdf"];;   
  
  let delete_file_if_exists pdfname=
     if Sys.file_exists (pdfname^".pdf") 
     then ["rm "^pdfname^".pdf"]
     else [];;   

  let transfer_range_to_rightmost_in_usual_case deflated_one receiving_one range_start range_end initial_total_length=
     let range_length = range_end - range_start + 1 in 
     if (range_length < 0) || (range_start < 1) || (range_end > initial_total_length)
     then raise(Bad_range_in_transfer(range_start,range_end)) 
     else
     let h1 =  deflated_one^"_half1" and h2 =  deflated_one^"_half2" in 
     (cut_in_two ~pdfname:deflated_one ~first_half_length:(range_start-1) ~total_length:initial_total_length)
     @
     (cut_in_two ~pdfname:h2 ~first_half_length:range_length ~total_length:(initial_total_length-range_start+1))
     @
     (merge [h1;h2^"_half2"] deflated_one )
     @
     (append_on_the_right receiving_one (h2^"_half1"))
     @
     [
        "rm "^h2^"_half1.pdf";
        "rm "^h2^"_half2.pdf"; 
        "rm "^h2^".pdf";
        "rm "^h1^".pdf"
     ];;

  let transfer_range_to_rightmost_in_limit_case deflated_one receiving_one range_end initial_total_length=
     let range_length = range_end  in 
     if (range_length < 0)  || (range_end > initial_total_length)
     then raise(Bad_range_in_transfer(1,range_end)) 
     else
     let h1 =  deflated_one^"_half1" and h2 =  deflated_one^"_half2" in 
     (cut_in_two ~pdfname:deflated_one ~first_half_length:range_length ~total_length:initial_total_length)
     @
     (rename h2 deflated_one )
     @
     (append_on_the_right receiving_one h1)
     @
     [
        "rm "^h1^".pdf"
     ];;

     let transfer_range_to_rightmost deflated_one receiving_one range_start range_end initial_total_length=
       if range_start = 1 
       then if range_end = initial_total_length 
            then raise(Pdf_file_cannot_be_emptied(deflated_one))
            else transfer_range_to_rightmost_in_limit_case deflated_one receiving_one range_end initial_total_length  
       else transfer_range_to_rightmost_in_usual_case deflated_one receiving_one range_start range_end initial_total_length;; 

  module Walker = struct 

  let blank_name = "blank";;
  let gas_factory = (Sys.getenv "HOME")^ "/Desktop/Gas_factory" ;;
  let walker_name_start = "walker";; 
  let walker_name_end = "_wghartnjklmiopfwhhokuuu";; 
  let walker_name = walker_name_start ^ walker_name_end;;
  

  let append_blank r =
     List.flatten(Int_range.scale (
      fun _->[
          Helper.cpdf^walker_name^".pdf "^blank_name^".pdf -o wghartnjklmiopfwhhokuuu.pdf";
          "mv wghartnjklmiopfwhhokuuu.pdf "^walker_name^".pdf" ;
      ]
     ) 1 r);;

  let init_append_and_explode pdfname r num_of_pages=
      ["cp "^pdfname^".pdf "^walker_name^".pdf"]@
     (append_blank r) @ 
     (explode (walker_name_start,walker_name_end) num_of_pages) ;;
  
  let implode_and_finish special_order old_path =
      (implode_following_a_special_order  
         (walker_name_start,walker_name_end) special_order)@
      (cleanup_after_special_order 
         (walker_name_start,walker_name_end) special_order)@
      [
        "mv "^walker_name^".pdf "^old_path^"_adurzhiet.pdf";
        "rm -rf "^gas_factory
      ] ;;

   end ;;
end;;


module Command = struct 
  
  let uni=Helper.wrap_univar_inside_workspace;;
  let bi=Helper.wrap_bivar_inside_workspace;;
  let tri=Helper.wrap_trivar_inside_workspace;;
  let qdi=Helper.wrap_quadrivar_inside_workspace;;
  let qti=Helper.wrap_quintivar_inside_workspace;;

  let append_on_the_right =bi Bare.append_on_the_right;;
  let cut_in_two =tri Bare.unlabeled_cut_in_two;;
  let cut_into_small_pieces =tri Bare.cut_into_small_pieces;;
  let delete_file =uni Bare.delete_file;;
  let delete_file_if_exists =uni Bare.delete_file_if_exists;;
  let explode =bi Bare.explode;;
  let export =bi Bare.unlabeled_export;;
  let extract_page_range =bi Bare.extract_page_range;;
  let extract_even_pages =uni Bare.extract_even_pages;;
  let extract_odd_pages =uni Bare.extract_odd_pages;;
  let finish_recto_verso =uni Bare.finish_recto_verso;;
  let implode =uni Bare.implode;;
  let import =uni Bare.import;;
  let insert_in_just_after =qdi Bare.unlabeled_insert_in_just_after;;
  let intertwine =qti Bare.unlabeled_intertwine;;
  let lay_down =uni Bare.lay_down;; 
  let merge =bi Bare.merge;;
  let prepare_recto_verso =bi Bare.prepare_recto_verso;;
  let remove_page_range_in_in_a_total_of = tri Bare.remove_page_range_in_a_total_of;;
  let rename =bi Bare.rename;;
  let replace_page_number_in_by=qdi Bare.unlabeled_replace_page_number_in_by;;
  let transfer_range_to_rightmost =qti Bare.transfer_range_to_rightmost ;;
  let upside_down =uni Bare.upside_down;; 
     

  module Walker = struct 

  

  let init_append_and_explode = tri  Bare.Walker.init_append_and_explode ;;
  let implode_and_finish = bi Bare.Walker.implode_and_finish ;; 

  end ;;

end;;

module Other_Tools = struct 

   let create_blank_page_with_prescribed_size (exact_width,exact_height) =
      let width = Cull_string.before_rightmost_possibly_all exact_width '.'
      and height =  Cull_string.before_rightmost_possibly_all exact_height '.' in 
      let source_for_blank_page = String.concat "\n"
      ["%PDF-1.4"; "1 0 obj<</Type/Catalog/Pages 2 0 R>>endobj";
       "2 0 obj<</Type/Pages/Count 1/Kids[3 0 R]>>endobj";
       "3 0 obj<</Type/Page/MediaBox[0 0 "^width^" "^height^"]/Parent 2 0 R/Resources<<>>>>endobj";
       "xref"; "0 4"; "0000000000 65535 f "; "0000000009 00000 n ";
      "0000000052 00000 n "; "0000000101 00000 n ";
      "trailer<</Size 4/Root 1 0 R>>"; "startxref"; "178"; "%%EOF"] in 
    let full_path_for_blank_page = (!(workspace_directory))^"/blank.pdf" in 
    let blank_page_ap = Absolute_path.create_file_if_absent full_path_for_blank_page in 
    let _ = Io.overwrite_with blank_page_ap source_for_blank_page in 
    blank_page_ap ;;

    let registered_directories = 
      Image.image Directory_name.of_string
      [
        (Sys.getenv "HOME")^"/Desktop";
      ] ;; 

let make_booklet_naively first_arg =   
  let _ = Unix_command.uc ("mkdir -p "^Bare.Walker.gas_factory) in 
  let _=(workspace_directory:= Bare.Walker.gas_factory) in 
  let old_ap = Directory_name.find_file_with_directory_list first_arg registered_directories in 
  let s_old_ap = Absolute_path.to_string old_ap in 
  let old_path = Cull_string.before_rightmost s_old_ap '.' in 
  let old_name = Cull_string.after_rightmost old_path '/' in 
  let acts1 = Image.image Unix_command.uc (Command.import  old_path) in 
  let initial_nbr_of_pages = Helper.number_of_pages_in_pdf s_old_ap in 
  let (width,height) = Helper.pagesize_in_pdf s_old_ap in  
  let rounded_offset = 
   (let r = (initial_nbr_of_pages) mod 4 in 
   if r=0 then 0 else 4-r ) in 
  let rounded_nbr_of_pages =  initial_nbr_of_pages + rounded_offset in 
  let _ = create_blank_page_with_prescribed_size (width,height) in 
  let acts2 = Image.image Unix_command.uc 
  (Command.Walker.init_append_and_explode  old_name rounded_offset rounded_nbr_of_pages) in 
  let special_order = Int_range.scale (
    fun j->let k=(j/4)in  match (j mod 4) with 
      0 -> rounded_nbr_of_pages-2*k+1 
    |1 ->  rounded_nbr_of_pages-2*k
    |2 -> 2*k+1 
    |_ -> 2*k+2
  ) 1 rounded_nbr_of_pages in 
  let acts3 = Image.image Unix_command.uc 
  (Command.Walker.implode_and_finish special_order old_path) in 
  if List.for_all (fun t->t=0) (acts1@acts2@acts3)
  then let msg="\n Setu. Prest eo an teul "^old_path^"_adurzhiet.pdf\n" in 
       print_string msg;;
 
  let make_booklet first_arg = try make_booklet_naively first_arg  with 
   Directory_name.File_not_found (_,_) ->
    let msg="\nN'em eus kavet "^first_arg^" e neblec'h. Amprouit eo skrivet mat an anv.\n" in 
    print_string msg;;  


end ;;


let append_on_the_right file1 file2 = Image.image Unix_command.uc 
  (Command.append_on_the_right file1 file2);;
 
let cut_into_small_pieces pdfname (i,j) max_piece_size=
  Image.image Unix_command.uc 
  (Command.cut_into_small_pieces pdfname (i,j) max_piece_size);; 

let cut_in_two ~pdfname ~first_half_length ~total_length =
  Image.image Unix_command.uc 
  (Command.cut_in_two pdfname first_half_length total_length);;

let delete_file  pdfname=Image.image Unix_command.uc 
  (Command.delete_file  pdfname);;

let delete_file_if_exists  pdfname=Image.image Unix_command.uc 
  (Command.delete_file_if_exists  pdfname);;  

let extract_page_range pdfname (i,j)=Image.image Unix_command.uc 
  (Command.extract_page_range pdfname (i,j));;

let extract_even_pages  pdfname=Image.image Unix_command.uc 
  (Command.extract_even_pages  pdfname);;

let extract_odd_pages  pdfname=Image.image Unix_command.uc 
  (Command.extract_odd_pages  pdfname);;

let explode (pdf_name_start,pdf_name_end) num_of_pages=
   Explicit.image Unix_command.uc 
  (Command.explode  (pdf_name_start,pdf_name_end) num_of_pages);; 

let export ~pdfname ~new_location=Image.image Unix_command.uc 
  (Command.export pdfname new_location);; 

let finish_recto_verso pdfname =Image.image Unix_command.uc 
  (Command.finish_recto_verso pdfname );;

let implode (pdf_name_start,pdf_name_end)=
   Image.image Unix_command.uc 
  (Command.implode  (pdf_name_start,pdf_name_end));; 

let import pdfname=Image.image Unix_command.uc 
  (Command.import  pdfname);;


let insert_in_just_after ~inserted_one ~receiving_one ~page_number ~initial_total_length=Image.image Unix_command.uc 
 (Command.insert_in_just_after inserted_one receiving_one page_number initial_total_length);;

let intertwine ~odd_pages ~even_pages ~num_odd ~num_even ~final_name=Image.image Unix_command.uc 
 (Command.intertwine odd_pages even_pages num_odd num_even final_name);;
  

let lay_down  pdfname=Image.image Unix_command.uc 
  (Command.lay_down  pdfname);;

let merge parts whole=
  Image.image Unix_command.uc 
  (Command.merge parts whole);; 

let number_of_pages_in_pdf ap = 
   Helper.number_of_pages_in_pdf (Absolute_path.to_string ap);;

let pagesize_in_pdf ap = 
   Helper.pagesize_in_pdf (Absolute_path.to_string ap);;


let prepare_recto_verso pdfname (i,j)=Image.image Unix_command.uc 
  (Command.prepare_recto_verso pdfname (i,j));;


let replace_page_number_in_by ~page_number ~receiving_one ~inserted_one  ~total_length=Image.image Unix_command.uc 
   (Command.replace_page_number_in_by page_number receiving_one inserted_one  total_length );;

let remove_page_number_in_in_a_total_of ~page_number ~deflated_one  ~total_length=
    Image.image Unix_command.uc 
    (Command.remove_page_range_in_in_a_total_of (page_number,page_number) deflated_one  total_length);;

let remove_page_range_in_in_a_total_of ~range_start ~range_end ~deflated_one  ~total_length=
    Image.image Unix_command.uc 
    (Command.remove_page_range_in_in_a_total_of (range_start,range_end) deflated_one  total_length);;

let rename  old_pdfname new_pdfname=
   Image.image Unix_command.uc 
  (Command.rename  old_pdfname new_pdfname);; 

let transfer_range_to_rightmost ~deflated_one ~receiving_one ~range_start ~range_end ~total_length=Image.image Unix_command.uc 
  (Command.transfer_range_to_rightmost deflated_one receiving_one range_start range_end total_length);;
   

let upside_down  pdfname=Image.image Unix_command.uc 
  (Command.upside_down  pdfname);;
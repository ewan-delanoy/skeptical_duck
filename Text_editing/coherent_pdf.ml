(*

#use "Text_editing/coherent_pdf.ml";;

*)


let workspace_directory=ref("");;
exception Incorrect_page_range of string*int*int;;


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
         Ennig.doyle (
            fun k->
            let start=i+(k-1)*max_piece_size in
            let ending=min j (start+max_piece_size-1) in 
            (start,ending)
      ) 1 num_of_pieces;;  

  let intertwining_decomposition num_odd num_even =
    let min_num=min num_odd num_even 
    and max_num=max num_odd num_even in 
    let common_part=List.flatten(Ennig.doyle (
       fun k->[(true,k);(false,k)]
    ) 1 min_num) 
    and comparison=num_odd>num_even in 
    let last_part=Ennig.doyle (
        fun k->(comparison,k)
    ) (min_num+1) max_num in 
    common_part@last_part;;

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
    Ennig.doyle(
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
      let temp4=Ordered.forget_order (Tidel2.diforchan temp3) in 
      let all_pages=String.concat " " (Image.image snd temp4) in 
      [Helper.cpdf^all_pages^" -o "^pdf_name_start^ending];;

   
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
      [
          Helper.cpdf^file1^".pdf "^file2^".pdf -o wghartnjklmiopfwhhokuuu.pdf";
          "mv wghartnjklmiopfwhhokuuu.pdf "^file1^".pdf" ;
      ];;

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
    then replace_first_page_number_in_by  receiving_one inserted_one  total_length
    else 
    if page_number=total_length
    then replace_last_page_number_in_by  receiving_one inserted_one  total_length
    else 
    replace_nonfirst_page_number_in_by page_number receiving_one inserted_one  total_length;;

  let unlabeled_replace_page_number_in_by page_number receiving_one inserted_one  total_length=
     replace_page_number_in_by
    ~page_number:page_number 
     ~receiving_one:receiving_one
      ~inserted_one:inserted_one  
       ~total_length:total_length;;

  let cut_into_small_pieces pdfname (i,j) max_piece_size=
         let ranges = Helper.small_pieces (i,j) max_piece_size in 
         let base  = Image.image (
            fun (k,pair)->(k,pair,Helper.usual_name_in_extract_page_range pdfname pair)
         )(Ennig.index_everything ranges) in  
         let part1=Image.image (fun (idx,pair,name)->
            Helper.generic_extract_page_range pdfname pair name
         ) base
         and part2=Image.image(
            fun (idx,pair,name)->
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

  let remove pdfname=["rm "^pdfname^".pdf"];;           
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
  let explode =bi Bare.explode;;
  let export =bi Bare.unlabeled_export;;
  let extract_page_range =bi Bare.extract_page_range;;
  let extract_even_pages =uni Bare.extract_even_pages;;
  let extract_odd_pages =uni Bare.extract_even_pages;;
  let finish_recto_verso =uni Bare.finish_recto_verso;;
  let implode =uni Bare.implode;;
  let import =uni Bare.import;;
  let insert_in_just_after =qdi Bare.unlabeled_insert_in_just_after;;
  let intertwine =qti Bare.unlabeled_intertwine;;
  let lay_down =uni Bare.lay_down;; 
  let merge =bi Bare.merge;;
  let prepare_recto_verso =bi Bare.prepare_recto_verso;;
  let remove =uni Bare.remove;;
  let rename =bi Bare.rename;;
  let replace_page_number_in_by=qdi Bare.unlabeled_replace_page_number_in_by;;
  let upside_down =uni Bare.upside_down;; 
     

end;;

let append_on_the_right file1 file2 = Image.image Unix_command.uc 
  (Command.append_on_the_right file1 file2);;
 
let cut_into_small_pieces pdfname (i,j) max_piece_size=
  Image.image Unix_command.uc 
  (Command.cut_into_small_pieces pdfname (i,j) max_piece_size);; 

let cut_in_two ~pdfname ~first_half_length ~total_length =
  Image.image Unix_command.uc 
  (Command.cut_in_two pdfname first_half_length total_length);;

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

let prepare_recto_verso pdfname (i,j)=Image.image Unix_command.uc 
  (Command.prepare_recto_verso pdfname (i,j));;

let remove  pdfname=Image.image Unix_command.uc 
  (Command.remove  pdfname);;

let replace_page_number_in_by ~page_number ~receiving_one ~inserted_one  ~total_length=Image.image Unix_command.uc 
   (Command.replace_page_number_in_by page_number receiving_one inserted_one  total_length );;

let rename  old_pdfname new_pdfname=
   Image.image Unix_command.uc 
  (Command.rename  old_pdfname new_pdfname);; 

let upside_down  pdfname=Image.image Unix_command.uc 
  (Command.upside_down  pdfname);;
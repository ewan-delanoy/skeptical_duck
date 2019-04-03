(*

#use "Text_editing/coherent_pdf.ml";;

Utility to cut a large PDF into easily printable chunks.
Each chunk is at most 20 pages long, and separated into
odd/even parts to allow for recto-verso printing. 

*)


let workspace_directory=ref("");;

module Command = struct 
  
let cpdf = "/Applications/cpdf ";;

let bare_extract_page_range pdfname (i,j) output_name=
  let si=string_of_int i and sj=string_of_int j in
   cpdf^pdfname^".pdf "^si^"-"^sj^" -o "^output_name
  ;;

let internal_extract_page_range pdfname (i,j) output_name=
  let old_dir=Sys.getcwd() in 
  [
     Unix_command.cd (!workspace_directory);
     (bare_extract_page_range pdfname (i,j) output_name); 
     Unix_command.cd old_dir;
  ];;

let name_in_extract_page_range pdfname (i,j)=
  let si=string_of_int i and sj=string_of_int j in
  pdfname^"_from_"^si^"_to_"^sj^".pdf" ;;


let extract_page_range pdfname (i,j)=
  let output_name = name_in_extract_page_range pdfname (i,j) in 
  internal_extract_page_range pdfname (i,j) output_name;;

let extract_even_pages pdfname=
  let old_dir=Sys.getcwd() in 
  [
     Unix_command.cd (!workspace_directory);
     cpdf^pdfname^".pdf even -o "^pdfname^"_even.pdf";
     Unix_command.cd old_dir;
  ];;

   let extract_odd_pages pdfname=
   let old_dir=Sys.getcwd() in 
  [
     Unix_command.cd (!workspace_directory);
     cpdf^pdfname^".pdf odd -o "^pdfname^"_odd.pdf";
     Unix_command.cd old_dir;
  ];;

   let explode (pdf_name_start,pdf_name_end) num_of_pages=
    let old_dir=Sys.getcwd() in 
    let full_name=pdf_name_start^pdf_name_end 
    and ending=pdf_name_end^".pdf" in 
    let temp1=Ennig.doyle(
       fun i->
       let si=string_of_int i in 
       cpdf^full_name^".pdf "^si^"-"^si^" -o "^pdf_name_start^si^ending;
    ) 1 num_of_pages in 
   [
      Unix_command.cd (!workspace_directory)
   ]@   
     temp1
   @[   
     Unix_command.cd old_dir;
   ];;

   let implode (pdf_name_start,pdf_name_end)=
      let old_dir=Sys.getcwd() in 
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
      let main=cpdf^all_pages^" -o "^pdf_name_start^ending in 
      [
         Unix_command.cd (!workspace_directory);
         main; 
         Unix_command.cd old_dir;
      ];;

   
    let prepare_recto_verso pdfname (i,j)=
        let si=string_of_int i and sj=string_of_int j in
        let excerpt_name = pdfname^"_from_"^si^"_to_"^sj  in 
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
      let old_dir=Sys.getcwd() in 
      [
         Unix_command.cd (!workspace_directory);  
         "rm "^pdfname^"_from_*.pdf";
         Unix_command.cd old_dir;
      ];;

    let append_on_the_right file1 file2 =
       let old_dir=Sys.getcwd() in 
      [
          Unix_command.cd (!workspace_directory);
          cpdf^file1^".pdf "^file2^".pdf -o wghartnjklmiopfwhhokuuu.pdf";
          "mv wghartnjklmiopfwhhokuuu.pdf "^file1^".pdf" ;
          Unix_command.cd old_dir;
        ];;

     let cut_in_two ~pdfname ~first_half_length ~total_length =
        let half1=pdfname^"_half1.pdf"
        and half2=pdfname^"_half2.pdf" in 
        (internal_extract_page_range pdfname (1,first_half_length) half1)
        @
        (internal_extract_page_range pdfname (first_half_length+1,total_length) half2);;

     let merge parts whole=
      let old_dir=Sys.getcwd() in 
      let parts=Image.image (fun name->name^".pdf") parts in 
      let joined_parts=String.concat " " parts in 
      let main=cpdf^joined_parts^" -o "^whole^".pdf" in 
      [
         Unix_command.cd (!workspace_directory);
         main; 
         Unix_command.cd old_dir;
      ];;

      let insert_in_just_after ~inserted_one ~receiving_one ~page_number ~initial_total_length=
        let old_dir=Sys.getcwd() in 
        (cut_in_two ~pdfname:receiving_one ~first_half_length:page_number ~total_length:initial_total_length)
        @
        (merge [receiving_one^"_half1";inserted_one;receiving_one^"_half2"] receiving_one )
        @
        [
           Unix_command.cd (!workspace_directory);
           "rm "^receiving_one^"_half1.pdf";
           "rm "^receiving_one^"_half2.pdf"; 
           Unix_command.cd old_dir;
        ];;

      let replace_first_page_number_in_by  ~receiving_one ~inserted_one  ~total_length=
        let old_dir=Sys.getcwd()  in 
        (cut_in_two ~pdfname:receiving_one ~first_half_length:1 ~total_length:total_length)
        @
        (merge [inserted_one;receiving_one^"_half2"] receiving_one )
        @
        [
           Unix_command.cd (!workspace_directory);
           "rm "^receiving_one^"_half1.pdf";
           "rm "^receiving_one^"_half2.pdf"; 
           Unix_command.cd old_dir;
        ];;

      let replace_nonfirst_page_number_in_by ~page_number ~receiving_one ~inserted_one  ~total_length=
        let old_dir=Sys.getcwd() 
        and temp=receiving_one^"_half2" in 
        (cut_in_two ~pdfname:receiving_one ~first_half_length:(page_number-1) ~total_length:total_length)
        @
        (cut_in_two ~pdfname:temp ~first_half_length:1 ~total_length:(total_length-page_number+1))
        @
        (merge [receiving_one^"_half1";inserted_one;temp^"_half2"] receiving_one )
        @
        [
           Unix_command.cd (!workspace_directory);
           "rm "^receiving_one^"_half1.pdf";
           "rm "^receiving_one^"_half2.pdf"; 
           "rm "^receiving_one^"_half2_half1.pdf"; 
           "rm "^receiving_one^"_half2_half2.pdf"; 
           Unix_command.cd old_dir;
        ];;
            
      let replace_page_number_in_by ~page_number ~receiving_one ~inserted_one  ~total_length=
        if page_number=1
        then replace_first_page_number_in_by  receiving_one inserted_one  total_length
        else replace_nonfirst_page_number_in_by page_number receiving_one inserted_one  total_length;;


      let small_pieces (i,j) max_piece_size=
         let num_of_pieces = Basic.frac_ceiling (j-i+1) max_piece_size in 
         Ennig.doyle (
            fun k->
            let start=i+(k-1)*max_piece_size in
            let ending=min j (start+max_piece_size-1) in 
            (start,ending)
      ) 1 num_of_pieces;;
     
       let cut_into_small_pieces pdfname (i,j) max_piece_size=
         let old_dir=Sys.getcwd() in 
         let ranges = small_pieces (i,j) max_piece_size in 
         let base  = Image.image (
            fun (k,pair)->(k,pair,name_in_extract_page_range pdfname pair)
         )(Ennig.index_everything ranges) in  
         let part1=Image.image (fun (idx,pair,name)->
            bare_extract_page_range pdfname pair name
         ) base
         and part2=Image.image(
            fun (idx,pair,name)->
            "mv "^name^" part"^(string_of_int idx)^".pdf"
         ) base in 
        
        [
         Unix_command.cd (!workspace_directory)
        ]@ 
           part1@part2 
         @[
          Unix_command.cd old_dir;
         ];;

       let rename old_pdfname new_pdfname=
         let old_dir=Sys.getcwd() in 
       [
         Unix_command.cd (!workspace_directory);
         "mv "^old_pdfname^".pdf "^new_pdfname^".pdf";
         Unix_command.cd old_dir;
       ];;
      
       let upside_down pdfname =  
          let old_dir=Sys.getcwd() in 
       [
          Unix_command.cd (!workspace_directory);
          cpdf^" -rotate-contents 180 "^pdfname^".pdf -o wghartnjklmiopfwhhokuuu.pdf";
          "mv wghartnjklmiopfwhhokuuu.pdf "^pdfname^".pdf" ;
          Unix_command.cd old_dir;
        ];;

      let lay_down pdfname =  
          let old_dir=Sys.getcwd() in 
       [
          Unix_command.cd (!workspace_directory);
          cpdf^" -rotate-contents 90 "^pdfname^".pdf -o wghartnjklmiopfwhhokuuu.pdf";
          "mv wghartnjklmiopfwhhokuuu.pdf "^pdfname^".pdf" ;
          Unix_command.cd old_dir;
        ];;
  

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

let finish_recto_verso pdfname =Image.image Unix_command.uc 
  (Command.finish_recto_verso pdfname );;

let implode (pdf_name_start,pdf_name_end)=
   Image.image Unix_command.uc 
  (Command.implode  (pdf_name_start,pdf_name_end));; 

let insert_in_just_after ~inserted_one ~receiving_one ~page_number ~initial_total_length=Image.image Unix_command.uc 
 (Command.insert_in_just_after inserted_one receiving_one page_number initial_total_length);;

let lay_down  pdfname=Image.image Unix_command.uc 
  (Command.lay_down  pdfname);;

let merge parts whole=
  Image.image Unix_command.uc 
  (Command.merge parts whole);; 

let prepare_recto_verso pdfname (i,j)=Image.image Unix_command.uc 
  (Command.prepare_recto_verso pdfname (i,j));;

let replace_page_number_in_by ~page_number ~receiving_one ~inserted_one  ~total_length=Image.image Unix_command.uc 
   (Command.replace_page_number_in_by page_number receiving_one inserted_one  total_length );;

let rename  old_pdfname new_pdfname=
   Image.image Unix_command.uc 
  (Command.rename  old_pdfname new_pdfname);; 

let upside_down  pdfname=Image.image Unix_command.uc 
  (Command.upside_down  pdfname);;
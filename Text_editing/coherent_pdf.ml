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
   cpdf^pdfname^".pdf "^si^"-"^sj^" -o "^output_name;
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

   let explode pdfname num_of_pages=
    let old_dir=Sys.getcwd() in 
    let temp1=Ennig.doyle(
       fun i->
       let si=string_of_int i in 
       cpdf^pdfname^".pdf "^si^"-"^si^" "^pdfname^si^".pdf";
    ) 1 num_of_pages in 
   [
      Unix_command.cd (!workspace_directory)
   ]@   
     temp1
   @[   
     Unix_command.cd old_dir;
   ];;

   let implode pdfname=
      let old_dir=Sys.getcwd() in 
      let temp1=More_unix.quick_beheaded_complete_ls (!workspace_directory) in 
      let temp2=List.filter(
          fun fn->
            (Supstring.begins_with fn pdfname)&&
            (Supstring.ends_with fn ".pdf")
      ) temp1 in 
      let temp3=Option.filter_and_unpack (
         fun fn->
           let temp3=Cull_string.two_sided_cutting (pdfname,".pdf") fn in 
           try (fun i->Some(i,fn))(int_of_string temp3) with 
           _->None
      ) temp2 in 
      let temp4=Ordered.forget_order (Tidel2.diforchan temp3) in 
      let all_pages=String.concat " " (Image.image snd temp4) in 
      let main=cpdf^all_pages^" -o "^pdfname^".pdf" in 
      [
         Unix_command.cd (!workspace_directory);
         main; 
         Unix_command.cd old_dir;
      ];;

   let implode_and_rename pdfname renamer=
      let old_dir=Sys.getcwd() in 
      let temp1=More_unix.quick_beheaded_complete_ls (!workspace_directory) in 
      let temp2=List.filter(
          fun fn->
            (Supstring.begins_with fn pdfname)&&
            (Supstring.ends_with fn ".pdf")
      ) temp1 in 
      let temp3=Option.filter_and_unpack (
         fun fn->
           let temp3=Cull_string.two_sided_cutting (pdfname,".pdf") fn in 
           try (fun i->Some(i,fn))(int_of_string temp3) with 
           _->None
      ) temp2 in 
      let temp4=Ordered.forget_order (Tidel2.diforchan temp3) in 
      let all_pages=String.concat " " (Image.image snd temp4) in 
      let implosion_part=cpdf^all_pages^" -o "^pdfname^".pdf" 
      and renaming_part=Image.image (
         fun (i,fn)->"mv "^fn^" "^(renamer i)^".pdf"  
      ) temp4 in 
      [
         Unix_command.cd (!workspace_directory);
         implosion_part; 
      ]@
         renaming_part  
      @[   
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
      [
          cpdf^file1^".pdf "^file2^".pdf -o wghartnjklmiopfwhhokuuu.pdf";
          "mv wghartnjklmiopfwhhokuuu.pdf "^file1^".pdf" 
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
            bare_extract_page_range pdfname (i,j) name
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
         ];;;;

end;;

let append_on_the_right file1 file2 = Image.image Unix_command.uc 
  (Command.append_on_the_right file1 file2);;
 
let cut_in_two ~pdfname ~first_half_length ~total_length =
  Image.image Unix_command.uc 
  (Command.cut_in_two pdfname first_half_length total_length);;

let extract_page_range pdfname (i,j)=Image.image Unix_command.uc 
  (Command.extract_page_range pdfname (i,j));;

let extract_even_pages  pdfname=Image.image Unix_command.uc 
  (Command.extract_even_pages  pdfname);;

let extract_odd_pages  pdfname=Image.image Unix_command.uc 
  (Command.extract_odd_pages  pdfname);;

let explode pdfname num_of_pages=
   Image.image Unix_command.uc 
  (Command.explode  pdfname num_of_pages);; 

let implode pdfname=
   Image.image Unix_command.uc 
  (Command.implode  pdfname);; 

let implode_and_rename pdfname renamer=
   Image.image Unix_command.uc 
  (Command.implode_and_rename  pdfname renamer);; 

let merge parts whole=
  Image.image Unix_command.uc 
  (Command.merge parts whole);; 

let prepare_recto_verso pdfname (i,j)=Image.image Unix_command.uc 
  (Command.prepare_recto_verso pdfname (i,j));;

let finish_recto_verso pdfname =Image.image Unix_command.uc 
  (Command.finish_recto_verso pdfname );;

let cut_into_small_pieces pdfname (i,j) max_piece_size=
  Image.image Unix_command.uc 
  (Command.cut_into_small_pieces pdfname (i,j) max_piece_size);; 
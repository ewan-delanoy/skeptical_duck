(*

#use "Text_editing/coherent_pdf.ml";;

Utility to cut a large PDF into easily printable chunks.
Each chunk is at most 20 pages long, and separated into
odd/even parts to allow for recto-verso printing. 

*)


let workspace_directory=ref("");;

module Command = struct 
  
let cpdf = "/Applications/cpdf ";;

let internal_extract_page_range pdfname (i,j) output_name=
  let si=string_of_int i and sj=string_of_int j in
  let old_dir=Sys.getcwd() in 
  [
     Unix_command.cd (!workspace_directory);
     cpdf^pdfname^".pdf "^si^"-"^sj^" -o "^output_name;
     Unix_command.cd old_dir;
  ];;

let extract_page_range pdfname (i,j)=
  let si=string_of_int i and sj=string_of_int j in
  let output_name = pdfname^"_from_"^si^"_to_"^sj^".pdf" in 
  internal_extract_page_range pdfname (i,j) output_name;;

let extract_even_pages pdfname=
  let old_dir=Sys.getcwd() in 
  [
     Unix_command.cd (!workspace_directory);
     cpdf^pdfname^".pdf even "^pdfname^"_even.pdf";
     Unix_command.cd old_dir;
  ];;

   let extract_odd_pages pdfname=
   let old_dir=Sys.getcwd() in 
  [
     Unix_command.cd (!workspace_directory);
     cpdf^pdfname^".pdf odd "^pdfname^"_odd.pdf";
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

    let prepare_recto_verso pdfname (i,j)=
        let si=string_of_int i and sj=string_of_int j in
        let excerpt_name = pdfname^"_from_"^si^"_to_"^sj  in 
        let excerpt = excerpt_name^".pdf" in 
        let even_pages = excerpt_name^"_even.pdf"  
        and odd_pages = excerpt_name^"_odd.pdf" in 
        (extract_page_range pdfname (i,j))
        @
        (extract_even_pages excerpt)
        @
        (extract_odd_pages excerpt)
        @
        [
          "open -a /Applications/Preview.app "^odd_pages;
          "open -a /Applications/Preview.app "^even_pages;
        ];;

    let append_on_the_right file1 file2 =
      [
          cpdf^file1^" "^file2^" -o wghartnjklmiopfwhhokuuu.pdf";
          "mv wghartnjklmiopfwhhokuuu.pdf "^file1 
        ];;

     let cut_in_two ~pdfname ~first_half_length ~total_length =
        let half1=pdfname^"_half1.pdf"
        and half2=pdfname^"_half2.pdf" in 
        (internal_extract_page_range pdfname (1,first_half_length) half1)
        @
        (internal_extract_page_range pdfname (first_half_length+1,total_length) half2);;



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

let prepare_recto_verso pdfname (i,j)=Image.image Unix_command.uc 
  (Command.prepare_recto_verso pdfname (i,j));;




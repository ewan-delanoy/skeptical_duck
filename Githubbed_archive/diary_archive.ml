(************************************************************************************************************************
Snippet 95 : 
************************************************************************************************************************)


(************************************************************************************************************************
Snippet 94 : Code to OCR-size PDF's into .html  (see also 91 for .txt instead of html)
************************************************************************************************************************)

open Needed_values ;; 

let lag = (0) ;;
let num_of_pages = 15 ;;
let dirname = "Building_site/";;
let first_treated_page = 1 ;;

let bare_filename = "brit.pdf"
let write1 k =
   let sk = string_of_int k 
   and sj = string_of_int (k+lag) 
   and sn = string_of_int num_of_pages in 
   "pdftoppm "^bare_filename^" p"^sk^" -png -f "^sj^" -singlefile\n"^
   "tesseract -l eng p"^sk^".png p"^sk^"\n"^
   "mv p"^sk^".txt /media/sf_Downloads/"^dirname^" \n"^
   "echo "^sk^" of "^sn;;


let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let last_treated_page = (first_treated_page-1) + num_of_pages ;;

let text1 = "\n\n\n"^(String.concat "\n" 
 (Int_range.scale write1 first_treated_page last_treated_page))^"\n\n\n" ;;   
   
Io.overwrite_with ap1 text1;;

let partial_texts_for_html = Int_range.scale (fun k->
   let sk = string_of_int k in 
   let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  let uncompressed_pagetext = rf fn in 
  let pagetext = Make_paragraphs_one_lined.in_string 
  (Remove_hyphens.in_string uncompressed_pagetext) in  
  pagetext)  first_treated_page last_treated_page ;;
 
 
 let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"full.html");;  
 
let html_beginning = String.concat "\n"
 ["<!DOCTYPE html>"; "<html>"; "<head>";
 "\t<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">";
 "\t<title> Title  </title>"; "</head>";
 "<body background=\"bg03.gif\">"; "<center>";
 "\t<h1> TITLE </h1>"; "</center>"] ;;

let html_ending = String.concat "\n"
 ["</body>"; "</html>"] ;;

 let html_full_text = 
   String.concat "\n"
   [html_beginning;Htmlize.pages partial_texts_for_html;html_ending] ;;
 
  Io.overwrite_with full_ap html_full_text;;


(************************************************************************************************************************
Snippet 93 : Code using the Parse_js module 
************************************************************************************************************************)


(*

let ap1 = Absolute_path.of_string 
 (home^"/Teuliou/Sites/Gwerzher_Leoriou/node_modules/async/lib/async.js") ;;
let text1 = Io.read_whole_file ap1 ;; 

let g1 = Parse_js.tokens text1 ;;
let res1 = Parse_js.parse_string text1 ;;
let res2 = Parse_js.parse_program text1 ;;
let res3 = Parse_js.program_of_string text1 ;;

*)


(************************************************************************************************************************
Snippet 92 : Absorbing code from Y. Padioleau's codebase
************************************************************************************************************************)

let (root,backup_dir,githubbing)=Coma_big_constant.Third_World.triple ;;
let fw_config = Fw_configuration.of_root root ;;
let github_config = Fw_poly.construct_github_configuration 
  ~root:root
  ~dir_for_backup:backup_dir
  ~gitpush_after_backup:githubbing
  ~github_url:Coma_big_constant.github_url
  ~encoding_protected_files:[] ;;
let cs_ref=ref(Fw_with_githubbing.plunge_fw_config_with_github_config  fw_config github_config);;
let s_root = Dfa_root.connectable_to_subpath root ;;
let s_above_root = Cull_string.before_rightmost (Dfa_root.without_trailing_slash root) '/';;

let a1 =(More_unix.create_subdirs_and_fill_files_if_necessary root
       Coma_constant.minimal_set_of_needed_dirs 
           Coma_constant.conventional_files_with_minimal_content) ;;

let a2 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "common.ml";;
let a3 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "common2.ml";;
(* let a4 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "ocaml.ml";; *)
let a5 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "parse_info.ml";;
let a6 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "flag_parsing_js.ml";;
let a7 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "ast_js.ml";;
let a8 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "parser_js.mly";;
let a9 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "lexer_js.mll";;
(* let a10 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "visitor_js.ml";; *)
let a11 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "token_helpers_js.ml";;
let a12 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "parsing_hacks_js.ml";;
let a13 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "parse_js.ml";;
let a14 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "tools_for_absolute_path.ml";;
let a15 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "absolute_path.ml";;
let a16 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "io.ml";;

let raco () = Modify_coma_state.Reference.recompile cs_ref (Some "aaa");;

let v1 = Fw_with_dependencies.list_values_from_module (!cs_ref) "common" ;;

Needed_values.vfm ;;

let z1 = Fw_with_dependencies.all_endinglesses (!cs_ref) ;;

let fg1 = Modify_coma_state.Syntactic_sugar.forget cs_ref ["common2"] ;;

let current_module = ref ("parse_info") ;;
let ap1() = Absolute_path.of_string ("../Cherokee/old_"^(!current_module)^".ml") ;;
let ap2() = Absolute_path.of_string ("../Cherokee/"^(!current_module)^".ml") ;;

let ci i j= Lines_in_string.copy_interval_from_file_to_file (i,j) (ap1()) (ap2()) ;;
let ri i j = Lines_in_string.remove_interval_in_file (ap2()) i j ;;

let act1 () = Replace_inside.replace_several_inside_file 
 [("PI.","Parse_info.")] (ap2()) ;;




let r1 = [("T.","Parser_js.");("TH.","Token_helpers_js.");("Ast.","Ast_js.")]


let (a,b,c) = Lines_in_string.tripartition_associated_to_interval "" 79 123 ;;
let new_b = Replace_inside.replace_inside_string ("-> T","-> Parser_js.T") b ;;
let text2 = String.concat "\n" [a;new_b;c] ;;



(************************************************************************************************************************
Snippet 91 : Code to OCR-size PDF's into .txt 
************************************************************************************************************************)

open Needed_values ;;

let lag = (0) ;;
let num_of_pages = 12 ;;
let dirname = "Building_site/";;
let first_treated_page = 1 ;;

let bare_filename = "brit.pdf"
let write1 k =
   let sk = string_of_int k 
   and sj = string_of_int (k+lag) in 
   "pdftoppm "^bare_filename^" p"^sk^" -png -f "^sj^" -singlefile\n"^
   "tesseract -l eng p"^sk^".png p"^sk^"\n"^
   "mv p"^sk^".txt /media/sf_Downloads/"^dirname^" \n"^
   "echo "^sk;;


let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let last_treated_page = (first_treated_page-1) + num_of_pages ;;

let text1 = "\n\n\n"^(String.concat "\n" 
 (Int_range.scale write1 first_treated_page last_treated_page))^"\n\n\n" ;;   
   
Io.overwrite_with ap1 text1;;

let partial_texts_for_txt = Int_range.scale (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  let announcer = "%\n% Page "^sk^" \n%\n" in 
 let uncompressed_pagetext = rf fn in 
 let pagetext = Make_paragraphs_one_lined.in_string 
 (Remove_hyphens.in_string uncompressed_pagetext) in  
 announcer^pagetext)  first_treated_page last_treated_page ;;


let partial_texts_for_html = Int_range.scale (fun k->
   let sk = string_of_int k in 
   let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  let uncompressed_pagetext = rf fn in 
  let pagetext = Make_paragraphs_one_lined.in_string 
  (Remove_hyphens.in_string uncompressed_pagetext) in  
  pagetext)  first_treated_page last_treated_page ;;
 
 
 let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"full.txt");;  
 
 let txt_full_text = String.concat "\n" partial_texts_for_txt ;;
 let html_full_text = Htmlize.pages partial_texts_for_html ;;
 
 Io.overwrite_with full_ap txt_full_text;;
 Io.overwrite_with full_ap html_full_text;;



(************************************************************************************************************************
Snippet 90 : Musing on Egyptian fractions
************************************************************************************************************************)


let rec next_gcd_correct_index (l,walker) = 
  if List.for_all (fun t->(Gcd.gcd t walker) =1) l
  then walker
  else next_gcd_correct_index (l,walker+1) ;;  

let reduce_fraction old_a old_b =
  if old_a = 0 
  then (0,1) 
  else let g = Gcd.gcd old_a old_b in 
       (old_a/g,old_b/g) ;; 

let next_state (l,a,b,walker) = 
  (* it is assume a /b >= 1/walker *)
  let i1 = next_gcd_correct_index (l,walker) in 
  let (small_a,small_b)=reduce_fraction (a*i1-b) (b*i1) in 
  (i1::l,small_a,small_b,Basic.frac_ceiling small_b small_a) ;;

let rec finish_solving old_uple =
 let (l,a,b,walker) = old_uple in 
 if a = 1 
 then (List.rev l,b)
 else 
 finish_solving (next_state old_uple) ;;

let solve a b =   finish_solving([],a,b,Basic.frac_ceiling b a) ;;

let v0 = ([],3,7,Basic.frac_ceiling 7 3) ;;
let ff = Memoized.small next_state v0 ;;



(************************************************************************************************************************
Snippet 89 : Linear algebra on variables indexed by Z^2
************************************************************************************************************************)

let w1 n =Int_range.scale (fun y->(n,y-1)) 1 (n+1) ;;
let w2 n =Int_range.scale (fun x->(n-x,n)) 1 (2*n) ;;
let w3 n =Int_range.scale (fun y->(-n,n-y)) 1 (2*n) ;;
let w4 n =Int_range.scale (fun x->(-n+x,-n)) 1 (2*n) ;;
let w5 n =Int_range.scale (fun y->(n,-n+y)) 1 (n-1) ;;
let ww n =
    if n = 0 then [0,0] else 
    List.flatten (Image.image (fun w->w n) [w1;w2;w3;w4;w5]) ;;  

let small_size = 2 ;;    
let base1 = List.flatten (Int_range.scale ww 0 (2*small_size));;
let b_index pair = Listennou.find_index pair base1  ;;

let for_two =((fun pair1 pair2 ->
  Total_ordering.for_integers 
  (b_index pair1) (b_index pair2) ): (int *int) Total_ordering_t.t);;

let for_ttwo = Total_ordering.product for_two for_two ;;

let for_four =((fun 
   (p1,p2)
   (q1,q2)
   -> 
    let mp = max (b_index p1) (b_index p2) 
    and mq = max (b_index q1) (b_index q2)  in
    let trial1 = Total_ordering.standard mp mq in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else 
      for_ttwo  (p1,p2) (q1,q2)
): ((int *int)*(int *int)) Total_ordering_t.t ) ;;

let m_index l = Max.list (Image.image b_index l) ;; 

let for_list =((fun 
   l1 l2
   -> 
    let trial1 = Total_ordering.standard (m_index l1) (m_index l2) in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else 
      Total_ordering.standard l1 l2
): ((int *int) list) Total_ordering_t.t ) ;;


let unchecked_base2= Cartesian.square(Cartesian.square(Int_range.range (-small_size) small_size)) ;;
let check_pair (c,d) = if c=0 then d>0 else c>0 ;;
let check_double_pair ((a,b),(c,d)) =
  ((a,b)<>(0,0)) && (check_pair(c,d)) ;;
let base2 = List.filter check_double_pair unchecked_base2 ;;
let base3 = Image.image (fun ((a,b),(c,d))->
  [(a+c,b+d);(a-c,b-d);(a,b);(c,d)]
  ) base2 ;;
let base4 = Ordered.sort for_list base3 ;;
let standardize_pair (x,y)= if x<0 then (-x,-y) else (x,y) ;;
let mima (a,b) = if (for_two a b)=Total_ordering_result_t.Lower then (b,a) else (a,b) ;;
let standardize_list l=
   let temp1 = Image.image standardize_pair l in 
   let tempf = (fun k->List.nth temp1 (k-1)) in 
   (mima (tempf 1,tempf 2),mima (tempf 3,tempf 4)) ;;
let base5 = Image.image standardize_list base4 ;;
let base6 = Listennou.nonredundant_version base5 ;;

let ff k = List.nth base6 (k-1) ;;


(************************************************************************************************************************
Snippet 88 : Short code related to similar matrices exercise
************************************************************************************************************************)

let test1 n = List.exists (fun a->((a*(4-a)+1) mod n)= 0) (Int_range.range 0 (n-1)) ;;
    
let v1 = List.filter test1 (Int_range.range 3 50) ;;

let see1 n = List.filter (fun a->((a*(4-a)+1) mod n)= 0) (Int_range.range 0 (n-1)) ;;

let u1 = Cartesian.square (Int_range.range (-50) 50) ;;
let sign_condition (x,y) =
    if x<0 then false else 
    if x=0 then y>0 else true ;;
let u2 = List.filter (fun (x,y)->(sign_condition (x,y))&&(Gcd.gcd x y=1)) u1 ;;
let u3 = Image.image (fun (x,y)->(max (abs x) (abs y),(x,y)) ) u2 ;;
let (Set_of_poly_pairs_t.S u4) = Set_of_poly_pairs.sort u3 ;;
let u5 = Image.image snd u4 ;;

let tf1 (a,b) = Listennou.force_find (fun (z2,z4)->
  abs((a*a-4*a-1)*(z2*z2)+2*(a-2)*b*z2*z4+(b*b)*(z4*z4))<b*b 
  ) u5 ;;

let current_b = 29 ;;  
let abs_b = abs current_b ;;
let good_moduli = see1 abs_b ;;
let ff a = tf1(a,current_b) ;;

(************************************************************************************************************************
Snippet 87 : Exercise related to Cantor set 
************************************************************************************************************************)


let read_fraction s = 
  let (a,b) = Cull_string.split_wrt_rightmost s '/' in 
  (int_of_string a,int_of_string b) ;;

let interval_size_is_smaller_than frac1 frac2 (p,q) =
  let (a1,b1) = read_fraction frac1 
  and (a2,b2) = read_fraction frac2 in 
  q * (a2*b1-a1*b2) < p * (b1*b2)  ;;

let interval_size_is_larger_than frac1 frac2 (p,q) =
   let (a1,b1) = read_fraction frac1 
   and (a2,b2) = read_fraction frac2 in 
   q * (a2*b1-a1*b2) > p * (b1*b2)  ;;

let minmax_using_tor total_ordering x y =
if (Listennou.find_index x total_ordering) < (Listennou.find_index y total_ordering) 
then (x,y)
else (y,x) ;;  

let possibly_empty_interval_using_tor total_ordering a b =
  if minmax_using_tor total_ordering a b = (a,b)
  then Some(a,b)
  else None ;;  

let interval_intersection_using_tor total_ordering (a1,b1) (a2,b2) =
  let (_,a3) = minmax_using_tor total_ordering  a1 a2 
  and (b3,_) = minmax_using_tor total_ordering  b1 b2 in 
  possibly_empty_interval_using_tor total_ordering  a3 b3;; 


let current_k = 3 ;; 

let outer_interval_is_too_small  total_ordering =
 let n = List.length total_ordering in 
 let ia = Listennou.find_index "a" total_ordering 
 and ib = Listennou.find_index "b" total_ordering in 
 if List.mem (ia,ib) [1,2;n-1,n] then true else
 if (ia<2)||(ib>=n) then false else 
 let just_below_a = List.nth total_ordering (ia-2) 
 and just_above_b = List.nth total_ordering ib in 
 interval_size_is_smaller_than just_below_a just_above_b (4,Basic.power 3 current_k) ;; 

let inner_interval_is_too_large  total_ordering =
 let ia = Listennou.find_index "a" total_ordering 
 and ib = Listennou.find_index "b" total_ordering in 
 if ib=ia+1 then false else 
 let just_above_a = List.nth total_ordering ia 
 and just_below_b = List.nth total_ordering (ib-2) in 
 interval_size_is_larger_than just_above_a just_below_b (8,Basic.power 3 current_k) ;; 


let intervals_outside = 
[
  "1/9","2/9";"1/3","2/3";"7/9","8/9"
] ;;

let base1 = List.flatten(Image.image (fun (x,y)->[x;y]) intervals_outside);;

let base2 = Listennou.extend_total_ordering_by_adding_two_elements 
  base1 "a" "b" ;;

let (bad1,good1) = List.partition inner_interval_is_too_large base2 ;; 
let (bad2,good2) = List.partition outer_interval_is_too_small good1 ;; 

let u1 = Image.image (
 fun total_ordering ->
   (Image.image (interval_intersection_using_tor total_ordering ("a","b")) 
   intervals_outside,total_ordering)
) good2 ;;

let u2 = Listennou.partition_according_to_fst u1 ;; 
let tf k = List.nth u2 (k-1) ;; 

(************************************************************************************************************************
Snippet 86 : Enumeration of multi-degrees related to symmetric polynomials
************************************************************************************************************************)


let order_for_triples = ((
  fun (x1,x2,x3) (y1,y2,y3) ->
    let sx = x1+x2+x3
    and sy =y1+y2+y3 in 
    let trial1 = Total_ordering.standard sx sy in 
    if trial1 <> Total_ordering_result_t.Equal then trial1 else   
    let mx = Max.list [x1;x2;x3] 
      and my = Max.list [y1;y2;y3] in 
      let trial2 = Total_ordering.standard mx my in 
      if trial2 <> Total_ordering_result_t.Equal then trial2 else 
      Total_ordering.silex_for_intlists [x1;x2;x3] [y1;y2;y3] 
) :> (int*int*int) Total_ordering_t.t );;

let orbit (x1,x2,x3) = Ordered.sort order_for_triples 
   [ (x1,x2,x3);(x1,x3,x2);(x2,x1,x3);(x2,x3,x1);(x3,x1,x2);(x3,x2,x1); ] ;;

let u1 = Ordered.sort order_for_triples  (Cartesian.cube (Int_range.range 0 8)) ;;   

let u2 = Explicit.image (fun tr->(tr,orbit tr)) u1 ;;
let u3 = List.filter (fun (tr,l)->tr=List.hd l) u2 ;; 
let tf k = List.nth u3 (k-1) ;;


(************************************************************************************************************************
Snippet 85 : Reindex pages of a book for printing
************************************************************************************************************************)

let ap1 = Absolute_path.of_string (Needed_values.home^"/Downloads/Gwenn/");;
let s_ap1 = Absolute_path.to_string ap1 ;;

Coherent_pdf.workspace_directory := s_ap1 ;;
Coherent_pdf.explode ("p","") 248;;

let print_transform n =
    let pre_q=(n/8) in 
    let pre_r= n-8*pre_q in 
    let (q,old_r)=(if pre_r=0 then (pre_q-1,8) else (pre_q,pre_r) ) in 
    let new_r = List.nth [4;1;8;5;2;3;6;7] (old_r-1) in 
    8*q+new_r ;; 

let command_for_index i =
    let j = print_transform i in 
    let si = string_of_int i 
    and sj = string_of_int j in 
    "mv "^s_ap1^"p"^sj^".pdf "^s_ap1^"q"^si^".pdf" ;;

let number_of_chunks = 31 ;;
let reindexing_commands = Int_range.scale command_for_index 1 (8*number_of_chunks) ;;
let act () = Image.image Sys.command reindexing_commands ;;

Coherent_pdf.implode ("q","") ;;

(************************************************************************************************************************
Snippet 84 : Musing on Steinhaus triangles
************************************************************************************************************************)

let i_fold_merge = Ordered.fold_merge Total_ordering.for_integers ;;
let i_sort = Ordered.sort Total_ordering.for_integers ;;
let il_sort = Ordered.sort Total_ordering.silex_for_intlists ;;

let index_from_x unadbridged_x_form =
    let x_form = Cull_string.trim_spaces unadbridged_x_form in 
    if not(Supstring.begins_with x_form "x") 
    then None 
    else
    Some(int_of_string(Cull_string.cobeginning 1 x_form));;     

let indices_from_xlist xlist =
  let parts = Str.split (Str.regexp_string "+") xlist in  
  i_sort(Option.filter_and_unpack index_from_x parts);;

let temporary_store=Absolute_path.of_string (Needed_values.home^"/Downloads/temp.txt") ;;
let transmitter_file = Absolute_path.of_string "Fads/pan.ml";;

let act () = 
  let stored_text = Io.read_whole_file temporary_store in 
  let temp1 = Replace_inside.replace_several_inside_string 
   ["[","";"]","";"(","";")",""] stored_text in
  let xsums = Str.split (Str.regexp_string ",") temp1 in 
  let dim_after = List.length xsums in  
  let indices = i_fold_merge (Image.image indices_from_xlist xsums) in 
  let ocamlese_before = String.concat "," (Image.image ( fun i->
   "x"^(string_of_int i)
  ) indices) in
  let ocamlese_after = String.concat ";" xsums in 
  let ocamlese_uple = String.concat "," (Int_range.scale ( fun i->
  "tf "^(string_of_int i)
  ) 1 (List.length indices))  in 
  let lines_in_preproduced_text =
  [ 
   "let dim_before = "^(string_of_int (List.length indices))^" ;;";
   "let dim_after = "^(string_of_int dim_after)^" ;;";
   "let to_long_list ("^ocamlese_before^") ="; "    Image.image (fun t-> t mod 2)";
   "    [";
   "      "^ocamlese_after;
   "    ] ;;"; "let to_uple l ="; "    let tf = (fun k->List.nth l (k-1)) in ";
   "    ("^ocamlese_uple^") ;;  "; ] in 
  let preproduced_text = "\n\n\n"^(String.concat "\n" lines_in_preproduced_text)^"\n\n\n" in 
  Replace_inside.overwrite_between_markers_inside_file 
      (Overwriter.of_string preproduced_text)
      ("(* Pre-"^"processed part starts here *)","(* Pre-"^"processed part ends here *)")
      transmitter_file ;;


(* Pre-processed part starts here *)


let dim_before = 4 ;;
let dim_after = 36 ;;
let to_long_list (x2,x4,x5,x6) =
    Image.image (fun t-> t mod 2)
    [
      0; x2; x2; 1; x2 + 1; 1; x4; x4 + 1; x2 + x4; x2 + x4 + 1; x5; x4 + x5; x5 + 1; x2 + x4 + x5 + 1; x5; x6; x5 + x6; x4 + x6; x4 + x5 + x6 + 1; x2 + x6; x2 + x5 + x6; x2 + x4 + x5 + x6 + 1; x2 + x4 + x5 + 1; x2 + x4 + x6 + 1; x2 + 1; x2 + x4 + x5 + x6; x4 + x5; x2 + x4 + x6; x5 + 1; x2 + x4 + x6; x5 + x6 + 1; x2 + x4 + x5; x4 + x5 + 1; x2 + x6 + 1; x2 + x4 + x5 + x6 + 1; x5 + 1
    ] ;;
let to_uple l =
    let tf = (fun k->List.nth l (k-1)) in 
    (tf 1,tf 2,tf 3,tf 4) ;;  


(* Pre-processed part ends here *)


let test uple =
  let l = to_long_list uple in
  let n = (List.length l)/2 in 
    let l2 = List.filter (fun y->y = 0) l in 
    (List.length l2)=n ;;
  


let test2 l = test (to_uple l) ;;  
  
let base1 = Int_range.scale (fun _->[0;1]) 1 dim_before ;;    
let base2 = Cartesian.general_product base1 ;;
let base3 = List.filter test2 base2 ;; 
let base4 = Image.image (fun u->(u,to_long_list(to_uple u))) base3 ;;

let nonzero_linear_forms = List.tl(Cartesian.product [0;1] base2) ;;
let eval_linear_form (const_part,linear_part) x=
  let temp1 = List.combine (const_part::linear_part) (1::x) in 
  let temp2 = Image.image (fun (a,b)->a*b) temp1 in 
  abs((Basic.fold_sum temp2) mod 2) ;;
let kernel_of_linear_form = Memoized.make(fun lf ->
    List.filter (fun x->eval_linear_form lf x = 0) base3 ) ;;   
let kernel_size = Memoized.make(fun lf->
    List.length(kernel_of_linear_form lf)
  ) ;;
let computation1 = Explicit.image kernel_size nonzero_linear_forms ;;

let res1 = Max.maximize_it_with_care kernel_size nonzero_linear_forms ;;

let defect_at_index =Memoized.make(fun idx -> 
    let (a,b) = List.partition (fun l->List.nth l (idx-1)=0) base3 in 
    (abs(List.length(b)-List.length(a))) );; 

let minimal_defects =
    Min.minimize_it_with_care  defect_at_index 
      (Int_range.range 1 dim_before) ;;   

let big_proj shadow = il_sort(Image.image (fun l->Listennou.project l shadow) base3) ;;  
let shadows = il_sort (Listennou.power_set (Int_range.range 1 dim_before)) ;;     
let (_,shadowers) = Max.maximize_it_with_care (fun sh->List.length(big_proj sh)) shadows ;;




(************************************************************************************************************************
Snippet 83 : Draft to preprocess a file using data from PARI-GP
************************************************************************************************************************)

open Needed_values ;;

let i_fold_merge = Ordered.fold_merge Total_ordering.for_integers ;;
let i_sort = Ordered.sort Total_ordering.for_integers ;;

let index_from_x unadbridged_x_form =
    let x_form = Cull_string.trim_spaces unadbridged_x_form in 
    if not(Supstring.begins_with x_form "x") 
    then None 
    else
    Some(int_of_string(Cull_string.cobeginning 1 x_form));;     

let indices_from_xlist xlist =
  let parts = Str.split (Str.regexp_string "+") xlist in  
  i_sort(Option.filter_and_unpack index_from_x parts);;

let temporary_store=Absolute_path.of_string (home^"/Downloads/temp.txt") ;;
let transmitter_file = Absolute_path.of_string "Fads/pan.ml";;


let stored_text = Io.read_whole_file temporary_store ;;
let temp1 = Replace_inside.replace_several_inside_string 
  ["[","";"]","";"(","";")",""] stored_text ;;
let xsums = Str.split (Str.regexp_string ",") temp1 ;;
let dim_after = List.length xsums ;;
let indices = i_fold_merge (Image.image indices_from_xlist xsums) ;;
let dim_before = List.length indices ;;
let ocamlese_before = String.concat "," (Image.image ( fun i->
   "x"^(string_of_int i)
) indices) ;;
let ocamlese_after = String.concat ";" xsums ;;
let ocamlese_uple = String.concat "," (Image.image ( fun i->
  "tf "^(string_of_int i)
) indices) ;;


let lines_in_preproduced_text =
[ "let max_idx = "^(string_of_int dim_after)^" ;;";
   "let to_long_list ("^ocamlese_before^") ="; "    Image.image (fun t-> t mod 2)";
   "    [";
   "      "^ocamlese_after;
   "    ] ;;"; "let to_uple l ="; "    let tf = (fun k->List.nth l (k-1)) in ";
   "    ("^ocamlese_uple^") ;;  "; ] ;;

let preproduced_text = "\n\n\n"^(String.concat "\n" lines_in_preproduced_text)^"\n\n\n" ;;

let prprpr () = 
    Replace_inside.overwrite_between_markers_inside_file 
      (Overwriter.of_string preproduced_text)
      ("(* Pre-"^"processed part starts here *)","(* Pre-"^"processed part ends here *)")
      transmitter_file ;;


(************************************************************************************************************************
Snippet 82 : Lower bounds on linear recurrent sequences of order 2
************************************************************************************************************************)

let nachste (x,y) = (y,4*y-5*x) ;;

let nachstee (x,y,l) = (y,4*y-5*x,x::l) ;; 

let precision = ref 1000 ;;

let measure (x,y) =
    let opt_example = ref None 
    and walker = ref (x,y) 
    and bound=abs(x) in 
    for k = 1 to (!precision) 
    do 
       walker := (nachste(!walker));
       if abs(fst(!walker)) <= bound 
       then opt_example := Some k 
    done;
    match (!opt_example) with 
    None -> 2
   |Some kmax -> (kmax)+1 ;;  

let see_measure (x,y) = 
    let n = measure(x,y) 
    and walker = ref (x,y,[]) in
    for k = 3 to n 
    do 
        walker := nachstee (!walker)
    done;
    let (a,b,l)=(!walker) in 
    List.rev(b::a::l) ;;
    
let base1 =Memoized.make(fun n -> List.filter (fun (x,y)->
    let mx = abs x and my =abs y in 
    ((x,y)<>(0,-1)) &&
    (x>=0)&&((Gcd.gcd x y)=1) && (mx<=my) && (max mx my=n)
  ) (Cartesian.square (Int_range.range (-n) n))) ;;

let base_image1 = Memoized.make (fun n->
    Max.maximize_it_with_care measure 
    (base1 n)
) ;;

let rec breaker_tester bound candidate =
  if fst(base_image1 candidate) >= bound 
  then  candidate 
  else  breaker_tester bound (candidate+1) ;;

let next_breaker = Memoized.recursive (fun old_f bound -> 
     if bound<=3 then (1,(3,[1; 1; -1],[])) else 
     let m = breaker_tester bound (fst(old_f(bound-1))) in 
     let (_,l) = base_image1 m in 
     let (a,others) = Listennou.ht l in 
     let sol = see_measure a in 
     (m,(List.length sol,sol,others))
  ) ;;


let bi = base_image1 ;;
let sm = see_measure ;;
let nb = next_breaker ;;

(************************************************************************************************************************
Snippet 81 : Debugging compiling of mll and mly files
************************************************************************************************************************)

let (root,backup_dir,githubbing)=Coma_big_constant.Third_World.triple ;;
let fw_config = Fw_configuration.of_root root ;;
let github_config = Fw_poly.construct_github_configuration 
  ~root:root
  ~dir_for_backup:backup_dir
  ~gitpush_after_backup:githubbing
  ~github_url:Coma_big_constant.github_url
  ~encoding_protected_files:[] ;;
let cs_ref=ref(Fw_with_githubbing.plunge_fw_config_with_github_config  fw_config github_config);;
let s_root = Dfa_root.connectable_to_subpath root ;;
let s_above_root = Cull_string.before_rightmost (Dfa_root.without_trailing_slash root) '/';;

let a1 =(More_unix.create_subdirs_and_fill_files_if_necessary root
       Coma_constant.minimal_set_of_needed_dirs 
           Coma_constant.conventional_files_with_minimal_content) ;;



let a1 =(More_unix.create_subdirs_and_fill_files_if_necessary root
       Coma_constant.minimal_set_of_needed_dirs 
           Coma_constant.conventional_files_with_minimal_content) ;;

let a2 = Modify_coma_state.Syntactic_sugar.register_one cs_ref "cherokee_token.ml";;

let lines = ["cherokee_lexer.mll"];;

let bad1 () = Modify_coma_state.Syntactic_sugar.register_several cs_ref lines ;;

let rootless_paths = Image.image Dfn_rootless.of_line lines ;;

let bad2 () = Modify_coma_state.Reference.register_rootless_paths cs_ref rootless_paths ;;

let cs = (!cs_ref) ;;

let bad3 () = Modify_coma_state.And_save.register_rootless_paths cs rootless_paths ;;

let bad4 () = Fw_with_githubbing.register_rootless_paths cs rootless_paths ;;

let fw_with_bc = Fw_poly.parent cs ;;

let bad5 () = Fw_with_batch_compilation.register_rootless_paths fw_with_bc rootless_paths ;;

let old_fw_with_deps = Fw_poly.parent  fw_with_bc ;;

let (new_fw_with_deps,((ac_paths,uc_paths,nc_paths),_))=
       Fw_with_dependencies.register_rootless_paths old_fw_with_deps rootless_paths ;;

module BCPri = Fw_with_batch_compilation.Private ;;       

let old_list_of_cmpl_results = BCPri.get_cmpl_results fw_with_bc ;; 

let new_list_of_cmpl_results = Image.image (
        fun mn -> 
          match List.assoc_opt mn old_list_of_cmpl_results with 
          None -> (mn,false)
          |Some(old_res) -> (mn,old_res)
     ) (Fw_with_dependencies.dep_ordered_modules new_fw_with_deps) ;; 

let fw_with_bc2 = BCPri.usual_extension new_fw_with_deps new_list_of_cmpl_results ;;    

let unordered_mods = Image.image Dfn_rootless.to_module uc_paths ;;

let bad6 () = BCPri.modern_recompile fw_with_bc2 unordered_mods;;

module BCOtm = BCPri.Ocaml_target_making ;;

let (all_deps,new_deps,changed_modules) = 
        Fw_with_dependencies.below_several new_fw_with_deps unordered_mods ;;
let bad7 ()=
        BCOtm.usual_feydeau fw_with_bc2 all_deps ;;

let cmod = Compilation_mode_t.Usual ;;

let (opt_modnames,opt_rootless_path)= (Some(all_deps),None);;

let bad8 ()=
   BCOtm.feydeau cmod fw_with_bc2 (opt_modnames,opt_rootless_path);;

let bad9 ()=
   BCOtm.shaft_part_of_feydeau cmod fw_with_bc2 (opt_modnames,opt_rootless_path);;   

let bad10 () =
   BCOtm.list_of_commands_for_shaft_part_of_feydeau cmod fw_with_bc2 (opt_modnames,opt_rootless_path) ;;

let l=BCOtm.dependencies_inside_shaft cmod fw_with_bc2 (opt_modnames,opt_rootless_path) ;;

let mn0 = List.hd l ;;

let eless0=Fw_with_dependencies.endingless_at_module fw_with_bc2 mn0 ;;

module BCCmd = BCPri.Command ;;

let bad11 () = BCCmd.module_separate_compilation cmod fw_with_bc2 eless0 ;;

let bad12 () = Commands_for_batch_compilation.module_separate_compilation cmod new_fw_with_deps eless0 ;;

let check = 
   Commands_for_batch_compilation.Private.command_for_cmo_from_mll cmod root fw_with_bc2 eless0;;


(*

let a0 = 
   let i1 = Sys.command ("rm -rf "^s_root^"*") in
   let i2 = Sys.command ("cp "^s_above_root^"/Wyoming/* "^s_root) in 
   (i1,i2) ;; 

#use"Fads/cloth.ml";;   



*)

(************************************************************************************************************************
Snippet 80 : Duplicating a paragraph in a file 
************************************************************************************************************************)


let ap1 = Absolute_path.of_string  
   "Compilation_management/commands_for_batch_compilation.ml" ;;
let text1 = Io.read_whole_file ap1 ;;

let (a,b,c) = Lines_in_string.tripartition_associated_to_interval text1 75 97 ;;

let text2 = String.concat "\n\n" [a;b;b;c] ;;

Io.overwrite_with ap1 text2 ;;


(************************************************************************************************************************
Snippet 79 : Write mathjax text for answer on chain additions
************************************************************************************************************************)

open Needed_values ;;

let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;

let i_is_included_in = Ordered.is_included_in i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_sort = Ordered.sort i_order ;;

let il_fold_merge = Ordered.fold_merge il_order ;;
let il_sort = Ordered.sort il_order ;;

let decompositions n = Int_range.scale (fun j->(j,n-j)) 1 (n/2) ;;

let try_easier_path old_f l = 
    if List.length (l) < 2 then None else 
    let (a,others) = Listennou.ht l in   
       let candidate = List.filter (List.mem a) (old_f others ) in 
       if candidate = [] then None else Some candidate ;;

let abauzit_order = 
    let tempf=(
          fun l1 l2->
           let t=Total_ordering.standard(List.length l1)(List.length l2) in
           if t<>Total_ordering_result_t.Equal then t else
           il_order (List.rev l1) (List.rev l2)
    ) in
     (tempf:>( (int list) Total_ordering_t.t));;
      
let abauzit_expansion l = 
    let (n,temp1) = Listennou.ht (List.rev l) in 
    let temp2 = List.rev temp1 
    and decs = decompositions n in 
    let temp3 = Image.image (fun (a,b)->i_merge (i_sort [a;b]) temp2) decs in   
    Ordered.sort abauzit_order temp3 ;;

let main = Memoized.recursive (fun old_f l->
   if l=[1] then [[1]] else
   match try_easier_path old_f l with 
    Some(easier_answer) -> easier_answer 
   | None ->    
     let n = List.hd(List.rev l) in 
     let temp1 = abauzit_expansion l in 
     let temp2 = Image.image old_f temp1 in 
     let (m,temp3) = Min.minimize_it_with_care (fun res->List.length(List.hd res)) temp2 in
     let temp4 = il_fold_merge temp3 in 
     Image.image (fun sol->sol@[n]) temp4
) ;;

let measure = Memoized.make(fun l->
    let res = main l in List.length(List.hd res)
) ;;

let adrien_analysis (l,bound) = 
    Option.seek (fun (h,l2)->
      (measure [h]) + (List.length l2) >= bound  
    )  (Three_parts.beheaded_tails l) ;;
    
let singleton_analysis n =
  let temp1 = decompositions n in 
  let temp2 = Image.image (fun (a,b)->(a,b,measure [a],measure [b])) temp1 in 
  let (almost_m,temp3) = Min.minimize_it_with_care (fun (a,b,ma,mb)->max ma mb) temp2 in 
  (measure [n])=(almost_m+1)  ;;    

let singleton_test l = 
    let n = List.hd(List.rev l) in 
    if l <> [n]
    then false 
    else  singleton_analysis n ;;    

let hashtbl_for_impatient_main = Hashtbl.create 100 ;;     
    
let partial_analysis_without_writing l =
    let temp1 = abauzit_expansion l 
     and bound = (measure l)-1 in 
    let temp2 = Image.image (fun l2-> (l2,adrien_analysis (l2,bound)) ) temp1 in 
    let (good_temp2,bad_temp2) = List.partition (fun (l2,opt)->opt<>None) temp2 in 
    let temp3 = Image.image (fun (l2,opt)->(l2,Option.unpack opt)) good_temp2 in 
    let temp4 = Option.filter_and_unpack (
      fun (l2,_) -> 
        if Hashtbl.find_opt hashtbl_for_impatient_main l2 = None 
        then Some l2 
        else None 
    ) bad_temp2 in 
    (singleton_test l,temp3,temp4,List.hd(main l)) ;; 
    
let soi = string_of_int ;; 
let soil l = "["^(String.concat ";" (Image.image soi l))^"]";;

let pointed_card_element_to_string (x,(head,passive_part)) =
   "("^(soil x)^",("^(soi head)^","^(soil passive_part)^"))";;

let pointed_card_to_string l = 
   "["^(String.concat ";" (Image.image pointed_card_element_to_string l))^"]";;

let write_about_singleton n solution =
   "simplest_case "^(soi n)^" "^(soil solution)^" ;;";;

let write_about_missing_link missing_link =
  "pa "^(soil missing_link)^" ;;";;

  let write_about_missing_links missing_links =
    String.concat "\n" (Image.image write_about_missing_link missing_links);;

let write_about_pointed_card l pointed_card solution  = 
  "pointed_card "^(soil l)^" "^
  (pointed_card_to_string pointed_card)^" "^(soil solution)^" ;;";;
  ;;

let write_about_partial_analysis l (is_singleton,pointed_card,missing_links,solution) =
  let n = List.hd(List.rev solution) in 
   if is_singleton 
   then write_about_singleton n solution 
   else
   if missing_links <> []
   then write_about_missing_links missing_links         
   else write_about_pointed_card l pointed_card solution ;;

let partial_analysis l =
    let temp = partial_analysis_without_writing l in 
    let text = "\n\n\n"^(write_about_partial_analysis l temp)^"\n\n\n" in 
    let _ = (print_string text;flush stdout) in 
    temp ;;

let check_admissibility x=
  let rec tempf =( 
     fun to_be_treated -> match to_be_treated with 
     [] -> true
     | a:: others -> 
        if others = []
        then true 
        else if List.exists (fun (x,y)->x+y=a) (Uple.inclusive_list_of_pairs others) 
        then tempf(others)
        else false     
  ) in 
  tempf(List.rev x) ;;
  
exception Nonadmissible_set of int list ;;

let check_admissiblity_agressively sol =
    if check_admissibility sol then () else raise(Nonadmissible_set(sol));;

exception Non_inclusion of (int list) * (int list) ;;

let initial_check (x,sol) =
   let _ = check_admissiblity_agressively sol in 
   if not(i_is_included_in x sol)
   then raise(Non_inclusion(x,sol)) 
   else () ;;    



exception Impatient_main_exn of int list ;;

let impatient_main l = match Hashtbl.find_opt hashtbl_for_impatient_main l with 
  Some (answer) -> answer
  |None -> raise(Impatient_main_exn(l));;

let impatient_measure l = List.length (impatient_main l) ;;

Hashtbl.add hashtbl_for_impatient_main [1] [1] ;;

exception Simplest_case_exn of (int list) * int ;; 

let simplest_case n sol = 
    let _ = initial_check ([n],sol) in 
    let temp1 = abauzit_expansion [n] in 
    let tempm = (fun l->Max.list (Image.image (fun t->impatient_measure [t]) l)) in 
    let m = Min.list (Image.image tempm temp1) in     
    if List.length(sol)<>m+1 
    then raise(Simplest_case_exn(sol,m+1))
    else  Hashtbl.add  hashtbl_for_impatient_main [n] sol ;;

exception Bad_pointed_card_element of (int list) * int * (int list) ;; 

let check_pointed_card_element (l2,(head,passive_part))=
   if not(Listennou.extends (List.rev l2) (List.rev(head::passive_part))) 
   then raise(Bad_pointed_card_element(l2,head,passive_part))
   else () ;;
   
let check_pointed_card = List.iter  check_pointed_card_element ;;

exception Pointed_card_exn of ((int list) * int) list ;;

let pointed_card l pointed_card sol =
  let _ = initial_check (l,sol) 
  and _ = check_pointed_card pointed_card in 
  let temp1 = abauzit_expansion l 
  and bound = List.length(sol)-1 in 
  let temp2 = Image.image (
    fun l2 -> match List.assoc_opt l2 pointed_card with 
    Some(head,passive_part) ->(l2,impatient_measure([head])+List.length passive_part)
    |None -> (l2,impatient_measure l2)
  ) temp1 in 
  let bad_ones = List.filter (fun (l2,m_l2)->m_l2<bound) temp2 in 
  if bad_ones <> [] 
  then raise(Pointed_card_exn(bad_ones))
  else Hashtbl.add  hashtbl_for_impatient_main l sol ;;    

let pa = partial_analysis ;;
let pb n = pa [n] ;;

simplest_case 2 [1;2] ;;
simplest_case 3 [1;2;3] ;;
simplest_case 4 [1;2;4] ;;
simplest_case 5 [1;2;3;5] ;;
simplest_case 6 [1;2;3;6] ;;
pointed_card [7] [([3; 4], (3, [4])); ([2; 5], (5, [])); ([1; 6], (6, []))]  [1; 2; 3; 4; 7] ;;
pointed_card [3;8] [([3; 4], (3, [4])); ([3; 5], (3, [5])); ([2; 3; 6], (2, [3; 6]));
([1; 3; 7], (3, [7]))] [1; 2; 3; 4; 8];;
simplest_case 8 [1;2;4;8] ;;
simplest_case 9 [1; 2; 3; 6; 9] ;;
simplest_case 10 [1; 2; 3; 5; 10] ;;
pointed_card [11]  [([5; 6], (5, [6])); ([4; 7], (7, [])); ([2; 9], (9, []));
([1; 10], (10, []))] [1; 2; 3; 4; 7; 11] ;;
pointed_card [5;12] [([5;6],(5,[6]));([5;7],(5,[7]));([4;5;8],(4,[5;8]));([3;5;9],(3,[5;9]));([2;5;10],(5,[10]));([1;5;11],(5,[11]))] [1;2;3;5;6;12] ;;
simplest_case 12 [1; 2; 3; 6; 12] ;;
pointed_card [13] [([6; 7], (6, [7])); ([5; 8], (5, [8])); ([4; 9], (9, []));
([3; 10], (10, [])); ([2; 11], (11, [])); ([1; 12], (12, []))] [1; 2; 3; 5; 8; 13] ;;
pointed_card [14] [([7], (7, [])); ([6; 8], (6, [8])); ([5; 9], (5, [9])); ([4; 10], (10, []));
([3; 11], (11, [])); ([2; 12], (12, [])); ([1; 13], (13, []))]  [1; 2; 3; 4; 7; 14] ;;
simplest_case 15 [1; 2; 3; 5; 10; 15] ;;
pointed_card [3;16] [([3;13],(13,[]));([3;7;9],(3,[7;9]));([3;6;10],(3,[6;10]));([3;5;11],(3,[5;11]));([3;4;12],(3,[4;12]));([2;3;14],(14,[]));([1;3;15],(15,[]))] [1;2;3;4;8;16] ;;
pointed_card [5;16] [([5;8],(5,[8]));([5;11],(5,[11]));([5;7;9],(5,[7;9]));([5;6;10],(5,[6;10]));([4;5;12],(4,[5;12]));([3;5;13],(3,[5;13]));([2;5;14],(5,[14]));([1;5;15],(5,[15]))] [1;2;3;5;8;16] ;;
pointed_card [6;16] [([6;8],(6,[8]));([6;10],(6,[10]));([6;7;9],(6,[7;9]));([5;6;11],(5,[6;11]));([4;6;12],(4,[6;12]));([3;6;13],(3,[6;13]));([2;6;14],(6,[14]));([1;6;15],(6,[15]))] [1;2;3;6;8;16] ;;
pointed_card [7;16] [([7;8],(7,[8]));([7;9],(7,[9]));([6;7;10],(6,[7;10]));([5;7;11],(5,[7;11]));([4;7;12],(7,[12]));([3;7;13],(7,[13]));([2;7;14],(7,[14]));([1;7;15],(7,[15]))] [1;2;3;4;7;8;16] ;;
simplest_case 16 [1; 2; 4; 8; 16] ;;
pointed_card [12;17] [([8;9;12],(8,[9;12]));([7;10;12],(7,[10;12]));([6;11;12],(6,[11;12]));([4;12;13],(12,[13]));([3;12;14],(12,[14]));([2;12;15],(12,[15]));([1;12;16],(12,[16]))] [1;2;3;5;6;12;17] ;;
simplest_case 17 [1; 2; 4; 8; 9; 17] ;;
simplest_case 18 [1; 2; 3; 6; 9; 18] ;;
pointed_card [19] [([9;10],(9,[10]));([8;11],(11,[]));([7;12],(7,[12]));([6;13],(13,[]));([5;14],(14,[]));([4;15],(15,[]));([2;17],(17,[]));([1;18],(18,[]))] [1;2;3;4;8;11;19] ;;
pointed_card [7;20] [([7;10],(7,[10]));([7;13],(7,[13]));([7;9;11],(7,[9;11]));([7;8;12],(7,[8;12]));([6;7;14],(6,[7;14]));([5;7;15],(5,[7;15]));([4;7;16],(7,[16]));([3;7;17],(7,[17]));([2;7;18],(7,[18]));([1;7;19],(7,[19]))] [1;2;3;4;7;10;20] ;;
pointed_card [9;20] [([9;10],(9,[10]));([9;11],(9,[11]));([8;9;12],(8,[9;12]));([7;9;13],(7,[9;13]));([6;9;14],(6,[9;14]));([5;9;15],(5,[9;15]));([4;9;16],(9,[16]));([3;9;17],(9,[17]));([2;9;18],(9,[18]));([1;9;19],(9,[19]))] [1;2;3;6;9;10;20] ;;
simplest_case 20 [1;2;3;5;10;20] ;;
pointed_card [21] [([10;11],(10,[11]));([9;12],(9,[12]));([8;13],(13,[]));([7;14],(7,[14]));([6;15],(15,[]));([4;17],(17,[]));([3;18],(18,[]));([2;19],(19,[]));([1;20],(20,[]))] [1;2;3;4;7;14;21] ;;
pointed_card [22] [([11],(11,[]));([10;12],(10,[12]));([9;13],(9,[13]));([8;14],(14,[]));([7;15],(7,[15]));([5;17],(17,[]));([4;18],(18,[]));([3;19],(19,[]));([2;20],(20,[]));([1;21],(21,[]))] [1;2;3;4;7;11;22] ;;
pointed_card [23] [([11;12],(11,[12]));([10;13],(10,[13]));([9;14],(9,[14]));([8;15],(15,[]));([7;16],(7,[16]));([6;17],(17,[]));([5;18],(18,[]));([4;19],(19,[]));([3;20],(20,[]));([2;21],(21,[]));([1;22],(22,[]))] [1;2;3;5;10;13;23] ;;
pointed_card [5;24] [([5;19],(19,[]));([5;11;13],(5,[11;13]));([5;10;14],(5,[10;14]));([5;9;15],(5,[9;15]));([5;8;16],(5,[8;16]));([5;7;17],(5,[7;17]));([5;6;18],(5,[6;18]));([4;5;20],(20,[]));([3;5;21],(21,[]));([2;5;22],(22,[]));([1;5;23],(23,[]))] [1;2;3;5;6;12;24] ;;
pointed_card [7;24] [([7;12],(7,[12]));([7;17],(7,[17]));([7;11;13],(7,[11;13]));([7;10;14],(7,[10;14]));([7;9;15],(7,[9;15]));([7;8;16],(7,[8;16]));([6;7;18],(6,[7;18]));([5;7;19],(5,[7;19]));([4;7;20],(7,[20]));([3;7;21],(7,[21]));([2;7;22],(7,[22]));([1;7;23],(7,[23]))] [1;2;3;5;7;12;24] ;;
simplest_case 24 [1;2;3;6;12;24] ;;
pointed_card [25] [([12;13],(12,[13]));([11;14],(11,[14]));([10;15],(10,[15]));([9;16],(9,[16]));([8;17],(17,[]));([7;18],(7,[18]));([6;19],(19,[]));([5;20],(20,[]));([4;21],(21,[]));([3;22],(22,[]));([2;23],(23,[]));([1;24],(24,[]))] [1;2;3;5;10;15;25] ;;
pointed_card [26] [([13],(13,[]));([12;14],(12,[14]));([11;15],(11,[15]));([10;16],(10,[16]));([9;17],(9,[17]));([8;18],(18,[]));([7;19],(7,[19]));([6;20],(20,[]));([5;21],(21,[]));([4;22],(22,[]));([3;23],(23,[]));([2;24],(24,[]));([1;25],(25,[]))] [1;2;3;5;8;13;26] ;;
pointed_card [20;27] [([13;14;20],(13,[14;20]));([12;15;20],(12,[15;20]));([11;16;20],(11,[16;20]));([10;17;20],(10,[17;20]));([9;18;20],(9,[18;20]));([8;19;20],(19,[20]));([6;20;21],(20,[21]));([5;20;22],(20,[22]));([4;20;23],(20,[23]));([3;20;24],(20,[24]));([2;20;25],(20,[25]));([1;20;26],(20,[26]))] [1;2;3;4;7;10;20;27] ;;
simplest_case 27 [1;2;3;6;9;18;27] ;;
pointed_card [28] [([14],(14,[]));([13;15],(13,[15]));([12;16],(12,[16]));([11;17],(11,[17]));([10;18],(10,[18]));([9;19],(9,[19]));([8;20],(20,[]));([7;21],(7,[21]));([6;22],(22,[]));([5;23],(23,[]));([4;24],(24,[]));([3;25],(25,[]));([2;26],(26,[]));([1;27],(27,[]))] [1;2;3;4;7;14;28] ;;
pointed_card [29] [([14;15],(14,[15]));([13;16],(13,[16]));([11;18],(11,[18]));([10;19],(19,[]));([8;21],(21,[]));([7;22],(22,[]));([6;23],(23,[]));([4;25],(25,[]));([3;26],(26,[]));([2;27],(27,[]));([1;28],(28,[]))] [1;2;3;4;7;11;18;29] ;;
pointed_card [17;30] [([13;17],(13,[17]));([15;17],(15,[17]));([14;16;17],(14,[16;17]));([12;17;18],(12,[17;18]));([11;17;19],(11,[17;19]));([10;17;20],(10,[17;20]));([9;17;21],(9,[17;21]));([8;17;22],(17,[22]));([7;17;23],(7,[17;23]));([6;17;24],(17,[24]));([5;17;25],(17,[25]));([4;17;26],(17,[26]));([3;17;27],(17,[27]));([2;17;28],(17,[28]));([1;17;29],(17,[29]))] [1;2;3;5;10;15;17;30] ;;
simplest_case 30 [1;2;3;5;10;15;30] ;;
pointed_card [31] [([15;16],(15,[16]));([14;17],(14,[17]));([13;18],(13,[18]));([12;19],(19,[]));([11;20],(11,[20]));([10;21],(21,[]));([9;22],(22,[]));([8;23],(23,[]));([6;25],(25,[]));([5;26],(26,[]));([4;27],(27,[]));([3;28],(28,[]));([2;29],(29,[]));([1;30],(30,[]))] [1;2;3;4;7;14;17;31] ;;
pointed_card [5;16;32] [([5;16;27],(5,[16;27]));([5;15;16;17],(5,[15;16;17]));([5;14;16;18],(5,[14;16;18]));([5;13;16;19],(5,[13;16;19]));([5;12;16;20],(5,[12;16;20]));([5;11;16;21],(5,[11;16;21]));([5;10;16;22],(5,[10;16;22]));([5;9;16;23],(5,[9;16;23]));([5;8;16;24],(5,[8;16;24]));([5;7;16;25],(5,[7;16;25]));([5;6;16;26],(5,[6;16;26]));([4;5;16;28],(4,[5;16;28]));([3;5;16;29],(3,[5;16;29]));([2;5;16;30],(5,[16;30]));([1;5;16;31],(5,[16;31]))] [1;2;3;5;8;16;32] ;;
pointed_card [3;32] [([3;29],(29,[]));([3;15;17],(15,[17]));([3;14;18],(14,[18]));([3;13;19],(13,[19]));([3;12;20],(12,[20]));([3;11;21],(11,[21]));([3;10;22],(10,[22]));([3;9;23],(9,[23]));([3;8;24],(24,[]));([3;7;25],(7,[25]));([3;6;26],(26,[]));([3;5;27],(27,[]));([3;4;28],(28,[]));([2;3;30],(30,[]));([1;3;31],(31,[]))] [1;2;3;4;8;16;32] ;;
pointed_card [5;32] [([5;27],(27,[]));([5;15;17],(5,[15;17]));([5;14;18],(5,[14;18]));([5;13;19],(5,[13;19]));([5;12;20],(5,[12;20]));([5;11;21],(5,[11;21]));([5;10;22],(5,[10;22]));([5;9;23],(5,[9;23]));([5;8;24],(5,[8;24]));([5;7;25],(5,[7;25]));([5;6;26],(5,[6;26]));([4;5;28],(28,[]));([3;5;29],(29,[]));([2;5;30],(30,[]));([1;5;31],(31,[]))] [1;2;3;5;8;16;32] ;;
pointed_card [6;32] [([6;26],(26,[]));([6;15;17],(6,[15;17]));([6;14;18],(6,[14;18]));([6;13;19],(6,[13;19]));([6;12;20],(6,[12;20]));([6;11;21],(6,[11;21]));([6;10;22],(6,[10;22]));([6;9;23],(6,[9;23]));([6;8;24],(6,[8;24]));([6;7;25],(6,[7;25]));([5;6;27],(5,[6;27]));([4;6;28],(28,[]));([3;6;29],(29,[]));([2;6;30],(30,[]));([1;6;31],(31,[]))] [1;2;3;6;8;16;32] ;;
pointed_card [7;32] [([7;25],(25,[]));([7;15;17],(7,[15;17]));([7;14;18],(7,[14;18]));([7;13;19],(7,[13;19]));([7;12;20],(7,[12;20]));([7;11;21],(7,[11;21]));([7;10;22],(7,[10;22]));([7;9;23],(7,[9;23]));([7;8;24],(7,[8;24]));([6;7;26],(26,[]));([5;7;27],(27,[]));([4;7;28],(28,[]));([3;7;29],(29,[]));([2;7;30],(30,[]));([1;7;31],(31,[]))] [1;2;3;4;7;8;16;32] ;;
pointed_card [9;32] [([9;16],(9,[16]));([9;23],(9,[23]));([9;15;17],(9,[15;17]));([9;14;18],(9,[14;18]));([9;13;19],(9,[13;19]));([9;12;20],(9,[12;20]));([9;11;21],(9,[11;21]));([9;10;22],(9,[10;22]));([8;9;24],(8,[9;24]));([7;9;25],(7,[9;25]));([6;9;26],(6,[9;26]));([5;9;27],(5,[9;27]));([4;9;28],(9,[28]));([3;9;29],(9,[29]));([2;9;30],(9,[30]));([1;9;31],(9,[31]))] [1;2;4;8;9;16;32] ;;
pointed_card [10;32] [([10;16],(10,[16]));([10;22],(10,[22]));([10;15;17],(10,[15;17]));([10;14;18],(10,[14;18]));([10;13;19],(10,[13;19]));([10;12;20],(10,[12;20]));([10;11;21],(10,[11;21]));([9;10;23],(9,[10;23]));([8;10;24],(8,[10;24]));([7;10;25],(7,[10;25]));([6;10;26],(6,[10;26]));([5;10;27],(5,[10;27]));([4;10;28],(10,[28]));([3;10;29],(10,[29]));([2;10;30],(10,[30]));([1;10;31],(10,[31]))] [1;2;4;6;10;16;32] ;;
pointed_card [12;32] [([12;16],(12,[16]));([12;20],(12,[20]));([12;15;17],(12,[15;17]));([12;14;18],(12,[14;18]));([12;13;19],(12,[13;19]));([11;12;21],(11,[12;21]));([10;12;22],(10,[12;22]));([9;12;23],(9,[12;23]));([8;12;24],(8,[12;24]));([7;12;25],(7,[12;25]));([6;12;26],(6,[12;26]));([5;12;27],(5,[12;27]));([4;12;28],(12,[28]));([3;12;29],(12,[29]));([2;12;30],(12,[30]));([1;12;31],(12,[31]))] [1;2;4;6;12;16;32] ;;
pointed_card [15;32] [([15;16],(15,[16]));([15;17],(15,[17]));([14;15;18],(14,[15;18]));([13;15;19],(13,[15;19]));([12;15;20],(12,[15;20]));([11;15;21],(11,[15;21]));([10;15;22],(10,[15;22]));([9;15;23],(9,[15;23]));([8;15;24],(15,[24]));([7;15;25],(7,[15;25]));([6;15;26],(15,[26]));([5;15;27],(15,[27]));([4;15;28],(15,[28]));([3;15;29],(15,[29]));([2;15;30],(15,[30]));([1;15;31],(15,[31]))] [1;2;3;5;10;15;16;32] ;;
simplest_case 32 [1;2;4;8;16;32] ;;
pointed_card [14;33] [([14;19],(14,[19]));([14;16;17],(14,[16;17]));([14;15;18],(14,[15;18]));([13;14;20],(13,[14;20]));([12;14;21],(12,[14;21]));([11;14;22],(11,[14;22]));([10;14;23],(10,[14;23]));([9;14;24],(9,[14;24]));([8;14;25],(14,[25]));([7;14;26],(7,[14;26]));([6;14;27],(14,[27]));([5;14;28],(14,[28]));([4;14;29],(14,[29]));([3;14;30],(14,[30]));([2;14;31],(14,[31]));([1;14;32],(14,[32]))] [1;2;3;5;7;14;19;33] ;;
pointed_card [20;33] [([13;20],(13,[20]));([16;17;20],(16,[17;20]));([15;18;20],(15,[18;20]));([14;19;20],(14,[19;20]));([12;20;21],(12,[20;21]));([11;20;22],(11,[20;22]));([10;20;23],(10,[20;23]));([9;20;24],(9,[20;24]));([8;20;25],(20,[25]));([7;20;26],(7,[20;26]));([6;20;27],(20,[27]));([5;20;28],(20,[28]));([4;20;29],(20,[29]));([3;20;30],(20,[30]));([2;20;31],(20,[31]));([1;20;32],(20,[32]))] [1;2;3;5;10;13;20;33] ;;
simplest_case 33 [1;2;4;8;16;17;33] ;;
pointed_card [13;34] [([13;17],(13,[17]));([13;21],(13,[21]));([13;16;18],(13,[16;18]));([13;15;19],(13,[15;19]));([13;14;20],(13,[14;20]));([12;13;22],(12,[13;22]));([11;13;23],(11,[13;23]));([10;13;24],(10,[13;24]));([9;13;25],(9,[13;25]));([8;13;26],(13,[26]));([7;13;27],(7,[13;27]));([6;13;28],(13,[28]));([5;13;29],(13,[29]));([4;13;30],(13,[30]));([3;13;31],(13,[31]));([2;13;32],(13,[32]));([1;13;33],(13,[33]))] [1;2;3;5;8;13;21;34] ;;
simplest_case 34 [1;2;4;8;9;17;34] ;;
pointed_card [35] [([17;18],(17,[18]));([16;19],(19,[]));([15;20],(15,[20]));([14;21],(14,[21]));([13;22],(13,[22]));([12;23],(23,[]));([11;24],(11,[24]));([10;25],(25,[]));([9;26],(26,[]));([8;27],(27,[]));([7;28],(28,[]));([6;29],(29,[]));([5;30],(30,[]));([4;31],(31,[]));([2;33],(33,[]));([1;34],(34,[]))] [1;2;3;4;7;14;21;35] ;;
pointed_card [11;36] [([11;18],(11,[18]));([11;25],(11,[25]));([11;17;19],(11,[17;19]));([11;16;20],(11,[16;20]));([11;15;21],(11,[15;21]));([11;14;22],(11,[14;22]));([11;13;23],(11,[13;23]));([11;12;24],(11,[12;24]));([10;11;26],(10,[11;26]));([9;11;27],(9,[11;27]));([8;11;28],(11,[28]));([7;11;29],(7,[11;29]));([6;11;30],(11,[30]));([5;11;31],(11,[31]));([4;11;32],(11,[32]));([3;11;33],(11,[33]));([2;11;34],(11,[34]));([1;11;35],(11,[35]))] [1;2;3;4;7;11;18;36] ;;
pointed_card [17;36] [([17;18],(17,[18]));([17;19],(17,[19]));([16;17;20],(16,[17;20]));([15;17;21],(15,[17;21]));([14;17;22],(14,[17;22]));([13;17;23],(13,[17;23]));([12;17;24],(12,[17;24]));([11;17;25],(11,[17;25]));([10;17;26],(10,[17;26]));([9;17;27],(9,[17;27]));([8;17;28],(17,[28]));([7;17;29],(7,[17;29]));([6;17;30],(17,[30]));([5;17;31],(17,[31]));([4;17;32],(17,[32]));([3;17;33],(17,[33]));([2;17;34],(17,[34]));([1;17;35],(17,[35]))] [1;2;4;8;9;17;18;36] ;;
simplest_case 36 [1;2;3;6;9;18;36] ;;
pointed_card [37] [([18;19],(18,[19]));([17;20],(17,[20]));([16;21],(21,[]));([15;22],(15,[22]));([14;23],(14,[23]));([13;24],(13,[24]));([12;25],(25,[]));([11;26],(11,[26]));([10;27],(27,[]));([9;28],(28,[]));([8;29],(29,[]));([7;30],(30,[]));([6;31],(31,[]));([4;33],(33,[]));([3;34],(34,[]));([2;35],(35,[]));([1;36],(36,[]))] [1;2;3;5;8;16;21;37] ;;
pointed_card [38] [([19],(19,[]));([18;20],(18,[20]));([17;21],(17,[21]));([16;22],(22,[]));([15;23],(15,[23]));([14;24],(14,[24]));([13;25],(13,[25]));([12;26],(26,[]));([11;27],(11,[27]));([10;28],(28,[]));([9;29],(29,[]));([8;30],(30,[]));([7;31],(31,[]));([5;33],(33,[]));([4;34],(34,[]));([3;35],(35,[]));([2;36],(36,[]));([1;37],(37,[]))] [1;2;3;4;8;11;19;38] ;;
pointed_card [39] [([19;20],(19,[20]));([18;21],(18,[21]));([17;22],(17,[22]));([16;23],(23,[]));([15;24],(15,[24]));([14;25],(14,[25]));([13;26],(13,[26]));([12;27],(27,[]));([11;28],(11,[28]));([10;29],(29,[]));([9;30],(30,[]));([8;31],(31,[]));([6;33],(33,[]));([5;34],(34,[]));([4;35],(35,[]));([3;36],(36,[]));([2;37],(37,[]));([1;38],(38,[]))] [1;2;3;5;8;13;26;39] ;;
pointed_card [7;40] [([7;33],(33,[]));([7;19;21],(7,[19;21]));([7;18;22],(7,[18;22]));([7;17;23],(7,[17;23]));([7;16;24],(7,[16;24]));([7;15;25],(7,[15;25]));([7;14;26],(7,[14;26]));([7;13;27],(7,[13;27]));([7;12;28],(7,[12;28]));([7;11;29],(7,[11;29]));([7;10;30],(7,[10;30]));([7;9;31],(7,[9;31]));([7;8;32],(7,[8;32]));([6;7;34],(34,[]));([5;7;35],(35,[]));([4;7;36],(36,[]));([3;7;37],(37,[]));([2;7;38],(38,[]));([1;7;39],(39,[]))] [1;2;3;4;7;10;20;40] ;;
pointed_card [13;40] [([13;20],(13,[20]));([13;27],(13,[27]));([13;19;21],(13,[19;21]));([13;18;22],(13,[18;22]));([13;17;23],(13,[17;23]));([13;16;24],(13,[16;24]));([13;15;25],(13,[15;25]));([13;14;26],(13,[14;26]));([12;13;28],(12,[13;28]));([11;13;29],(11,[13;29]));([10;13;30],(10,[13;30]));([9;13;31],(9,[13;31]));([8;13;32],(13,[32]));([7;13;33],(7,[13;33]));([6;13;34],(13,[34]));([5;13;35],(13,[35]));([4;13;36],(13,[36]));([3;13;37],(13,[37]));([2;13;38],(13,[38]));([1;13;39],(13,[39]))] [1;2;3;5;10;13;20;40] ;;
pointed_card [15;40] [([15;20],(15,[20]));([15;25],(15,[25]));([15;19;21],(15,[19;21]));([15;18;22],(15,[18;22]));([15;17;23],(15,[17;23]));([15;16;24],(15,[16;24]));([14;15;26],(14,[15;26]));([13;15;27],(13,[15;27]));([12;15;28],(12,[15;28]));([11;15;29],(11,[15;29]));([10;15;30],(10,[15;30]));([9;15;31],(9,[15;31]));([8;15;32],(15,[32]));([7;15;33],(7,[15;33]));([6;15;34],(15,[34]));([5;15;35],(15,[35]));([4;15;36],(15,[36]));([3;15;37],(15,[37]));([2;15;38],(15,[38]));([1;15;39],(15,[39]))] [1;2;3;5;10;15;20;40] ;;
simplest_case 40 [1;2;3;5;10;20;40] ;;
pointed_card [41] [([20;21],(20,[21]));([19;22],(19,[22]));([18;23],(18,[23]));([17;24],(17,[24]));([16;25],(25,[]));([15;26],(15,[26]));([14;27],(14,[27]));([13;28],(13,[28]));([12;29],(29,[]));([11;30],(11,[30]));([10;31],(31,[]));([8;33],(33,[]));([7;34],(34,[]));([6;35],(35,[]));([5;36],(36,[]));([4;37],(37,[]));([3;38],(38,[]));([2;39],(39,[]));([1;40],(40,[]))] [1;2;3;5;10;20;21;41] ;;
pointed_card [42] [([21],(21,[]));([20;22],(20,[22]));([19;23],(19,[23]));([18;24],(18,[24]));([17;25],(17,[25]));([16;26],(26,[]));([15;27],(15,[27]));([14;28],(14,[28]));([13;29],(13,[29]));([12;30],(30,[]));([11;31],(11,[31]));([9;33],(33,[]));([8;34],(34,[]));([7;35],(35,[]));([6;36],(36,[]));([5;37],(37,[]));([4;38],(38,[]));([3;39],(39,[]));([2;40],(40,[]));([1;41],(41,[]))] [1;2;3;4;7;14;21;42] ;;
pointed_card [43] [([21;22],(21,[22]));([20;23],(20,[23]));([19;24],(19,[24]));([18;25],(18,[25]));([17;26],(17,[26]));([16;27],(27,[]));([15;28],(15,[28]));([14;29],(14,[29]));([13;30],(13,[30]));([12;31],(31,[]));([11;32],(11,[32]));([10;33],(33,[]));([9;34],(34,[]));([8;35],(35,[]));([7;36],(36,[]));([6;37],(37,[]));([5;38],(38,[]));([4;39],(39,[]));([3;40],(40,[]));([2;41],(41,[]));([1;42],(42,[]))] [1;2;3;5;10;20;23;43] ;;
pointed_card [44] [([22],(22,[]));([21;23],(21,[23]));([20;24],(20,[24]));([19;25],(19,[25]));([18;26],(18,[26]));([17;27],(17,[27]));([16;28],(28,[]));([15;29],(15,[29]));([14;30],(14,[30]));([13;31],(13,[31]));([11;33],(11,[33]));([10;34],(34,[]));([9;35],(35,[]));([8;36],(36,[]));([7;37],(37,[]));([6;38],(38,[]));([5;39],(39,[]));([4;40],(40,[]));([3;41],(41,[]));([2;42],(42,[]));([1;43],(43,[]))] [1;2;3;4;7;11;22;44] ;;
pointed_card [45] [([22;23],(22,[23]));([21;24],(21,[24]));([20;25],(20,[25]));([19;26],(19,[26]));([18;27],(18,[27]));([17;28],(17,[28]));([16;29],(29,[]));([15;30],(15,[30]));([14;31],(14,[31]));([13;32],(13,[32]));([12;33],(33,[]));([11;34],(11,[34]));([10;35],(35,[]));([9;36],(36,[]));([8;37],(37,[]));([7;38],(38,[]));([6;39],(39,[]));([5;40],(40,[]));([4;41],(41,[]));([3;42],(42,[]));([2;43],(43,[]));([1;44],(44,[]))] [1;2;3;5;10;15;30;45] ;;
pointed_card [46] [([23],(23,[]));([22;24],(22,[24]));([21;25],(21,[25]));([20;26],(20,[26]));([19;27],(19,[27]));([18;28],(18,[28]));([17;29],(17,[29]));([16;30],(30,[]));([15;31],(15,[31]));([14;32],(14,[32]));([13;33],(13,[33]));([12;34],(34,[]));([11;35],(11,[35]));([10;36],(36,[]));([9;37],(37,[]));([8;38],(38,[]));([7;39],(39,[]));([6;40],(40,[]));([5;41],(41,[]));([4;42],(42,[]));([3;43],(43,[]));([2;44],(44,[]));([1;45],(45,[]))] [1;2;3;5;10;13;23;46] ;;
pointed_card [47] [([23;24],(23,[24]));([22;25],(22,[25]));([21;26],(21,[26]));([19;28],(19,[28]));([18;29],(29,[]));([16;31],(31,[]));([12;35],(35,[]));([10;37],(37,[]));([9;38],(38,[]));([8;39],(39,[]));([6;41],(41,[]));([5;42],(42,[]));([4;43],(43,[]));([3;44],(44,[]));([2;45],(45,[]));([1;46],(46,[]))] [1;2;3;4;7;10;20;27;47] ;;
pointed_card [5;48] [([5;43],(43,[]));([5;23;25],(23,[25]));([5;22;26],(22,[26]));([5;21;27],(21,[27]));([5;20;28],(20,[28]));([5;19;29],(19,[29]));([5;18;30],(18,[30]));([5;17;31],(17,[31]));([5;15;33],(15,[33]));([5;14;34],(14,[34]));([5;13;35],(13,[35]));([5;12;36],(36,[]));([5;11;37],(11,[37]));([5;10;38],(38,[]));([5;9;39],(39,[]));([5;8;40],(40,[]));([5;7;41],(41,[]));([5;6;42],(42,[]));([4;5;44],(44,[]));([3;5;45],(45,[]));([2;5;46],(46,[]));([1;5;47],(47,[]))] [1;2;3;5;6;12;24;48] ;;
pointed_card [7;48] [([7;41],(41,[]));([7;23;25],(7,[23;25]));([7;22;26],(7,[22;26]));([7;21;27],(7,[21;27]));([7;20;28],(7,[20;28]));([7;19;29],(7,[19;29]));([7;18;30],(7,[18;30]));([7;17;31],(7,[17;31]));([7;16;32],(7,[16;32]));([7;15;33],(7,[15;33]));([7;14;34],(7,[14;34]));([7;13;35],(7,[13;35]));([7;12;36],(7,[12;36]));([7;11;37],(7,[11;37]));([7;10;38],(7,[10;38]));([7;9;39],(7,[9;39]));([7;8;40],(7,[8;40]));([6;7;42],(42,[]));([5;7;43],(43,[]));([4;7;44],(44,[]));([3;7;45],(45,[]));([2;7;46],(46,[]));([1;7;47],(47,[]))] [1;2;3;5;7;12;24;48] ;;
simplest_case 48 [1;2;3;6;12;24;48] ;;
pointed_card [49] [([24;25],(24,[25]));([23;26],(23,[26]));([22;27],(22,[27]));([21;28],(21,[28]));([20;29],(20,[29]));([19;30],(19,[30]));([18;31],(18,[31]));([17;32],(17,[32]));([16;33],(33,[]));([15;34],(15,[34]));([14;35],(14,[35]));([13;36],(13,[36]));([12;37],(37,[]));([11;38],(11,[38]));([10;39],(39,[]));([9;40],(40,[]));([8;41],(41,[]));([7;42],(42,[]));([6;43],(43,[]));([5;44],(44,[]));([4;45],(45,[]));([3;46],(46,[]));([2;47],(47,[]));([1;48],(48,[]))] [1;2;3;6;12;24;25;49] ;;
pointed_card [50] [([25],(25,[]));([24;26],(24,[26]));([23;27],(23,[27]));([22;28],(22,[28]));([21;29],(21,[29]));([20;30],(20,[30]));([19;31],(19,[31]));([18;32],(18,[32]));([17;33],(17,[33]));([16;34],(34,[]));([15;35],(15,[35]));([14;36],(14,[36]));([13;37],(13,[37]));([12;38],(38,[]));([11;39],(11,[39]));([10;40],(40,[]));([9;41],(41,[]));([8;42],(42,[]));([7;43],(43,[]));([6;44],(44,[]));([5;45],(45,[]));([4;46],(46,[]));([3;47],(47,[]));([2;48],(48,[]));([1;49],(49,[]))] [1;2;3;5;10;15;25;50] ;;
simplest_case 51 [1;2;3;6;12;24;27;51] ;;
pointed_card [52] [([26],(26,[]));([25;27],(25,[27]));([24;28],(24,[28]));([23;29],(23,[29]));([22;30],(22,[30]));([21;31],(21,[31]));([20;32],(20,[32]));([19;33],(19,[33]));([18;34],(18,[34]));([17;35],(17,[35]));([16;36],(36,[]));([15;37],(15,[37]));([14;38],(14,[38]));([13;39],(13,[39]));([12;40],(40,[]));([11;41],(11,[41]));([10;42],(42,[]));([9;43],(43,[]));([8;44],(44,[]));([7;45],(45,[]));([6;46],(46,[]));([5;47],(47,[]));([4;48],(48,[]));([3;49],(49,[]));([2;50],(50,[]));([1;51],(51,[]))] [1;2;3;5;8;13;26;52] ;;
pointed_card [53] [([26;27],(26,[27]));([25;28],(25,[28]));([24;29],(29,[]));([23;30],(23,[30]));([22;31],(22,[31]));([21;32],(21,[32]));([19;34],(19,[34]));([18;35],(35,[]));([16;37],(37,[]));([15;38],(38,[]));([14;39],(39,[]));([12;41],(41,[]));([11;42],(42,[]));([10;43],(43,[]));([9;44],(44,[]));([8;45],(45,[]));([7;46],(46,[]));([6;47],(47,[]));([4;49],(49,[]));([3;50],(50,[]));([2;51],(51,[]));([1;52],(52,[]))] [1;2;3;5;6;12;24;29;53] ;;
simplest_case 54 [1;2;3;6;9;18;27;54] ;;
pointed_card [55] [([27;28],(27,[28]));([26;29],(26,[29]));([25;30],(25,[30]));([24;31],(31,[]));([23;32],(23,[32]));([22;33],(22,[33]));([21;34],(21,[34]));([20;35],(35,[]));([19;36],(19,[36]));([18;37],(37,[]));([17;38],(38,[]));([16;39],(39,[]));([14;41],(41,[]));([13;42],(42,[]));([12;43],(43,[]));([11;44],(44,[]));([10;45],(45,[]));([9;46],(46,[]));([8;47],(47,[]));([6;49],(49,[]));([5;50],(50,[]));([4;51],(51,[]));([3;52],(52,[]));([2;53],(53,[]));([1;54],(54,[]))] [1;2;3;4;7;11;22;33;55] ;;



type abbreviated_move = P of int list |S of int ;;

let all_moves = [S 2; S 3; S 4; S 5; S 6; P [7]; P [3; 8]; S 8; S 9; S 10; P [11]; P [5; 12];
S 12; P [13]; P [14]; S 15; P [3; 16]; P [5; 16]; P [6; 16]; P [7; 16]; 
S 16; P [12; 17]; S 17; S 18; P [19]; P [7; 20]; P [9; 20]; S 20; P [21];
P [22]; P [23]; P [5; 24]; P [7; 24]; S 24; P [25]; P [26]; P [20; 27]; 
S 27; P [28]; P [29]; P [17; 30]; S 30; P [31]; P [5; 16; 32]; P [3; 32];
P [5; 32]; P [6; 32]; P [7; 32]; P [9; 32]; P [10; 32]; P [12; 32];
P [15; 32]; S 32; P [14; 33]; P [20; 33]; S 33; P [13; 34]; S 34; P [35];
P [11; 36]; P [17; 36]; S 36; P [37]; P [38]; P [39]; P [7; 40]; P [13; 40];
P [15; 40]; S 40; P [41]; P [42]; P [43]; P [44]; P [45]; P [46]; P [47];
P [5; 48]; P [7; 48]; S 48; P [49]; P [50]; S 51; P [52]; P [53]; S 54;
P [55]] ;;

type expanded_move = ES of int * int | EP of (int list) * int * ((int * int list) list) * ((int list) list) ;;

let expand_move  = function
  S i->ES(i,impatient_measure [i])
  |(P l)-> 
  let temp1 = abauzit_expansion l 
   and bound = (impatient_measure l)-1 in 
  let temp2 = Image.image (fun l2-> (l2,adrien_analysis (l2,bound)) ) temp1 in 
  let (good_temp2,bad_temp2) = List.partition (fun (l2,opt)->opt<>None) temp2 in 
  let pointed_ones = Image.image (fun (l2,opt)->Option.unpack opt) good_temp2 in 
  let temp4 = Image.image  fst bad_temp2 in 
  EP(l,bound+1,pointed_ones,temp4) ;; 

let all_expanded_moves = Image.image expand_move all_moves ;;  

let write_es_to_mathjax i m = 
    (soi i)^ " & " ^(soi m)^ " & \\textrm{Simplest case}" ;;
    
let intlist_to_mathjax l = "\\lbrace "^(String.concat "," (Image.image soi l))^" \\rbrace";;

let pointed_one_to_mathjax (head,passive) =
   let passive_component = (
     if passive = [] 
     then "" 
     else "|"^(String.concat "," (Image.image soi passive))) in  
     "["^(soi head)^passive_component^"]" ;; 

let pointed_ones_to_mathjax pointed_ones = 
  String.concat "" (Image.image pointed_one_to_mathjax pointed_ones) ;;

let helpers_to_mathjax helpers = 
  String.concat "" (Image.image intlist_to_mathjax helpers) ;;

let write_ep_to_mathjax l m pointed_ones helpers = 
      (intlist_to_mathjax l)^ " & " ^(soi m)^ " & "^ 
      (pointed_ones_to_mathjax pointed_ones)^(helpers_to_mathjax helpers) ;;    
    
let write_expanded_move_to_mathjax = function 
 ES(i,m) -> write_es_to_mathjax i m 
 |EP(l,m,pointed_ones,helpers) -> write_ep_to_mathjax l m pointed_ones helpers ;;

let backslash = "\\" ;;
let double_backslash = backslash ^ backslash ;;

 let write_expanded_move_to_mathjax_line x = 
    (write_expanded_move_to_mathjax x)^double_backslash^"\n\\hline" ;;

let array_in_mathjax l =
   String.concat "\n"
    (
    "\\begin{array}{|r|c|l|}\n\\hline"::
    (" A & \\mu & \\textrm{Explanations}"^double_backslash^"\n\\hline")::  
     (Image.image write_expanded_move_to_mathjax_line l)@ 
    [
    "\\end{array}"
    ]
    ) ;;

let max_part_size = 15 ;;

let parts = Listennou.cut_into_small_parts all_expanded_moves max_part_size ;;

let prelude ="/////////////////////////////////////////////////////////\nQuestion : \n/////////////////////////////////////////////////////////\n\n" ;;

let arrays_in_mathjax = prelude ^ (String.concat "\n\n\n" (Image.image array_in_mathjax parts)) ;; 

let the_ap = Absolute_path.of_string "~/Teuliou/Bash_scripts/example_maath.txt";;

let act () = Io.overwrite_with the_ap arrays_in_mathjax  ;;

(*

#use "Fads/nap.ml";;

*)

(************************************************************************************************************************
Snippet 78 : Transfer a large snippet from one file to another
************************************************************************************************************************)

open Needed_values ;;

let z1 = rf "Fads/nap.ml" ;;
let z2 = Lines_in_string.interval z1 209 298 ;;
let ap = Absolute_path.of_string "Fads/pan.ml" ;;
let z3 () = Io.Private.append_string_to_file z2 ap ;;

type ps = P of int list |S of int ;;

let accu = ref [] ;;

let simplest_case i x = (accu:=(S i)::(!accu)) ;;
let pointed_card a b c = (accu:=(P a)::(!accu)) ;;

(************************************************************************************************************************
Snippet 77 : Third stab at boundary operator combinatorics 
************************************************************************************************************************)

open Needed_values ;;

let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;
let j_order = Total_ordering.product i_order il_order ;;

let i_fold_merge = Ordered.fold_merge i_order ;;
let i_intersection = Ordered.intersect i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_mem = Ordered.mem i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;

let il_sort = Ordered.sort il_order ;;
let il_setminus = Ordered.setminus il_order ;;


let j_merge = Ordered.merge j_order ;;

type set_index = S of int ;;

let n1 = 4 ;;
let whole = Int_range.range 1 n1 ;;
let u1 = il_sort (Listennou.power_set whole) ;;
let rtl l = List.rev (List.tl l);;
let u2 = rtl (rtl u1);;
let normal_form x = 
     let cx = i_setminus whole x in 
     if (il_order x cx)=Total_ordering_result_t.Lower 
     then x else cx ;;
let u3 = List.filter (fun x->normal_form(x)=x) u2;;      
let u4 = Int_range.index_everything u3 ;;
let u5 = Image.image (fun (j,z)->(z,S(j+2))) u4 ;;
let u6 = Image.image (fun x->let nx=normal_form x in (x,List.assoc nx u5)) u2;;


module Atom = struct 

type t = A of int ;;

let order = ((fun (A i) (A j)->Total_ordering.for_integers i j) : t Total_ordering_t.t) ;; 
  
let table_for_sets_containing_a_given_atom = 
    let temp = Image.image (Image.image (fun i->S i)) 
    (il_sort(Listennou.power_set (Int_range.range 1 9))) in 
    Image.image (fun (j,l)->(A j,l)) (Int_range.index_everything temp) ;;
let all = Image.image fst table_for_sets_containing_a_given_atom ;;

let check_boolean_constraints constraints atm =
    let associated_sets = List.assoc atm table_for_sets_containing_a_given_atom in 
    List.for_all (fun (set,bowl)->(List.mem set associated_sets)=bowl) constraints ;;

let unveil (A i) = i ;;

end ;;  

module Early_atom = struct 

type t = EA of int ;;
  
let order = ((fun (EA i) (EA j)->Total_ordering.for_integers i j) : t Total_ordering_t.t) ;; 
    
let table_for_sets_containing_a_given_early_atom = 
      let temp = Image.image (Image.image (fun i->S i)) 
      (il_sort(Listennou.power_set (Int_range.range 1 2))) in 
      Image.image (fun (j,l)->(EA j,l)) (Int_range.index_everything temp) ;;
let all = Image.image fst table_for_sets_containing_a_given_early_atom ;;

let to_boolean_combination atm = 
  let associated_sets = List.assoc atm table_for_sets_containing_a_given_early_atom in 
   Image.image (fun s->(s,List.mem s associated_sets)) [S 1;S 2] ;;

let unveil (EA i) = i ;;

end ;;  

module Early_union = struct 

let order = Total_ordering.silex_compare  Early_atom.order ;; 
let whole = Int_range.scale (fun j->Early_atom.EA j)  1 4;;
let all = Image.image (Image.image (fun j->Early_atom.EA j)) u2 ;;
let all_pairs = Uple.list_of_pairs all ;;
let nondisjoint_pairs =
    List.filter (fun (a,b)->
       not(Ordered.does_not_intersect Early_atom.order a b)
      ) all_pairs ;;
let normal_form x = 
    let cx = Ordered.setminus Early_atom.order whole x in 
    if (order cx x)=Total_ordering_result_t.Lower 
    then cx else x ;;
let all_normal_forms = Image.image normal_form all ;;    
let table_for_image_sets = Image.image (fun x->
  (x,S(2+(Listennou.find_index (normal_form x) all_normal_forms))) 
) all ;;
let image_set x = List.assoc x table_for_image_sets ;;
let intersection x y = Ordered.intersect Early_atom.order x y;;
let union x y = Ordered.merge Early_atom.order x y;;

let unveil l = Image.image Early_atom.unveil l;;

end ;;  



module Molecule = struct 

type t = M of Atom.t list ;;  

let fold_union pre_ll = 
    let ll =  Image.image (fun (M m)-> m) pre_ll in 
    M (Ordered.fold_merge Atom.order ll);;
let intersection (M x) (M y) = M (Ordered.intersect Atom.order x y);;
let setminus (M x) (M y) = M (Ordered.setminus Atom.order x y);;
let union (M x) (M y) = M (Ordered.merge Atom.order x y);;


let delta mx my = union (setminus mx my) (setminus my mx) ;; 

let of_set set_idx =
    M( Option.filter_and_unpack (
       fun (atm_idx,l)->if List.mem set_idx l then Some atm_idx else None
    ) Atom.table_for_sets_containing_a_given_atom ) ;;

let complement_of_set set_idx = 
  M( Option.filter_and_unpack (
    fun (atm_idx,l)->if not(List.mem set_idx l) then Some atm_idx else None
 ) Atom.table_for_sets_containing_a_given_atom ) ;;

let of_boolean_combination constraints =
  M(Option.filter_and_unpack (
    fun (atm_idx,l)->if Atom.check_boolean_constraints constraints atm_idx then Some atm_idx else None
 ) Atom.table_for_sets_containing_a_given_atom) ;;

let of_early_atom eatm = 
  of_boolean_combination (Early_atom.to_boolean_combination eatm) ;;

let of_early_union eatm_l = fold_union (Image.image of_early_atom eatm_l) ;;
    
let of_early_union_image eatm_l = of_set (Early_union.image_set eatm_l) ;;


let main_test (pre_a,pre_b) =
    let a = of_early_union pre_a 
    and b = of_early_union pre_b in 
    let pre_c = Early_union.intersection pre_a pre_b 
    and c = intersection a b in 
    let hand1 = intersection c (of_early_union_image pre_c)
    and hand2 = intersection c (union (of_early_union_image pre_a)  (of_early_union_image pre_b)) in 
    delta hand1 hand2 ;;

let unveil (M l) = Image.image Atom.unveil l ;; 

end ;;  



let z1 = Early_union.nondisjoint_pairs ;;
let veiled_z2 = Image.image (fun (a,b)->((a,b),Molecule.main_test (a,b)) ) z1 ;;
let z2 =  Image.image (fun ((a,b),m)->(Early_union.unveil a,Early_union.unveil b,Molecule.unveil m) ) veiled_z2 ;;   

let aa = Image.image (fun x->Early_atom.EA x) [1;2] ;;
let bb = Image.image (fun x->Early_atom.EA x) [1;3] ;;
let ab = Early_union.union aa bb ;;

let faa = Molecule.of_early_union_image aa ;;
let fbb = Molecule.of_early_union_image bb ;;
let fab = Molecule.of_early_union_image ab ;;

let veiled_tab = Molecule.setminus faa (Molecule.union (Molecule.of_early_union ab) fab) ;;
let tab = Molecule.unveil veiled_tab ;;
let check_tab  = i_setminus tab (i_fold_merge(Image.image (fun (a,b,m)->m) z2));;

let z3 = Image.image (fun (a,b,m)->(a,b,i_intersection m tab)) z2 ;;
let get (a0,b0) = Option.unpack(Option.find_and_stop (fun (a,b,m)->if (a,b)=(a0,b0) then Some(m) else None) z3) ;;
let part1 = (get ([4],[3;4]));;
let tab2 = i_setminus tab part1 ;;

let z4 = z3 ;;
let v1 = List.tl(il_sort(Image.image (fun (a,b,m)->m) z4)) ;;
let v2 = List.rev v1 ;;

let v3 = List.hd v2 ;;
let v4 = Option.filter_and_unpack (fun (a,b,m)->if m=v3 then Some(a,b) else None) z4 ;;
(*

let veiled_tab = Molecule.setminus fab (Molecule.union faa fbb) ;;
let tab = Molecule.unveil veiled_tab ;;

let check_tab  = i_setminus tab (i_fold_merge(Image.image (fun (a,b,m)->m) z2));;

let z3 = Image.image (fun (a,b,m)->(a,b,i_intersection m tab)) z2 ;;
let get (a0,b0) = Option.unpack(Option.find_and_stop (fun (a,b,m)->if (a,b)=(a0,b0) then Some(m) else None) z3) ;;
let part1 = i_merge (get ([1;2],[1;2;3])) (get ([1;3],[1;2;3]));;
let tab2 = i_setminus tab part1 ;;

let z4 = Image.image (fun (a,b,m)->(a,b,i_intersection m tab2)) z3 ;;

let v1 = List.tl(il_sort(Image.image (fun (a,b,m)->m) z4)) ;;
let v2 = List.rev v1 ;;

let v3 = List.hd v2 ;;
let v4 = Option.filter_and_unpack (fun (a,b,m)->if m=v3 then Some(a,b) else None) z4 ;;
*)


(************************************************************************************************************************
Snippet 76 : Second stab at boundary operator combinatorics 
************************************************************************************************************************)

let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;
let j_order = Total_ordering.product i_order il_order ;;

let i_fold_merge = Ordered.fold_merge i_order ;;
let i_intersection = Ordered.intersect i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;

let il_sort = Ordered.sort il_order ;;
let il_setminus = Ordered.setminus il_order ;;


let j_merge = Ordered.merge j_order ;;

let n1 = 4 ;;
let whole = Int_range.range 1 n1 ;;
let u1 = il_sort (Listennou.power_set whole) ;;
let rtl l = List.rev (List.tl l);;
let u2 = rtl (rtl u1);;
let normal_form x = 
     let cx = i_setminus whole x in 
     if (il_order x cx)=Total_ordering_result_t.Lower 
     then x else cx ;;
let u3 = List.filter (fun x->normal_form(x)=x) u2;;      
let u4 = Int_range.index_everything u3 ;;
let u5 = Image.image (fun (j,z)->(z,[j+4])) u4 ;;
let u6 = Image.image (fun x->let nx=normal_form x in (x,(x,List.assoc nx u5))) u2;;
let u7 = Uple.list_of_pairs u2 ;;
let u8 = List.filter (fun (a,b)->(i_intersection a b)<>[]) u7;;
let u9 = Int_range.index_everything u8 ;;

module Kafka = struct

type t = {
  size : int ; 
  atoms : int list ;
  decompositions : (int * (int list)) list ;
  images : ((int list) * ((int list) * (int list))) list ;
} ;;


let share_with_foreigner kfk x = 
   let n = kfk.size 
   and m = List.length(kfk.atoms) in
   let temp1 = Int_range.index_everything kfk.atoms in  
   let decs1 = Image.image (fun (j,atm)->(atm,[n+j;n+j+m])) temp1
   and expansion_for_x = Int_range.range (n+1) (n+m) in 
   let replacer = (fun z->
      if z=x then expansion_for_x else 
      try List.assoc z decs1 with _ -> [z]
    ) in 
   let replacer2 = (fun lz->
    i_fold_merge (Image.image replacer lz)
    ) in  
   let decs2 = Image.image (fun (w,old_decomposition)->
     (w,replacer2 old_decomposition) 
    ) kfk.decompositions in  
   let new_atoms = Int_range.range (n+1) (n+2*m)
   and new_decompositions = j_merge [x,expansion_for_x] (j_merge decs1 decs2) 
   and new_images = Image.image (
     fun (old_a,(a,b)) -> (old_a,(replacer2 a,replacer2 b))
   ) kfk.images in 
   {
    kfk with 
    atoms = new_atoms ;
    decompositions = new_decompositions ;
    images = new_images ;
  } ;; 

let start =   
  {
    size = 100 ;
    atoms = [1; 2; 3; 4] ;
    decompositions = [] ;
    images = u6 ;
  } ;; 

  let expand kfk l = 
    let replacer = (fun z->
      match List.assoc_opt z kfk.decompositions with 
      Some(old_answer) -> old_answer
      | None -> [z]
    ) in 
    i_fold_merge (Image.image replacer l) ;;


  let get_image kfk l = try snd(List.assoc l kfk.images) with _ -> [] ;;

  let is_fully_decomposed kfk l = i_is_included_in l kfk.atoms ;; 

  let nondecomposed_elements kfk l = i_setminus l kfk.atoms ;; 

let declare_empty kfk zeroes =
  let remaining_atoms = i_setminus kfk.atoms zeroes in 
  let m = List.length remaining_atoms 
  and n = kfk.size in 
  let interval = Int_range.range (n+1) (n+m) in 
  let table = List.combine remaining_atoms interval in 
  let cleanup = (fun x->Image.image (fun t->
      try List.assoc t table with _ ->t) (i_setminus x zeroes)) in   
  let new_decompositions = Image.image (fun (x,dx)->(x,cleanup dx)) kfk.decompositions 
  and new_images = Image.image (
    fun (old_a,(a,b)) -> (old_a,(cleanup a,cleanup b))
  ) kfk.images in 
  {
   kfk with 
   atoms = interval ;
   decompositions = new_decompositions ;
   images = new_images ;
 } ;; 
 

end ;;


module Haddock = struct 

type t = 
   S1 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list)  
 | S2 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) 
 | S3 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) 
 | S4 of int * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) * (int list) * (int list)  

;;  

type road =
   Decompose of (int list) * (int list)
  |Declare_empty of (int list) * (int list) ;;

let is_unfinished = function 
 S4(idx,a,b,anb,g_anb,g_a,g_b,d1,d2) -> (d1,d2) <> ([],[])
|_ -> true ;;

let road_to_go  kfk haddock = match haddock with 
  S1(idx,a,b,anb,f_anb,f_a,f_b) ->
       Decompose(
          (Kafka.nondecomposed_elements kfk anb),  
          (Kafka.nondecomposed_elements kfk f_anb)) 
  |S2(idx,a,b,anb,g_anb,f_a,f_b) -> 
    Decompose(Kafka.nondecomposed_elements kfk f_a,[]) 
  |S3(idx,a,b,anb,g_anb,g_a,f_b) -> 
    Decompose(Kafka.nondecomposed_elements kfk f_b,[]) 
  |S4(idx,a,b,anb,g_anb,g_a,g_b,d1,d2) -> Declare_empty(d1,d2) ;;

let improve_a_little_bit  kfk haddock = match haddock with 
  S1(idx,a,b,anb,f_anb,f_a,f_b) ->
       if (Kafka.is_fully_decomposed kfk anb) &&  (Kafka.is_fully_decomposed kfk f_anb) 
       then S2(idx,a,b,anb,i_intersection anb f_anb,f_a,f_b) 
       else haddock
  |S2(idx,a,b,anb,g_anb,f_a,f_b) -> 
    if (Kafka.is_fully_decomposed kfk f_a) 
      then S3(idx,a,b,anb,g_anb,i_intersection anb f_a,f_b) 
      else haddock
  |S3(idx,a,b,anb,g_anb,g_a,f_b) -> 
        if (Kafka.is_fully_decomposed kfk f_b) 
          then let g_b = i_intersection anb f_b in 
               let g_ab = i_merge g_a g_b in 
               S4(idx,a,b,anb,g_anb,g_a,g_b,i_setminus g_anb g_ab,i_setminus g_ab g_anb) 
          else haddock    
  |S4(idx,a,b,anb,g_anb,g_a,g_b,d1,d2) -> haddock ;;

let improve kfk haddock = 
    let rec tempf = (fun old_x ->
       let new_x=improve_a_little_bit  kfk old_x in 
       if new_x = old_x then old_x else tempf new_x
    ) in 
    tempf haddock ;;

let initial_individual kfk (idx,(a,b)) = 
  let c =  i_intersection a b in 
  S1(idx,a,b,Kafka.expand kfk c,Kafka.get_image kfk c,Kafka.get_image kfk a,Kafka.get_image kfk b) ;;
  
let individual kfk (a,b) = 
   let candidate = improve kfk (initial_individual kfk (a,b)) in 
   if is_unfinished candidate 
   then Some (candidate,road_to_go kfk candidate)
   else None ;;

let total kfk = 
   Option.find_and_stop (individual kfk) u9 ;;


end ;;  

module This_kafka = struct 

  let main_ref = ref Kafka.start ;;

  let share x = 
     let new_kafka = Kafka.share_with_foreigner (!main_ref) x in 
     let _ = (main_ref:=new_kafka) in 
     new_kafka ;;

  let atoms () = (!main_ref).Kafka.atoms ;;

  let expand  = Kafka.expand (!main_ref) ;;

  let final_haddock () = Haddock.total (!main_ref) ;;

  let declare_empty zeroes = 
    let new_kafka = Kafka.declare_empty (!main_ref) zeroes in 
    let _ = (main_ref:=new_kafka) in 
    new_kafka ;;

  let act = function 
    Haddock.Decompose(d1,d2) -> let d = List.hd (d1@d2) in share d 
    |Declare_empty(e1,e2) -> declare_empty (i_merge e1 e2) ;;

end ;;  

module That_kafka = struct 

let share x = let _ = This_kafka.share x in snd(Option.unpack(This_kafka.final_haddock ())) ;;
let declare_empty x = let _ = This_kafka.declare_empty x in snd(Option.unpack(This_kafka.final_haddock ())) ;;

let act road = let _ = This_kafka.act road in  This_kafka.final_haddock () ;;

end ;;  



let rec iterator preceding_moves = match preceding_moves with 
   [] -> failwith("pusher_exn") 
  |(preceding_move,_) :: _->
      (match That_kafka.act preceding_move with 
      None -> List.rev preceding_moves 
    |Some(candidate,road) -> 
      iterator((road,!(This_kafka.main_ref))::preceding_moves) );;
  
let z1 = [(Haddock.Decompose([5],[])),Kafka.start]  ;;
let z2 = iterator z1 ;;   
let (z3,kfk1) = List.hd(List.rev z2) ;;   
let kfk1 = (!(This_kafka.main_ref))
let im = Kafka.get_image kfk1 ;;

let v1 = Uple.list_of_pairs u2 ;;
let v2 = List.filter (
   fun (a,b) ->
     let c = i_merge a b in 
     let d = i_setminus (im c) (i_merge (im a) (im b)) in 
     d<>[]
) v1 ;;



(************************************************************************************************************************
Snippet 75 : First stab at boundary operator combinatorics 
************************************************************************************************************************)

let i_order = Total_ordering.for_integers ;;
let il_order = Total_ordering.silex_compare i_order ;;

let i_merge = Ordered.merge i_order ;;
let i_intersection = Ordered.intersect i_order ;;
let i_is_included_in = Ordered.is_included_in i_order ;;


let il_sort = Ordered.sort il_order ;;


let u1 = il_sort (Listennou.power_set [1;2;3]) ;;
let u2 = Uple.list_of_pairs u1 ;;
let u3 = List.filter (fun (x,y)->i_intersection x y <> []) u2 ;;

let table_for_ff = 
  [[],0; [1],1; [2],2; [3],3; [1; 2],3; [1; 3],2; [2; 3],1; [1; 2; 3],0] ;;
let ff x = List.assoc x table_for_ff  ;;

let u4 = Image.image (fun (a,b)->
  let c= i_intersection a b in 
  (a,b,(c,ff c,ff a, ff b))) u3;;

let tf k = List.nth u4 (k-1) ;;  

let v1 = Cartesian.cube u1 ;;
let haddock k a b c = ((i_intersection k a) = (i_intersection k (i_merge b c)));;
let main_test (x1,x2,x3) =
    (haddock [1] x1 x2 x3) && (haddock [2] x2 x3 x1) && (haddock [3] x3 x1 x2) ;;      
let v2 = List.filter main_test v1 ;;
let v3 = List.filter (fun (x1,x2,x3) -> not(i_is_included_in x3 (i_merge x1 x2))) v2;;



(************************************************************************************************************************
Snippet 74 : Preprocess some PARI/GP code
************************************************************************************************************************)

open Needed_values ;;

let n1 = 5 ;;
let m1 = ((n1-1) * (n1-2)) / 2;;
let u1 = Int_range.scale (fun x->[1;0]) 1 n1 ;;
let u2 = Cartesian.general_product u1 ;;
let u3 = Int_range.index_everything u2 ;;
let ts l= 
   String.concat "+" (Image.image (fun j->"t"^(string_of_int j)) l);;

let s1 i = (ts(Option.filter_and_unpack (fun (idx,l)->
    if (List.nth l (i-1) = 1) 
    then Some(idx)
    else None    
  ) u3)) ^ "-" ^(string_of_int m1);;
let s2 (i,j) = (ts(Option.filter_and_unpack (fun (idx,l)->
    if (List.nth l (i-1) = 1) && (List.nth l (j-1) = 1)
    then Some(idx)
    else None    
) u3)) ^ "-" ^(string_of_int (n1-2));;

let part1 = Int_range.scale s1 1 n1 ;;
let part2 = Image.image s2 (Int_uple.list_of_pairs n1);;
let whole = "\n\n\n[" ^ (String.concat "," (part1@part2)) ^ "]\n\n\n"  ;;
let pw () = print_string whole ;;

Ordered.setminus Total_ordering.for_integers (Int_range.range 1 32)
[1; 2; 3; 4; 5; 6; 7; 9; 10; 11; 13; 17; 18; 19; 21; 25; 32] ;;


(************************************************************************************************************************
Snippet 73 : Research of flexible permutation groups
************************************************************************************************************************)


let i_order = Total_ordering.for_integers ;;

let s_order = Total_ordering.lex_for_strings ;;
let si_order = Total_ordering.product s_order i_order ;;
let p_order = Total_ordering.product si_order i_order ;;
let o_order = Total_ordering.silex_compare p_order ;;

let i_insert = Ordered.insert i_order ;;
let i_merge = Ordered.merge i_order ;;
let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;

let o_merge = Ordered.merge o_order ;;

let p_insert = Ordered.insert p_order ;;
let p_is_included_in = Ordered.is_included_in p_order ;;
let p_mem = Ordered.mem p_order ;;
let p_sort = Ordered.sort p_order ;;

let s_sort = Ordered.sort s_order ;;


module Initial_data = struct 

   let current_size = 3 ;;
   let base = Int_range.scale (fun t->
          let c= char_of_int (64+t) in 
          ((String.make 1 c,t),current_size+t)
         ) 1 current_size ;; 
   let basic_vars = Int_range.scale (fun t->
      String.make 1 (char_of_int (64+t))
     ) 1 current_size ;;        
   let sphere = Memoized.recursive (fun old_f j->
      if j=0 then [""] else 
      let temp1 = Cartesian.product basic_vars (old_f (j-1)) in 
      Image.image (fun (x,y)->x^y) temp1
   ) ;;
   let vars = List.flatten (Int_range.scale sphere 1 5) ;;
   
end ;;    

module Obstruction_list = struct 

type t = OL of (((string * int) * int) list) list ;; 

let reunite shortened same_length = 
  let temp1 = List.filter (fun y->
     List.for_all (fun x->not(p_is_included_in x y)) shortened) same_length in 
  o_merge shortened temp1 ;;

let take_note_of_new_condition (OL l) point =
   let ((s,i),j) = point in 
   let rec tempf = (fun (same_length,shortened,to_be_treated)->
      match to_be_treated with 
      [] -> OL(reunite (List.rev shortened) (List.rev same_length)) 
      | obstr :: other_obstrs ->
         let (temp1,temp2) = List.partition (fun (pair,_)->pair=(s,i) ) obstr in
         if temp1 = []
         then tempf(obstr::same_length,shortened,other_obstrs)    
         else 
         let j1 = snd(List.hd temp1) in 
         if j1 =j 
         then tempf(same_length,temp2::shortened,other_obstrs)     
         else tempf(same_length,shortened,other_obstrs)   
   ) in 
   tempf([],[],l) ;;

let singletons (OL l)= 
   Option.filter_and_unpack (fun obstr -> 
        if List.length obstr =1 
        then Some(List.hd obstr)
      else None
   ) l;;

let removables (OL l) y= List.filter (p_is_included_in y) l ;;

let main_list = OL [ 

   [
      (("A", 2), 1); (("B", 1), 1); (("C", 1), 1); (("A", 4), 2); (("A", 5), 3);
      (("C", 2), 2); (("B", 4), 2); (("B", 5), 3);  (("B", 3), 4)
      (* makes ABBC win *)
   ];
   [
      (("A", 2), 1); (("B", 1), 1); (("C", 1), 1); (("A", 4), 2); (("A", 5), 3);
      (("C", 2), 2); (("B", 5), 3); (("A", 3), 5); (("B", 3), 6); (("B", 6), 4); 
      (("C", 4), 3); (("C", 5), 4); (("C", 6), 5)
      (* makes ACBB win *)
   ];
   
   [
      (("A", 2), 1); (("B", 1), 1); (("A", 4), 2); (("A", 5), 3);
      (("A", 3), 5); (("B", 3), 6); (("C", 4), 3); (("A", 6), 6); (("C", 6), 7);
      (* makes ACAB win *)
   ];   

] ;;

end ;;    





module Bough = struct 

type t = {
   size : int ;
   points : ((string * int) * int) list ;
   history : ((string * int) * int) list ;
   forbidden : Obstruction_list.t ;
} ;; 

exception Forbidden_insertion of (string * int) * int ;;

let insert_point bough point =
   let ((s,i),j) = point in 
   let old_constraints = bough.forbidden in 
   let immediate_constraints = Obstruction_list.singletons old_constraints in 
   if List.mem point immediate_constraints 
   then raise(Forbidden_insertion((s,i),j))   
   else  
   {
      size = max (bough.size) (max i j);
      points = p_insert point (bough.points);
      history = point :: (bough.history);
      forbidden = Obstruction_list.take_note_of_new_condition old_constraints point ;
   }

exception Already_assigned of string * int ;;

let expand bough (s0,i0) =
   let unordered_temp1 = Option.filter_and_unpack (
        fun ((s,i),j)->
          if s=s0 
          then (if i=i0 
               then raise(Already_assigned(s0,i0))
               else Some j) 
          else None    
   ) bough.points in 
   let already_reached1 = i_sort unordered_temp1 
   and immediate_constraints = Obstruction_list.singletons bough.forbidden in 
   let useful_immediate_constraints = i_sort(Option.filter_and_unpack 
    (fun (pair,j)->if pair=(s0,i0) then Some j else None) immediate_constraints)  in 
   let already_reached = i_merge already_reached1 useful_immediate_constraints 
   and new_whole = Int_range.range 1 (bough.size+1) in 
   let exits = i_setminus new_whole already_reached in 
   Image.image (
       fun j -> insert_point bough ((s0,i0),j) 
   ) exits ;;

let smooth_expand bough (s0,i0) = try expand bough (s0,i0) with
   Already_assigned(_,_) -> [bough] ;;

let eval_list bough =
    let rec tempf=( fun (l,i0) -> match l with 
      [] -> (Some i0,None)
     |s0::others ->
       match Option.seek (
       fun (pair,j)-> pair = (s0,i0)
       ) bough.points with 
       Some(_,j0) -> tempf(others,j0)
       |None -> (None,Some(s0,i0))
    ) in 
    tempf ;; 

let expand_long_string long_string =
   let n = String.length long_string in 
  Int_range.scale (fun j->String.make 1 (String.get long_string (n-j))) 1 n ;;

let eval bough long_string i =
    let l = expand_long_string long_string in 
    eval_list bough (l,i) ;;
    
let force_eval bough long_string i =
     let (opt_good,_) = eval bough long_string i in Option.unpack opt_good ;;

let full_shadow bough long_string =
     let temp1 = Int_range.scale (
        fun j->(j,eval bough long_string j)
     ) 1 Initial_data.current_size in 
     let (good_temp1,bad_temp1) = List.partition (
        fun (j,(opt_good,opt_bad)) -> opt_bad = None 
     ) temp1 in 
     (Image.image ( fun (j,(opt_good,opt_bad)) ->(j,Option.unpack opt_good) ) good_temp1,
     Image.image ( fun (j,(opt_good,opt_bad)) ->(j,Option.unpack opt_bad) ) bad_temp1 );;

   ;;

let to_string bough =
    let all_points = bough.points in 
    let vars_involved = s_sort (Image.image (fun (pair,_)->fst pair) all_points) in 
    let temp1 = Image.image (
      fun s0->(s0,Option.filter_and_unpack (
          fun ((s,i),j)->
              if s=s0 then Some(i,j) else None
      ) all_points) 
    ) vars_involved in
    let temp2 = Image.image (
      fun (s,l)->(String.make 4 ' ')^ s^" : "^(String.concat ", " (Image.image (fun (i,j)->Printf.sprintf "%i -> %i" i j) l))
    )  temp1 in 
    String.concat "\n" temp2 ;;

let constructor l = {
   size = snd (Max.maximize_it  (fun ((s,i),j)-> max i j ) l) ;
   points = l ;
   history = [] ;
   forbidden = Obstruction_list.main_list ;
} ;;

let starting_point = constructor Initial_data.base ;;

let fold_construct l = List.fold_left insert_point starting_point l ;;


let is_stronger_than bough conditions =
   let conditions_in_order = p_sort conditions in 
   p_is_included_in  conditions_in_order bough.points ;;  

let find_chair_opt bough indexed_list =
   match Option.seek (fun (idx,conditions)->is_stronger_than bough conditions ) indexed_list with 
    None -> None
    |Some(idx0,_) -> Some idx0 ;;

exception Compute_long_chain_exn of (string * int) list ;;

let compute_long_chain bough long_string =
   let rec tempf=( fun (treated,to_be_treated,slice) -> match to_be_treated with 
   [] -> List.rev treated 
  |s0::others -> 
     let temp1 = Image.image (eval bough s0) slice in 
     let (good_temp1,bad_temp1) = List.partition (fun (good_opt,bad_opt)->good_opt<>None) temp1 in 
     if bad_temp1 <> []
     then let temp2 = Image.image (fun (good_opt,bad_opt)->Option.unpack bad_opt) bad_temp1 in 
          raise(Compute_long_chain_exn (temp2))
     else  
     let new_slice = Image.image (fun (good_opt,bad_opt)->Option.unpack good_opt) good_temp1 in 
     tempf(((s0,new_slice)::treated,others,new_slice))
 ) in 
 tempf(["Z",[1;2;3]],expand_long_string long_string,[1;2;3]) ;; 

let deduce_long_snake_from_long_chain bough long_chain =
   let temp2 = Listennou.universal_delta_list long_chain in 
   let temp3 = Image.image (fun ((actor1,acted1),(actor2,acted2)) ->(actor2,acted1) ) temp2 in 
   let temp4 = List.flatten (Image.image (fun (s,l)->Image.image (fun i->(s,i)) l) temp3) in 
   let temp5 = Image.image (fun (s,i)->((s,i),force_eval bough s i) ) temp4 in
   p_sort temp5 ;; 

let compute_long_snake bough long_string =
     let long_chain = compute_long_chain bough long_string in 
     deduce_long_snake_from_long_chain bough long_chain ;;

let snake_history bough long_string =
  let temp1 = compute_long_snake bough long_string 
  and bare_history = List.rev (bough.history) in
  List.filter (fun p->p_mem p temp1) bare_history ;; 
  
let cut_long_chain_in_one_place bough long_chain =
   let (_,final_result) = List.hd(List.rev long_chain) in 
   let idx_for_max = Listennou.find_index (Max.list final_result) final_result in 
   let part1 = Image.image (fun (actor,acted)->
        (actor,Listennou.complement_of_singleton acted idx_for_max)) long_chain
   and temp1 = Image.image (fun (actor,acted)->
      (actor,List.nth acted (idx_for_max-1))) long_chain in
   let temp2 = Listennou.universal_delta_list temp1 in 
   let part2 = Image.image (fun ((actor1,acted1),(actor2,acted2)) ->((actor2,acted1),acted2) ) temp2 in    
   (idx_for_max,deduce_long_snake_from_long_chain bough part1,part2) ;;

end ;;



module Analysis_on_bough = struct 

type result_on_var = 
   Uunfinished of string *int 
  |Finnished_well of  ((int*int) list) 
  |Finished_badly of  int * int ;;  

let analize_var bough long_string = 
   let (dead,alive) = Bough.full_shadow bough long_string in 
   (match Option.seek (fun (i,j)->j<=Initial_data.current_size) dead with 
    (Some(i0,j0)) -> Finished_badly (i0,j0)
   |None ->
      (
         match alive with 
         [] -> Finnished_well(dead)
         |(_,(s1,i1)) :: _ -> Uunfinished(s1,i1) 
      )) ;;
     
type result_on_vars =
   Unfinished of string * int 
  |Finished_well of string * ((int*int) list)
  |Inconclusive of (string * int * int) list ;;

let analize_vars bough vars=
     let rec tempf =(
        fun (treated,to_be_treated) -> match to_be_treated with 
        [] -> Inconclusive (List.rev treated)
        | long_string :: others -> 
          (match analize_var bough long_string with
         Uunfinished (s,i) ->  Unfinished (s,i)
        |Finnished_well (explanation) -> Finished_well (long_string,explanation)
        |Finished_badly (i,j) -> tempf((long_string,i,j)::treated,others)
          )
     )  in 
     tempf ([],vars) ;;

type check_result =
     Usual of ((int*int) list)
    |Special of int  ;; 


exception Unhappy_ending of string * result_on_var ;;

let check_happy_ending bough var = match analize_var bough var with 
     Finnished_well(explanation) -> Usual(explanation) 
   | other -> raise (Unhappy_ending(var,other)) ;;

end ;;   



module Bough_list = struct 

let analize l vars = Analysis_on_bough.analize_vars (List.hd l) vars ;;

let expand boughs (b_idx0,s0,i_idx0)=
  let indexed_boughs = Int_range.index_everything boughs in 
  let temp2 = Image.image (
     fun (b_idx,bough) -> 
       if b_idx = b_idx0 
       then Bough.expand bough (s0,i_idx0)
       else [bough]    
  ) indexed_boughs in 
  List.flatten temp2 ;;

let to_string l = 
   let temp1 = Int_range.index_everything l in 
   let temp2 = List.rev_map (fun (idx,bough)->"\nCase "^(string_of_int idx)^".\n"^(Bough.to_string bough)) temp1 in 
   let sn = string_of_int(List.length l) in 
   (String.concat "\n" temp2)^"\n "^sn^" cases." ;;


let expand_all_using_delayed_expression boughs (s1,delayer,i1) =
   let temp1 = Image.image (
      fun bough -> let i2=Bough.force_eval bough delayer i1  in 
                   Bough.smooth_expand bough (s1,i2)   
   ) boughs in 
   List.flatten temp1 ;;      


let expand_long_string long_string =
      let n = String.length long_string in 
     Int_range.scale (fun j->
      let ck = String.get long_string (n-j) in 
      (String.make 1  ck,Cull_string.ending (j-1) long_string) ) 1 n ;;
       

let expand_following_long_chain boughs (long_string,i1)  = 
  let expansions = Image.image (fun (s1,delayer)->
   (s1,delayer,i1) ) (expand_long_string long_string) in 
  List.fold_left expand_all_using_delayed_expression boughs expansions ;;    

let shadow boughs (long_string,i1)  = 
   let temp1 =  expand_following_long_chain boughs (long_string,i1)  in 
   let temp2 = Image.image (fun bough->Bough.force_eval bough long_string i1) temp1 in 
   i_sort temp2 ;;

let seek_overture (long_string,i1) l=
   let bough0 = Bough.fold_construct l in 
   let temp1 =  expand_following_long_chain [bough0] (long_string,i1)  in 
   Option.find_and_stop (fun bough->
       let t = Bough.force_eval bough long_string i1 in 
       if t > Initial_data.current_size 
       then Some(l,t)  
       else None   
   ) temp1 ;;  
 
let is_closed pair l = (seek_overture pair l=None) ;;

let minimal_version pair bough = 
   Listennou.minimal_element_in_unpwards_filter 
     (is_closed pair) (bough.Bough.history) ;;

end ;;   


module Seek_minimal_obstruction = struct 
   

end ;;   




module Walker = struct 
   
type t = W of ((string*int) list)*(Bough.t list) ;;

let expand (W(expansion_history,boughs)) triple = 
   let (b_idx,s,i) = triple in   
   W((s,i)::expansion_history,Bough_list.expand boughs triple) ;;

   let analize (W(_,boughs)) vars = Bough_list.analize boughs vars ;; 

exception Iter_expand_exn ;;

let rec iter_expand w vars =
   match analize w vars with 
   Analysis_on_bough.Finished_well(long_string,explanation) -> (w,long_string,explanation)
   |Unfinished(s,i) -> let new_w = expand w (1,s,i) in 
                       iter_expand new_w vars
   |Inconclusive(_) -> raise (Iter_expand_exn);; 


let to_string (W(expansion_history,boughs)) = Bough_list.to_string boughs ;;

let print_out  (fmt:Format.formatter) w = Format.fprintf fmt "@[%s@]" (to_string w);; 

let constructor l = W([],[Bough.constructor l]) ;;



let starting_point = constructor Initial_data.base ;;


end ;;   

module This_walker = struct 

let main_ref = ref (Walker.starting_point) ;;

let initialize () = let _ =(main_ref := Walker.starting_point )  in Walker.starting_point;;

let expand (s,i) = 
   let new_w = Walker.expand (!main_ref) (1,s,i) in 
   let _ = (main_ref := new_w) in 
   new_w ;;

let analize () = Walker.analize (!main_ref) Initial_data.vars ;;

let rec iter_expand () = 
   let (new_w,long_string,explanation) = Walker.iter_expand (!main_ref) Initial_data.vars  in 
   let _ = (main_ref := new_w) in 
   (new_w,long_string,explanation) ;;

let expand_and_analize pair = let next_state = expand pair in (next_state,analize ()) ;;


end ;;   

(* #install_printer Walker.print_out ;; *)

This_walker.initialize () ;;


let ie = This_walker.iter_expand  ;;


(*
ie();;
let (current_solver,end_result) = (match This_walker.analize () with 
Analysis_on_bough.Finished_well (x, y) -> (x,y) 
| _-> failwith("aaa")
);;
let (Walker.W(_,alive1))=(!(This_walker.main_ref)) ;;
let bough1 = List.hd alive1 ;;
let history1= List.rev(bough1.Bough.history) ;;
let lc1 = Bough.compute_long_chain bough1 current_solver ;;
let ls1 = Bough.snake_history bough1 current_solver ;;
let (idx1,unquestioned,questioned) =
   Bough.cut_long_chain_in_one_place bough1 lc1 ;;
let im1 = Bough_list.minimal_version (current_solver,idx1) bough1 ;;

#use"Fads/cloth.ml";;

*)



(************************************************************************************************************************
Snippet 72 : Transform a text in an Ocaml string 
************************************************************************************************************************)

let z1 = Needed_values.rf "Fads/nap.ml"  ;;
let z2 = Lines_in_string.interval z1 12 25 ;;
let z3 = Replace_inside.replace_inside_string ("\"","\\\"") z2;;
let z4 = Lines_in_string.lines z3 ;;
let z5 = Image.image (fun line -> "\"" ^ (Cull_string.trim_spaces line) ^ "\"") z4 ;; 
let z6 = "\n\n\n" ^ (String.concat ";\n" z5) ^ "\n\n\n" ;;
let z7 () = print_string z6 ;;

(************************************************************************************************************************
Snippet 71 : Find and replace on several files 
************************************************************************************************************************)

let reps_ref = ref [ ];;

reps_ref := [
  "Fw_with_githubbing.all_endinglesses","Fw_with_dependencies.all_endinglesses";
  "Fw_with_githubbing.all_ml_absolute_paths","Fw_with_dependencies.all_ml_absolute_paths";
  "Fw_with_githubbing.all_mlx_files","Fw_with_dependencies.all_mlx_files";
  "Fw_with_githubbing.ancestors_for_module","Fw_with_dependencies.ancestors_for_module";
  "Fw_with_githubbing.all_subdirectories","Fw_with_dependencies.all_subdirectories";
  "Fw_with_githubbing.below","Fw_with_dependencies.below";
  "Fw_with_githubbing.check_that_no_change_has_occurred","Fw_with_archives.check_that_no_change_has_occurred";
  "Fw_with_githubbing.configuration","Fw_with_githubbing.to_fw_configuration";
  "Fw_with_githubbing.decipher_module","Fw_with_dependencies.decipher_module";
  "Fw_with_githubbing.decipher_path","Fw_with_dependencies.decipher_path";
  "Fw_with_githubbing.dep_ordered_modules","Fw_with_dependencies.dep_ordered_modules";
  "Fw_with_githubbing.direct_fathers_for_module","Fw_with_dependencies.direct_fathers_for_module";
  "Fw_with_githubbing.directly_below","Fw_with_dependencies.directly_below";
  "Fw_with_githubbing.duplicate_module","Fw_with_dependencies.duplicate_module";
  "Fw_with_githubbing.endingless_at_module","Fw_with_dependencies.endingless_at_module";
  "Fw_with_githubbing.find_subdir_from_suffix","Fw_with_dependencies.find_subdir_from_suffix";
  "Fw_with_githubbing.gitpush_after_backup","Fw_poly.gitpush_after_backup";
  "Fw_with_githubbing.latest_changes","Fw_with_archives.latest_changes";
  "Fw_with_githubbing.modules_using_value","Fw_with_dependencies.modules_using_value";
  "Fw_with_githubbing.noncompilable_files","Fw_with_archives.noncompilable_files";
  "Fw_with_githubbing.of_concrete_object","Fw_poly.of_concrete_object";
  "Fw_with_githubbing.root","Fw_poly.root";
  "Fw_with_githubbing.set_gitpush_after_backup","Fw_poly.set_gitpush_after_backup";
  "Fw_with_githubbing.to_concrete_object","Fw_poly.to_concrete_object";
  "Fw_with_githubbing.usual_compilable_files","Fw_with_archives.usual_compilable_files";
] ;;

let files_ref = ref [] ;;

files_ref := [
  "Compilation_management/create_world_copy.ml";
  "Compilation_management/modify_coma_state.ml";
  "Compilation_management/needed_data_summary.ml";  
  "Compilation_management/usual_coma_state.ml";
  "Compilation_management/other_coma_state.ml";
  "Filewatching/fw_with_persisting.ml";
  "Ocaml_analysis/compute_all_ocaml_items.ml";
  "Ocaml_analysis/read_needed_ocaml_files.ml";
  "self_contained_module_copy.ml";
] ;;

let tr x = Replace_inside.replace_several_inside_file (!reps_ref) (Absolute_path.of_string( "../Idaho/"^x)) ;;

let act () = List.iter tr (!files_ref) ;;

(************************************************************************************************************************
Snippet 70 : Modifying line intervals in a file
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "../Idaho/Filewatching/fw_with_githubbing.ml" ;;
let text1 = Io.read_whole_file ap1 ;;
let (before1,old_center1,after1) = Lines_in_string.tripartition_associated_to_interval 
    text1 257 336 ;;
let new_center1 = Lines_in_string.remove_lines_containing_substring_in_string
  "shrinkable" old_center1 ;;
let new_text1 = String.concat "\n" [before1;new_center1;after1] ;;  
Io.overwrite_with ap1 new_text1 ;;

Lines_in_string.remove_interval_in_file ap1 169 253 ;;

(************************************************************************************************************************
Snippet 69 : Exercise on flexible transitive permutation groups, version 1
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let s_order = Total_ordering.lex_for_strings ;;
let si_order = Total_ordering.product s_order i_order ;;
let p_order = Total_ordering.product si_order i_order ;;

let i_setminus = Ordered.setminus i_order ;;
let i_sort = Ordered.sort i_order ;;

let p_insert = Ordered.insert p_order ;;
let p_is_included_in = Ordered.is_included_in p_order ;;
let p_mem = Ordered.mem p_order ;;
let p_sort = Ordered.sort p_order ;;

let s_sort = Ordered.sort s_order ;;


module Initial_data = struct 

   let current_size = 3 ;;
   let base = Int_range.scale (fun t->
          let c= char_of_int (64+t) in 
          ((String.make 1 c,t),current_size+t)
         ) 1 current_size ;; 
   let basic_vars = Int_range.scale (fun t->
      String.make 1 (char_of_int (64+t))
     ) 1 current_size ;;        
   let sphere = Memoized.recursive (fun old_f j->
      if j=0 then [""] else 
      let temp1 = Cartesian.product basic_vars (old_f (j-1)) in 
      Image.image (fun (x,y)->x^y) temp1
   ) ;;
   let vars = List.flatten (Int_range.scale sphere 1 5) ;;
   
end ;;    

module Bough = struct 

type t = {
   size : int ;
   points : ((string * int) * int) list ;
   history : ((string * int) * int) list ;
} ;; 

let insert_point point bough =
   let ((s,i),j) = point in 
   {
      size = max (bough.size) (max i j);
      points = p_insert point (bough.points);
      history = point :: (bough.history);
   }

exception Already_assigned of string * int ;;

let expand bough (s0,i0) =
   let unordered_temp1 = Option.filter_and_unpack (
        fun ((s,i),j)->
          if s=s0 
          then (if i=i0 
               then raise(Already_assigned(s0,i0))
               else Some j) 
          else None    
   ) bough.points in 
   let already_reached = i_sort unordered_temp1 
   and new_whole = Int_range.range 1 (bough.size+1) in 
   let exits = i_setminus new_whole already_reached in 
   Image.image (
       fun j -> insert_point ((s0,i0),j) bough
   ) exits ;;

let eval_list bough =
    let rec tempf=( fun (l,i0) -> match l with 
      [] -> (Some i0,None)
     |s0::others ->
       match Option.seek (
       fun (pair,j)-> pair = (s0,i0)
       ) bough.points with 
       Some(_,j0) -> tempf(others,j0)
       |None -> (None,Some(s0,i0))
    ) in 
    tempf ;; 

let expand_long_string long_string =
   let n = String.length long_string in 
  Int_range.scale (fun j->String.make 1 (String.get long_string (n-j))) 1 n ;;

let eval bough long_string i =
    let l = expand_long_string long_string in 
    eval_list bough (l,i) ;;
    
let force_eval bough long_string i =
     let (opt_good,_) = eval bough long_string i in Option.unpack opt_good ;;

let full_shadow bough long_string =
     let temp1 = Int_range.scale (
        fun j->(j,eval bough long_string j)
     ) 1 Initial_data.current_size in 
     let (good_temp1,bad_temp1) = List.partition (
        fun (j,(opt_good,opt_bad)) -> opt_bad = None 
     ) temp1 in 
     (Image.image ( fun (j,(opt_good,opt_bad)) ->(j,Option.unpack opt_good) ) good_temp1,
     Image.image ( fun (j,(opt_good,opt_bad)) ->(j,Option.unpack opt_bad) ) bad_temp1 );;

   ;;

let to_string bough =
    let all_points = bough.points in 
    let vars_involved = s_sort (Image.image (fun (pair,_)->fst pair) all_points) in 
    let temp1 = Image.image (
      fun s0->(s0,Option.filter_and_unpack (
          fun ((s,i),j)->
              if s=s0 then Some(i,j) else None
      ) all_points) 
    ) vars_involved in
    let temp2 = Image.image (
      fun (s,l)->(String.make 4 ' ')^ s^" : "^(String.concat ", " (Image.image (fun (i,j)->Printf.sprintf "%i -> %i" i j) l))
    )  temp1 in 
    String.concat "\n" temp2 ;;

let constructor l = {
   size = snd (Max.maximize_it  (fun ((s,i),j)-> max i j ) l) ;
   points = l ;
   history = [] ;
} ;;

let is_stronger_than bough conditions =
   let conditions_in_order = p_sort conditions in 
   p_is_included_in  conditions_in_order bough.points ;;  

let find_chair_opt bough indexed_list =
   match Option.seek (fun (idx,conditions)->is_stronger_than bough conditions ) indexed_list with 
    None -> None
    |Some(idx0,_) -> Some idx0 ;;

exception Compute_long_chain_exn of (string * int) list ;;

let compute_long_chain bough long_string =
   let rec tempf=( fun (treated,to_be_treated,slice) -> match to_be_treated with 
   [] -> List.rev treated 
  |s0::others -> 
     let temp1 = Image.image (eval bough s0) slice in 
     let (good_temp1,bad_temp1) = List.partition (fun (good_opt,bad_opt)->good_opt<>None) temp1 in 
     if bad_temp1 <> []
     then let temp2 = Image.image (fun (good_opt,bad_opt)->Option.unpack bad_opt) bad_temp1 in 
          raise(Compute_long_chain_exn (temp2))
     else  
     let new_slice = Image.image (fun (good_opt,bad_opt)->Option.unpack good_opt) good_temp1 in 
     tempf(((s0,new_slice)::treated,others,new_slice))
 ) in 
 tempf(["Z",[1;2;3]],expand_long_string long_string,[1;2;3]) ;; 

let compute_long_snake bough long_string =
     let temp1 = compute_long_chain bough long_string in 
     let temp2 = Listennou.universal_delta_list temp1 in 
     let temp3 = Image.image (fun ((actor1,acted1),(actor2,acted2)) ->(actor2,acted1) ) temp2 in 
     let temp4 = List.flatten (Image.image (fun (s,l)->Image.image (fun i->(s,i)) l) temp3) in 
     let temp5 = Image.image (fun (s,i)->((s,i),force_eval bough s i) ) temp4 in
     p_sort temp5 ;; 

let snake_history bough long_string =
  let temp1 = compute_long_snake bough long_string 
  and bare_history = List.rev (bough.history) in
  List.filter (fun p->p_mem p temp1) bare_history ;; 
  


end ;;

module Special_reductions = struct 

   let main_list = [
      [
      (("A", 2), 1); (("B", 1), 1); (("C", 1), 1); (("A", 4), 2); (("A", 5), 3);
      (("C", 2), 2); (("B", 4), 2); (("B", 5), 3); (("A", 3), 5); (("B", 3), 4);
       (* in this case ABBC is a solution *)
      ];
      [
         (("A", 2), 1); (("B", 1), 1); (("C", 1), 1); (("A", 4), 2); (("A", 5), 3);
         (("B", 5), 3); (("A", 3), 5); (("B", 3), 6);
         (("B", 6), 4); (("C", 4), 3); 
         (* in this case ACBB is a solution *)
      ];
      [
         (("A", 2), 1); (("B", 1), 1); (("C", 1), 1); (("A", 4), 2); (("A", 5), 3);
         (("C", 2), 2);  (("A", 3), 5); (("B", 3), 6); (("C", 4), 4); (("C", 5), 3); 
         (* in this case CAB is a solution *)
      ];
      [
         (* (("B", 1), 1); (("C", 1), 1); (("B", 5), 3); (("A", 3), 5); (("B", 3), 6);
         (("C", 5), 5); (("A", 6), 6); (("C", 6), 3) *)
         (("A", 2), 1); (("B", 1), 1); (("C", 1), 1); (("A", 4), 2); (("A", 5), 3);
         (("C", 2), 2); (("B", 4), 2); (("B", 5), 3); (("A", 3), 5); (("B", 3), 6);
         (("B", 6), 4); (("C", 4), 4); (("C", 5), 5)
       (* in this case ABCB is a solution *)
      ];
      [
         (("B", 1), 1); (("A", 5), 3); (("B", 3), 6); (("C", 4), 4); (("A", 6), 6);
         (("C", 6), 7) 
         (* in this case CAB is a solution *)
      ];
   ]  ;; 
   
   let indexed_main_list = Int_range.index_everything main_list ;;

   end ;;   


module Analysis_on_bough = struct 

type result_on_var = 
   Uunfinished of string *int 
  |Finnished_well of  ((int*int) list) 
  |Finnished_with_a_jump of int
  |Finished_badly of  int * int ;;  

let analize_var bough long_string = 
   match Bough.find_chair_opt bough Special_reductions.indexed_main_list with
   Some(idx0) -> Finnished_with_a_jump(idx0)
   |None ->   
   let (dead,alive) = Bough.full_shadow bough long_string in 
   (match Option.seek (fun (i,j)->j<=Initial_data.current_size) dead with 
    (Some(i0,j0)) -> Finished_badly (i0,j0)
   |None ->
      (
         match alive with 
         [] -> Finnished_well(dead)
         |(_,(s1,i1)) :: _ -> Uunfinished(s1,i1) 
      )) ;;
     
type result_on_vars =
   Unfinished of string * int 
  |Finished_well of string * ((int*int) list)
  |Finished_with_a_jump of int
  |Inconclusive of (string * int * int) list ;;

let analize_vars bough vars=
     let rec tempf =(
        fun (treated,to_be_treated) -> match to_be_treated with 
        [] -> Inconclusive (List.rev treated)
        | long_string :: others -> 
          (match analize_var bough long_string with
         Uunfinished (s,i) ->  Unfinished (s,i)
        |Finnished_well (explanation) -> Finished_well (long_string,explanation)
        |Finnished_with_a_jump idx -> Finished_with_a_jump idx
        |Finished_badly (i,j) -> tempf((long_string,i,j)::treated,others)
          )
     )  in 
     tempf ([],vars) ;;

type check_result =
     Usual of ((int*int) list)
    |Special of int  ;; 


exception Unhappy_ending of string * result_on_var ;;

let check_happy_ending bough var = match analize_var bough var with 
     Finnished_well(explanation) -> Usual(explanation) 
   | other -> raise (Unhappy_ending(var,other)) ;;

exception Not_a_jump of result_on_var ;;
exception Jump_index_mismatch of int * int ;;

let check_jump bough jump_idx= match analize_var bough "" with 
   Finnished_with_a_jump idx -> if idx=jump_idx 
                                then Special(idx) 
                                else raise(Jump_index_mismatch(jump_idx,idx)) 
 | other -> raise (Not_a_jump(other)) ;;   


end ;;   



module Bough_list = struct 

let to_string l = 
   let temp1 = Int_range.index_everything l in 
   let temp2 = List.rev_map (fun (idx,bough)->"\nCase "^(string_of_int idx)^".\n"^(Bough.to_string bough)) temp1 in 
   let sn = string_of_int(List.length l) in 
   (String.concat "\n" temp2)^"\n "^sn^" cases." ;;

let analize l vars = Analysis_on_bough.analize_vars (List.hd l) vars ;;

end ;;   





module Walker = struct 
   
type t = W of ((Bough.t * (string * Analysis_on_bough.check_result )) list) * (Bough.t list) ;;

let expand (W(dead,alive)) (b_idx0,s0,i_idx0)=
  let temp1 = Int_range.index_everything alive in 
  let temp2 = Image.image (
     fun (b_idx,bough) -> 
       if b_idx = b_idx0 
       then Bough.expand bough (s0,i_idx0)
       else [bough]    
  ) temp1 in 
  W(dead,List.flatten temp2) ;;

let kill  (W(dead,alive)) b_idx0 long_string =
   let temp1 = Int_range.index_everything alive in 
   let (the_one,others) = List.partition (fun (b_idx,bough)->b_idx = b_idx0) temp1 in 
   let (_,dead_bough) = List.hd the_one in  
   let explanation = Analysis_on_bough.check_happy_ending dead_bough long_string in 
   (W((dead_bough,(long_string,explanation))::dead,Image.image snd others)) ;;   

let jump  (W(dead,alive)) b_idx0 jump_idx=
   let temp1 = Int_range.index_everything alive in 
   let (the_one,others) = List.partition (fun (b_idx,bough)->b_idx = b_idx0) temp1 in 
   let (_,dead_bough) = List.hd the_one in  
   let explanation = Analysis_on_bough.check_jump dead_bough jump_idx in 
   (W((dead_bough,("",explanation))::dead,Image.image snd others)) ;;  


let to_string (W(dead,alive)) = Bough_list.to_string alive ;;

let print_out  (fmt:Format.formatter) w = Format.fprintf fmt "@[%s@]" (to_string w);; 

let constructor l = W([],[Bough.constructor l]) ;;

let analize (W(dead,alive)) vars = Bough_list.analize alive vars ;; 

let starting_point = constructor Initial_data.base ;;

let alcove (W(dead,alive)) = dead ;;

end ;;   

module This_walker = struct 

let main_ref = ref (Walker.starting_point) ;;

let initialize () = let _ =(main_ref := Walker.starting_point )  in Walker.starting_point;;

let expand (s,i) = 
   let new_w = Walker.expand (!main_ref) (1,s,i) in 
   let _ = (main_ref := new_w) in 
   new_w ;;
     
let kill long_string = 
   let new_w = Walker.kill (!main_ref) 1 long_string in 
   let _ = (main_ref := new_w) in 
   new_w ;;     

let jump jump_idx = 
   let new_w = Walker.jump (!main_ref) 1 jump_idx in 
   let _ = (main_ref := new_w) in 
   new_w ;;     
           
   

let analize () = Walker.analize (!main_ref) Initial_data.vars ;;

let alcove () = Walker.alcove (!main_ref) ;;

let expand_and_analize pair = let next_state = expand pair in (next_state,analize ()) ;;
let kill_and_analize data = let next_state = kill data in (next_state,analize ()) ;;
let jump_and_analize jump_idx = let next_state = jump jump_idx in (next_state,analize ()) ;;

end ;;   

(* #install_printer Walker.print_out ;; *)

This_walker.initialize () ;;


let ea = This_walker.expand_and_analize  ;;
let ka = This_walker.kill_and_analize ;;
let ja = This_walker.jump_and_analize ;;

ea("A",2);;
ea("B",1);;
ea("C",1);;
ea("A",4);;
ea("A",5);;
ea("C",2);;
ea("B",4);;
ea("B",5);;
ea("A",3);;
ea("B",3);;
ja 1;;
ea("B",6);;
ea("C",4);;
ja 2;;
ea("C",5);;
ja 3;;
ea("A",6);;
ea("C",6);;
ja 4;;
ja 4;;
ja 4;;
ea("A",7);;
ea("C",6);;
ka "ACB" ;;
ea("A",6);;
ea("C",7);;
ea("B",7);;
ka "ABCB" ;;
ea("A",8);;


(*

ja 5;;
ea("C",7);;
ea("C",6);;
ea("B",7);;
ea("A",7);;
*)


(*
let (Walker.W(dead1,alive1))=(!(This_walker.main_ref)) ;;
let bough1 = List.hd alive1 ;;
let history1= List.rev(bough1.Bough.history) ;;
let lc1 = Bough.compute_long_chain bough1 "ABCB" ;;
let ls1 = Bough.snake_history bough1 "ABCB" ;;


let g1_acb = [(("B", 1), 1); (("C", 1), 1); (("A", 3), 5); (("B", 3), 6); (("C", 5), 7);
   (("A", 7), 6); (("C", 6), 3)] ;;
let g2_abcb = [(("B", 1), 1); (("C", 1), 1); (("B", 5), 3); (("A", 3), 5); (("B", 3), 6);
   (("C", 5), 7); (("A", 7), 6); (("C", 6), 5); (("B", 7), 7)] ;;





This_walker.analize () ;;

*)



(************************************************************************************************************************
Snippet 68 : Enumerating subgroups of S4 
************************************************************************************************************************)
let i_order = Total_ordering.for_integers ;;
let i_sort  = Ordered.sort i_order ;;
let i_is_included_in = Ordered.is_included_in i_order;;

let il_order = Total_ordering.silex_compare  Total_ordering.for_integers ;;
let il_sort  = Ordered.sort il_order ;;

let current_order = 4 ;;
let base = Permutation.iii current_order ;;

let eval_list_permutation sigma k = List.nth sigma (k-1) ;;

let compose_list_permutations sigma1 sigma2 = 
   Int_range.scale (fun k-> eval_list_permutation sigma1 (eval_list_permutation sigma2 k)) 1 current_order ;;

let uncurried_compose = Memoized.make(fun (i,j) ->
   let sigma1 = List.nth base (i-1)   
   and sigma2 = List.nth base (j-1) in 
   Listennou.find_index (compose_list_permutations sigma1 sigma2) base
);;     

let compose  i j = uncurried_compose (i,j) ;;

let base_size = List.length base ;;

let subset_product l1 l2 =
    let temp1 = Cartesian.product l1 l2 in 
    let temp2 = Image.image uncurried_compose temp1 in 
    i_sort temp2 ;; 

let rec helper_for_generated_subgroup (treated,seed) = 
      let possibly_new = subset_product treated seed in 
      let really_new = Ordered.setminus Total_ordering.for_integers possibly_new treated in 
      if really_new = [] 
      then treated 
      else let new_whole = Ordered.merge Total_ordering.for_integers really_new treated in 
            helper_for_generated_subgroup (new_whole,seed) ;;
        
let generated_subgroup seed = helper_for_generated_subgroup ([1],seed) ;; 

let trivial_subgroup = [1] ;;
let full_subgroup = Int_range.range 1 base_size ;;

let level1  = 
  il_sort (Int_range.scale (fun k->generated_subgroup [k]) 2 base_size) ;; 

let pre_level2 = 
    let temp1 = Uple.list_of_pairs level1 in 
    let temp2 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp1 in 
    il_sort temp2 ;;

let (next_to_level2,level2) = List.partition (fun x->List.mem x level1) pre_level2 ;;

let pre_level3 = 
  let temp1 = Cartesian.product level1 level2 in 
  let temp2 = List.filter (fun (x,y)->not(i_is_included_in x y)) temp1 in 
  let temp3 = Image.image (fun (a,b)->generated_subgroup(a@b)) temp2 in 
  il_sort temp3 ;;

let (next_to_level3,level3) = List.partition (fun x->List.mem x level2) pre_level3 ;;  

let halves x =
   let n = (List.length x)/2 in 
   List.filter (fun y->((List.length y)=n)&&(i_is_included_in y x) ) (level1@level2) ;;

let is_transitive sg =
   let temp1 = Image.image (fun sigma -> 
    eval_list_permutation sigma 1
   ) sg in 
   (i_sort temp1) = (Int_range.range 1 current_order) ;;
   

let d4 = List.hd(List.filter (fun x->List.length x=8) level2) ;; 
let halves_for_d4 = halves d4 ;; 

let a4 = List.hd(List.filter (fun x->List.length x=12) level2) ;; 
let halves_for_a4 = halves a4 ;; 

let halves_for_whole = halves full_subgroup ;;

    

(************************************************************************************************************************
Snippet 67 : Finding a polynomial x^4+p*x+q with Galois group A4
************************************************************************************************************************)
let u1 = Int_range.range (-50) 50 ;;
let u2 = Cartesian.square u1 ;;
let u3 = Image.image (fun (x,y)->(max(abs x)(abs y),(x,y)) ) u2 ;;
let u4 = Ordered.sort Total_ordering.standard2 u3 ;;
let unchecked_u5 = Image.image snd u4 ;;
let u5 = List.filter (fun (p,q)->List.for_all (fun z->z*z*z*z+p*z+q<>0) 
(Int_range.range (-1) 1)) unchecked_u5 ;;

let round x=
  let fl = floor x in 
  if (x -. fl) < 0.5 
  then int_of_float fl 
  else (int_of_float fl)+1 ;;  

let is_a_square n = 
    if n< 0 then false else
    let m =round(sqrt(float_of_int n)) in m * m = n;;  

let check = List.filter is_a_square (Int_range.range 0 100) ;;    

let u6 = List.filter (fun (p,q)->is_a_square(-27*p*p*p*p + 256*q*q*q)) u5 ;;

(************************************************************************************************************************
Snippet 66 : Removing indentation in a paragraph in a file  
************************************************************************************************************************)
let ap1 = Absolute_path.of_string "Fads/pan.ml" ;;

let text1 = Io.read_whole_file ap1 ;; 

let (before_text2,text2,after_text2) =
  Lines_in_string.tripartition_associated_to_interval text1 11 60 ;;

let old_lines_in_text2 = Lines_in_string.lines text2 ;;  
let new_lines_in_text2 = Image.image (Cull_string.cobeginning 3) old_lines_in_text2 ;; 

let new_text2 = String.concat "\n" new_lines_in_text2 ;;
let new_text1 = String.concat "\n" [before_text2;new_text2;after_text2] ;;

Io.overwrite_with ap1 new_text1 ;;

(************************************************************************************************************************
Snippet 65 : Intertwining prints for debugging purposes
************************************************************************************************************************)
open Needed_values ;;

let z1 = rf "Fads/pan.ml" ;;
let z2 = Lines_in_string.interval z1 10 58 ;;
let z3 = Lines_in_string.lines z2 ;;
let z4 = List.filter (fun line -> Cull_string.trim_spaces line <> "" ) z3 ;; 
let z5 = Int_range.index_everything z4 ;; 
let z6 = Image.image (
  fun (j,line) ->
    let sj = string_of_int j in 
    line^"\nprint_int "^sj^" ;;"
) z5 ;;
let z7 = "\n\n\n" ^ (String.concat "\n" z6) ^ "\n\n\n" ;;  

(************************************************************************************************************************
Snippet 64 : Problem involving periodicity
************************************************************************************************************************)
let find_periodicity l= 
  let rl = List.rev l in 
  let (a1,after_a1) = Listennou.ht rl in 
  let j = Listennou.find_index a1 after_a1 in 
  let inverted_motif = Listennou.big_head j rl in 
  let motif = List.rev inverted_motif in 
  let p = List.length motif in 
  let m0 = Min.list motif in 
  let i0 = Listennou.find_index m0 motif in 
  let after_m0 = Listennou.big_tail i0 motif 
  and before_m0 = Listennou.big_head (i0-1) motif in
  (p,m0::(after_m0@before_m0)) ;; 


let current_r = 5 ;;
let current_m = Gcd.lcm_for_many (Int_range.range 2 current_r) ;;

let pusher old_f n = 
  let lower_bound = max 1 (n-current_r) in  
  let temp1 = Int_range.range lower_bound (n-1) in 
  let temp2 = Image.image (fun m->(old_f m)-(current_m/(n-m))) temp1 in 
  let first_trial = Min.list temp2 in 
  if first_trial > 0 then first_trial else 
  let temp3 = Image.image (fun m->(old_f m)+(current_m/(n-m))) temp1 in   
  Max.list temp3 ;;

let ff = Memoized.recursive (fun old_f n->if n<2 then 1 else pusher old_f n) ;;

let z1 = Int_range.scale ff 1 200 ;;
let (period,motif) = find_periodicity z1 ;;
let last_in_motif = List.nth motif (period-1) ;;
let gg n = let r = n mod period in if r = 0 then last_in_motif else List.nth motif (r-1) ;;
let dg t = Min.list (Int_range.scale (fun k->(abs(gg(k+t)-gg(k)))*t ) 1 period) ;;
let (max_dg,dg_sols) = Min.minimize_it_with_care dg (Int_range.range 1 current_r) ;;
let largest_in_motif = Max.list motif ;;
let ratio = (float_of_int(largest_in_motif-List.hd(motif))) /. (float_of_int max_dg);;


(************************************************************************************************************************
Snippet 63 : Musings on the Szemeredi problem, chapter V
************************************************************************************************************************)
let current_width = 3 ;; 
let max_width = Sz_max_width_t.MW current_width ;;
let is_admissible = Sz_preliminaries.test_for_admissibility max_width ;;
let is_not_admissible x= (not(is_admissible x));;
let uncurried_sl  = Memoized.make (fun (x,k)->
  let temp1 = Sz_preliminaries.restricted_power_set (max_width,x) in 
  List.filter (fun z->List.length z=k) temp1 
) ;;  
let sl x k = uncurried_sl (x,k) ;;
let isl n k = uncurried_sl (Int_range.range 1 n,k) ;; 
let meas = Sz_precomputed.measure max_width ;;


let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
let i_is_included_in = Ordered.is_included_in Total_ordering.for_integers ;;
let i_merge = Ordered.merge Total_ordering.for_integers ;;
let i_outsert = Ordered.outsert Total_ordering.for_integers ;;
let i_fold_intersect = Ordered.fold_intersect Total_ordering.for_integers ;;
let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;



let original_minimal_carriers carriers sols =
  let indexed_carriers = Int_range.index_everything carriers in 
  let shadow = (
      fun sol ->
         Option.filter_and_unpack (
          fun (idx,carrier) -> 
             if i_is_included_in carrier sol 
             then Some idx 
            else None 
        ) indexed_carriers 
  )  in     
  let all_shadows = Image.image shadow sols in 
  Ordered_misc.minimal_transversals all_shadows ;;
 
let u_product sheaf1 sheaf2 = 
    let temp1 = Cartesian.product sheaf1 sheaf2 in 
    let temp2 = Image.image (fun (x,y)->i_merge x y) temp1 in 
    let temp3 = Ordered_misc.minimal_elts_wrt_inclusion temp2 in 
    il_sort temp3 ;;

 
let fold_u_product = function 
  [] -> []
  | sheaf :: other_sheaves -> List.fold_left  u_product sheaf other_sheaves ;;   
   

exception Nonunique_set_of_minimal_carriers of int list list list ;;
 
let set_of_minimal_carriers carriers sols =
 let version1 = original_minimal_carriers carriers sols in 
 let m = List.length(List.hd version1) in 
 let version2 = List.filter (fun x->List.length(x)=m) version1 in 
 let visualize = Image.image (fun idx->List.nth carriers (idx-1)) in 
 let version3 = Image.image visualize version2 in 
 if (List.length version3)<>1
 then raise (Nonunique_set_of_minimal_carriers version3)
 else List.hd version3;;
 
let set_of_minimal_carriers_with_extra carriers sols =
 try (Some(set_of_minimal_carriers carriers sols),None) with 
 Nonunique_set_of_minimal_carriers(nonunique) -> (None, Some nonunique)   ;;

let remains_of_obstructions_in_positing_case x=
  Option.filter_and_unpack (fun j->
      let k=(current_width+1)-j in 
      if x>2*k 
      then  Some [x-2*k;x-k]
      else None
  ) (Int_range.range 1 current_width) ;;

  

let analize_sheaf1 (left,bound,right) =
    let m = List.hd(List.rev right) in 
    let carriers = Sz_preliminaries.force_subset_in_interval    
      max_width right (1,m) in 
      set_of_minimal_carriers_with_extra carriers (sl left bound);;

let ref_for_missing_sheaves = ref [] ;;

exception Troublesome_aftersheaf of int list * int * int list * (int list list list);;
exception Missing_sheaves of (int list * int *  (int list list)) list ;;
  
let analize_sheaf2 (left,bound,right) =
  let (good_opt,bad_opt) = analize_sheaf1(left,bound,right) in 
  (
    match good_opt with 
     None -> raise(Troublesome_aftersheaf(left,bound,right,Option.unpack bad_opt)) 
    |Some usual -> 
      let _ = (ref_for_missing_sheaves:=[left,bound,usual]) in
      raise(Missing_sheaves [left,bound,usual])
  ) ;;    

let carrier_is_stronger_than_another carrier1 carrier2 =
   List.for_all (fun c1 -> 
    List.exists (fun c2 -> i_is_included_in c2 c1) carrier2) carrier1 ;;

exception Two_carriers_exn of 
  (int list) * int * (int list list) * (int list list) * (int list list list);;

let add_carrier_to_another (x,bound) carrier old_carrier =
   if carrier_is_stronger_than_another carrier old_carrier 
   then [carrier]
   else 
   let carrier2 = u_product carrier old_carrier in 
   let (good_opt,bad_opt) = set_of_minimal_carriers_with_extra carrier2 (sl x bound) in 
   if good_opt<>None
   then [Option.unpack good_opt]
   else raise (Two_carriers_exn(x,bound,carrier,old_carrier,Option.unpack bad_opt));; 

exception Add_carrier_exn of (int list) * int * (int list list) * (int list list list) ;;

let add_carrier (x,bound) carrier old_list = 
   match List.length old_list with 
    0 -> [carrier] 
   | 1 -> add_carrier_to_another (x,bound) carrier (List.hd old_list) 
   | _ -> raise (Add_carrier_exn(x,bound,carrier,old_list));;


let hashtbl_for_sheaves = ((Hashtbl.create 100): 
   ((int list) * int, int list list list) Hashtbl.t) ;;


let add_sheaf (x,bound) carrier =
  let new_val = (
   match Hashtbl.find_opt hashtbl_for_sheaves (x,bound)  with 
    Some (old_val) -> add_carrier (x,bound) carrier  old_val
   | None -> [carrier]
   ) in 
   Hashtbl.replace hashtbl_for_sheaves (x,bound) new_val ;;
   
let consult_sheaves (left,bound,right) =
  match Hashtbl.find_opt hashtbl_for_sheaves (left,bound)  with 
     None -> None
   | Some (sheaves) -> 
     Option.seek (fun sheaf->
       List.for_all (fun z->is_not_admissible (z@right)) sheaf
      ) sheaves
  ;;        

let sheaf_compare new_ll old_ll  =
    List.for_all (fun old_l-> List.exists 
    (fun new_l->i_is_included_in new_l old_l) new_ll ) old_ll  ;;

let sheaf_compare2 new_ll old_lls =
    List.exists (sheaf_compare new_ll) old_lls ;;
       
let sheaf_is_not_already_known_by_sheaves_hashtbl (left,bound,right) = 
  match Hashtbl.find_opt hashtbl_for_sheaves (left,bound)  with 
  None -> true
| Some (old_sheaves)  -> not(sheaf_compare2 right old_sheaves)  ;;       

let hashtbl_for_pre_measure = ((Hashtbl.create 100) :
(int list ,int list) Hashtbl.t) ;;

let sheaf_is_not_already_known_by_measures_hashtbl (left,bound,right) = 
  match Hashtbl.find_opt hashtbl_for_pre_measure left  with 
  None -> if is_admissible left 
          then (List.length left) >= bound
          else true
| Some (old_sol)  -> (List.length old_sol) >= bound  ;;   

let sheaf_is_not_already_known triple = 
   if sheaf_is_not_already_known_by_measures_hashtbl triple 
   then  sheaf_is_not_already_known_by_sheaves_hashtbl triple 
   else false ;; 

exception Borderline_case of ((int list) * int * ((int list) list)) list ;;

let commonest_decomposition (x,bound,carriers) =
    let (m,ry) = Listennou.ht(List.rev x) in 
    let y = List.rev ry in 
   let rem_obstr1 = remains_of_obstructions_in_positing_case m 
   and (pre_rem_obstr3,rem_obstr2) = List.partition (fun z->List.mem m z) carriers in    
   let rem_obstr3 = Image.image (i_outsert m) pre_rem_obstr3 in       
   let temp = (if List.mem [m] carriers then [] else 
    [
     (y,bound-1,il_fold_merge [rem_obstr1;rem_obstr2;rem_obstr3]);
    ])
   @ 
    [(y,bound,rem_obstr2)] in 
  let (cleaned_temp,dirty_temp) = List.partition sheaf_is_not_already_known_by_measures_hashtbl temp in    
  let (temp_good,temp_bad) = List.partition (fun (_,_,obstr2)->obstr2<>[]) cleaned_temp in   
  if temp_bad <> [] 
  then raise(Borderline_case(temp_bad)) 
  else    
  let temp3 = Image.image (
    fun tr3 ->
       let (y3,bound3,obstr3) = tr3 in 
       (tr3,set_of_minimal_carriers_with_extra obstr3 (sl y3 bound3))
  ) temp_good in 
  let (temp3_good,temp3_bad) = List.partition (fun 
    (_,(good_opt,bad_opt)) -> good_opt <> None
  ) temp3 in 
  (
    Image.image (fun ((y,bound,_),(good_opt,bad_opt))->(y,bound,Option.unpack good_opt) ) temp3_good,
    Image.image (fun (tr,(good_opt,bad_opt))->tr ) temp3_bad,
    dirty_temp
  );;   



let enhanced_commonest_decomposition triple =
  let (temp1,temp2,temp3) = commonest_decomposition triple in 
  (List.filter sheaf_is_not_already_known temp1, 
   List.filter sheaf_is_not_already_known temp2,
   temp3);;   

exception Add_sheaf_carefully_exn of (int list * int * int list list) list ;;

let add_sheaf_carefully (x,bound) carrier =
  let (temp1,temp2,_) = enhanced_commonest_decomposition (x,bound,carrier) in 
  let temp3 = temp1 @ temp2 in 
  if temp3 <> []
  then raise(Add_sheaf_carefully_exn(temp3))     
  else add_sheaf (x,bound) carrier ;;

let consult_sheaves_and_double_check (left,bound,right) =
  match consult_sheaves (left,bound,right) with 
    Some obstr -> Some obstr 
   | None ->
     let sols = sl left bound in 
     if List.for_all (fun z->is_not_admissible (z@right)) sols
     then analize_sheaf2(left,bound,right) 
     else None ;;
     


let analize_missing_sheaves ()=
  let triple = List.hd(!ref_for_missing_sheaves) in 
  let (temp1,temp2,temp3) = enhanced_commonest_decomposition triple in 
  let _= (ref_for_missing_sheaves:=temp1@temp2) in 
  (temp1,temp2,temp3);;

let default_string_of_intlist l=  
  "["^(String.concat ";" 
  (Image.image string_of_int l)
  )^"]";;  

  let ennified_string_of_intlist l=
    let a = List.hd(l) and b = List.hd(List.rev l) in 
    if (l=Int_range.range a b)
    then  "Ennig.ennig "^(string_of_int a)^" "^(string_of_int b)    
  else default_string_of_intlist l;;

let string_of_intlistlist ll=  
"["^(String.concat ";" 
(Image.image default_string_of_intlist ll)
)^"]";;

let message_for_newcomer (x,bound,carriers) =
 "asc ("^(ennified_string_of_intlist x)^","^(string_of_int bound)^") "^
  (string_of_intlistlist carriers)^" ;;" 
 ;;

let message_for_newcomers l= 
  "\n\n\n"^(String.concat "\n" 
  (Image.image message_for_newcomer l)
  )^"\n\n\n";;

let analize_repeatedly initial_triple =
   let triple1 = enhanced_commonest_decomposition initial_triple in  
   let (bt1,bt2,_) = triple1 in 
   if (bt1,bt2) = ([],[]) 
   then let msg = "\n\n\n"^(message_for_newcomer initial_triple)^"\n\n\n" in 
        let _= (print_string msg;flush stdout) in 
        (initial_triple,triple1)
   else  
    let rec tempf = (fun (t1,t2,t3)->
      let (nt1,nt2,nt3) = enhanced_commonest_decomposition (List.hd(t1@t2)) in 
      if (nt1,nt2) = ([],[])
      then let msg = message_for_newcomers (t1@t2) in 
           let _= (print_string msg;flush stdout) in 
           (initial_triple,(t1,t2,t3))  
      else tempf(nt1,nt2,nt3)
    ) in 
    tempf triple1 ;;  
    

let hashtbl_for_solving = ((Hashtbl.create 100) :
(int list * int * int list, int list) Hashtbl.t) ;;



let solve_in_easy_case (left,bound,right) = 
  let p = List.length left in 
  if p < bound 
  then Some None 
  else 
  if p = bound 
  then let unique = left @ right in 
       if is_admissible unique 
       then Some (Some unique)
       else Some None  
  else None ;;

let quick_way_out1 (left,bound,right) =
    if bound<>List.length(left) then false else 
    not(is_admissible(left@right)) ;;  

let pre_measure_arg whole              = (false,whole,0,[]) ;;
let solve_arg   (left,bound,right) = (true,left,bound,right) ;;
let pre_measure_ret m = (m,None) ;;
let solve_ret opt= ([],opt) ;;


let induction_in_solve_case old_f triple = 
  match solve_in_easy_case triple with 
   Some easy_answer -> solve_ret easy_answer 
   | None ->
  (
   match Hashtbl.find_opt hashtbl_for_solving triple with 
   Some old_answer -> solve_ret (Some old_answer) 
   | None ->
    let old_solve = (fun tr -> snd (old_f(solve_arg(tr)))) in 
    let opt_sol = (
      let (left,bound,right) = triple in  
      let (m,ry) = Listennou.ht(List.rev left) in 
      let y = List.rev ry in 
      if is_not_admissible (m::right)
      then old_solve(y,bound,right)
      else     
      let temp1 = [(y,bound-1,m::right);(y,bound,right)] in 
      let my = List.length(fst (old_f (pre_measure_arg(y)))) in 
      let temp2 = List.filter (fun (y2,bound2,_)->bound2<=my) temp1 in 
      let temp3 = List.filter (fun tr->not(quick_way_out1 tr)) temp2 in 
      let temp4 = List.filter (
          fun tr -> (consult_sheaves_and_double_check tr) = None
      )  temp3 in  
      let temp5 = Option.filter_and_unpack old_solve temp4 in 
      if temp5 = []
      then None  
      else Some(List.hd(List.rev temp5))
    ) in 
    let _ = (if opt_sol <>None 
      then Hashtbl.add hashtbl_for_solving triple (Option.unpack opt_sol)) in 
    solve_ret opt_sol
  );;

let induction_in_pre_measure_case old_f whole = 
   if is_admissible whole 
   then pre_measure_ret(whole) 
   else 
   match Hashtbl.find_opt hashtbl_for_pre_measure whole with 
    Some old_answer -> pre_measure_ret(old_answer) 
   |None -> 
   let new_answer = (
    let (m,ry) = Listennou.ht(List.rev whole) in 
   let y = List.rev ry in  
   let sy = fst(old_f(false,y,0,[])) in 
   if is_admissible(sy@[m])
   then  pre_measure_ret(sy@[m])
   else   
   (
     match snd(old_f(true,y,List.length sy,[m])) with 
     None -> pre_measure_ret(sy)
     |Some fitting_one -> pre_measure_ret(fitting_one@[m])
   )) in 
  let _ = Hashtbl.add hashtbl_for_pre_measure whole (fst new_answer) in 
  new_answer;;
   

let rec main_iterator (case,left,bound,right) =
  if case 
  then induction_in_solve_case main_iterator (left,bound,right) 
  else induction_in_pre_measure_case main_iterator left ;;  


let pre_measure whole            = fst(main_iterator(pre_measure_arg(whole))) ;; 
let solve (left,bound,right) = snd(main_iterator(solve_arg(left,bound,right))) ;;  

let clear_hashtables () = (
    Hashtbl.clear hashtbl_for_sheaves  ;
    Hashtbl.clear hashtbl_for_solving  ;
    Hashtbl.clear hashtbl_for_pre_measure  ;
) ;;

exception FF_exn of (( int list * int * int list list) *
((int list * int * int list list) list *
(int list * int * int list list) list *
(int list * int * int list list) list)) ;;

let ff n = 
  try pre_measure (Int_range.range 1 n) with 
  Missing_sheaves(l) ->
    let (t1,t2,t3) = List.hd l in 
    let (a,b) = analize_repeatedly(t1,t2,t3) in 
    raise(FF_exn(a,b)) ;;


let ams = analize_missing_sheaves;;
let asc = add_sheaf_carefully ;;

let comp1 = Int_range.scale ff 1 5 ;;

asc (Int_range.range 1 4,3) [[4]] ;; 
ff 6;;
asc (Int_range.range 1 1,1) [[1]] ;; 
asc (Int_range.range 1 2,1) [[1];[2]] ;; 
asc (Int_range.range 1 2,2) [[1]] ;; 
asc (Int_range.range 1 3,2) [[1];[2;3]] ;;
asc (Int_range.range 1 4,3) [[1;4]] ;; 
asc (Int_range.range 1 5,3) [[5];[1;4]] ;;
asc (Int_range.range 1 5,4) [[1;4]] ;;
ff 7;;
asc (Int_range.range 1 2,2) [[2]] ;; 
asc (Int_range.range 1 3,1) [[1];[2];[3]] ;;
asc (Int_range.range 1 3,2) [[2];[1;3]] ;;
asc (Int_range.range 1 4,2) [[2];[1;3];[1;4];[3;4]] ;;
asc (Int_range.range 1 4,3) [[2];[1;3];[3;4]] ;;
asc (Int_range.range 1 5,3) [[1;4];[2;5]] ;;
asc (Int_range.range 1 5,4) [[2;5]] ;;
asc (Int_range.range 1 6,3) [[6];[1;4];[2;5]] ;;
asc (Int_range.range 1 6,4) [[2;5];[4;6]] ;;
Int_range.scale ff 8 10;;
asc (Int_range.range 1 9,5) [[9]] ;; 
Int_range.scale ff 11 13;;
asc (Int_range.range 1 12,7) [[12]] ;; 
ff 14;;
asc (Int_range.range 1 10,5) [[9];[10]] ;;
asc (Int_range.range 1 10,6) [[9]] ;;
asc (Int_range.range 1 11,6) [[9];[10;11]] ;;
asc (Int_range.range 1 12,7) [[9;12]] ;;
asc (Int_range.range 1 13,7) [[13];[9;12]] ;;
asc (Int_range.range 1 13,8) [[9;12]] ;;
ff 15;;
asc (Int_range.range 1 10,6) [[10]] ;;
asc (Int_range.range 1 11,5) [[9];[10];[11]] ;;
asc (Int_range.range 1 11,6) [[10];[9;11]] ;;
asc (Int_range.range 1 12,6) [[10];[9;11];[9;12];[11;12]] ;;
asc (Int_range.range 1 12,7) [[10];[7;10];[9;11];[11;12]] ;;
asc (Int_range.range 1 13,7) [[9;12];[10;13]] ;;
asc (Int_range.range 1 13,8) [[10;13]] ;;
asc (Int_range.range 1 14,7) [[14];[9;12];[10;13]] ;;
asc (Int_range.range 1 14,8) [[10;13];[12;14]] ;;
Int_range.scale ff 16 18;;
asc (Int_range.range 1 17,9) [[17]] ;;
Int_range.scale ff 19 21;;
asc (Int_range.range 1 20,11) [[20]] ;;

(*


let l =  [[1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13], 7, [[13]; [9; 12]]] ;;
let (bt1,bt2,bt3) = List.hd l ;;
let (a,b) = analize_repeatedly(bt1,bt2,bt3) ;;
let triple0 = enhanced_commonest_decomposition (bt1,bt2,bt3) ;;    

let analize_repeatedly (x,bound,carriers) =
   let triple0 = enhanced_commonest_decomposition (x,bound,carriers) in  
    let rec tempf = (fun (t1,t2,t3)->
      let (nt1,nt2,nt3) = enhanced_commonest_decomposition (List.hd(t1@t2)) in 
      if (nt1,nt2) = ([],[])
      then let msg = message_for_newcomers (t1@t2) in 
           let _= (print_string msg;flush stdout) in 
           (triple0,(t1,t2,t3))  
      else tempf(nt1,nt2,nt3)
    ) in 
    tempf triple0 ;;  


asc (Ennig.ennig 1 10,5) [[9];[10]] ;; 
asc (Ennig.ennig 1 5,4) [[2;5]] ;;

let g1 = Hashtbl.find hashtbl_for_sheaves (Ennig.ennig 1 4,3) ;;
asc (Ennig.ennig 1 4, 3)  [[2]; [1; 3]; [3; 4]] ;;

*)

(************************************************************************************************************************
Snippet 62 : Musings on the Szemeredi problem, chapter IV
************************************************************************************************************************)
let current_width = 3 ;; 
let max_width = Sz_max_width_t.MW current_width ;;
let is_admissible = Sz_preliminaries.test_for_admissibility max_width ;;
let is_not_admissible x= (not(is_admissible x));;

let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
let i_is_included_in = Ordered.is_included_in Total_ordering.for_integers ;;
let i_merge = Ordered.merge Total_ordering.for_integers ;;
let i_outsert = Ordered.outsert Total_ordering.for_integers ;;
let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;

let uncurried_sl  = Memoized.make (fun (n,k)->
   let temp1 = Sz_preliminaries.restricted_power_set (max_width,Int_range.range 1 n) in 
   List.filter (fun z->List.length z=k) temp1 
) ;;  
let sl n k = uncurried_sl (n,k) ;;

let original_minimal_carriers carriers sols =
  let indexed_carriers = Int_range.index_everything carriers in 
  let shadow = (
      fun sol ->
         Option.filter_and_unpack (
          fun (idx,carrier) -> 
             if i_is_included_in carrier sol 
             then Some idx 
            else None 
        ) indexed_carriers 
  )  in     
  let all_shadows = Image.image shadow sols in 
  Ordered_misc.minimal_transversals all_shadows ;;
 
exception Nonunique_set_of_minimal_carriers ;;
 
let set_of_minimal_carriers carriers sols =
 let version1 = original_minimal_carriers carriers sols in 
 let m = List.length(List.hd version1) in 
 let version2 = List.filter (fun x->List.length(x)=m) version1 in 
 if (List.length version2)<>1
 then raise Nonunique_set_of_minimal_carriers 
 else 
 Image.image (fun idx->List.nth carriers (idx-1)) (List.hd version2)
 ;;
 
let set_of_minimal_carriers_with_extra carriers sols =
 try (Some(set_of_minimal_carriers carriers sols),None) with 
 _ -> (None, Some carriers)   ;;


let tag1 = Int_range.scale (fun x->[x;2*x-1]) 2 (1+current_width) ;;
let tag2 = il_sort (Ordered_misc.minimal_transversals tag1) ;;

module ConstraintList = struct 

type t = CL of (int * ((int list) list) ) list ;;

exception Constraint_already_present of (int list) * int ;; 

let helper_for_adding (x,j,cstr_for_xj) cl_content=
   let rec tempf = (fun (above_j,to_be_treated) ->
   match to_be_treated with 
     [] -> List.rev ((j,cstr_for_xj)::above_j) 
     |k_pair :: others ->
        let (k,cstr_for_k) = k_pair in 
        (match Total_ordering.for_integers j k with 
         Total_ordering_result_t.Lower->tempf(k_pair::above_j,others)
        |Greater-> 
          List.rev_append ((j,cstr_for_xj)::above_j) to_be_treated
        |Equal -> raise (Constraint_already_present(x,j))
        )
   ) in
   tempf([],cl_content) ;;

let add (x,j,cstr_for_xj) (CL cl_content) = 
    CL (helper_for_adding (x,j,cstr_for_xj) cl_content) ;;
let at_index (CL cl_content) idx = List.assoc_opt idx cl_content ;;    
let empty_one = CL [] ;;

end ;;  


module Oar = struct 

type t = {
  solution : (int list) ;
  constraints : ConstraintList.t ;
} ;;

let add_constraint (x,j,cstr_for_xj) oar ={
   oar with constraints = ConstraintList.add (x,j,cstr_for_xj) oar.constraints ;
} ;;

let extract_constraint oar j = ConstraintList.at_index oar.constraints j ;;
let extract_solution oar = oar.solution ;;

let of_solution sol = {
  solution = sol ;
  constraints = ConstraintList.empty_one ;
} ;;

end ;;

module Boat = struct 

type t = B of ((int list) * Oar.t) list ;;

let empty_one = B [] ;;

exception Solution_already_present of int list ;; 

let helper_for_solution_adding (x,sol_for_x) boat_content=
   let rec tempf = (fun (above_x,to_be_treated) ->
   match to_be_treated with 
     [] -> let oar_for_x = Oar.of_solution sol_for_x in 
            List.rev ((x,oar_for_x)::above_x) 
     |y_pair :: others ->
        let (y,data_for_y) = y_pair in 
        (match Total_ordering.silex_for_intlists x y with 
         Total_ordering_result_t.Lower->tempf(y_pair::above_x,others)
        |Greater-> 
          let oar_for_x = Oar.of_solution sol_for_x in 
          List.rev_append ((x,oar_for_x)::above_x) to_be_treated
        |Equal -> raise (Solution_already_present x)
        )
   ) in
   tempf([],boat_content) ;;

let add_solution (B l) (x,sol_for_x) = 
   B(helper_for_solution_adding (x,sol_for_x) l) ;; 

exception Solution_not_found of int list ;;

let helper_for_constraint_adding (x,j,cstr_for_xj) boat_content=
      let rec tempf = (fun (above_x,to_be_treated) ->
      match to_be_treated with 
        [] -> raise(Solution_not_found x)
        |y_pair :: others -> 
           let (y,data_for_y) = y_pair in 
           (match Total_ordering.silex_for_intlists x y with 
            Total_ordering_result_t.Lower->tempf(y_pair::above_x,others)
           |Greater-> raise(Solution_not_found x)
           |Equal -> 
              let new_data_for_y = Oar.add_constraint (x,j,cstr_for_xj) data_for_y in 
              List.rev_append above_x ((y,new_data_for_y)::others)
           )
      ) in
      tempf([],boat_content) ;;
   
let add_constraint (B l) (x,j,constr_for_xj) = 
      B(helper_for_constraint_adding (x,j,constr_for_xj) l) ;;   

let get_opt (B l) x = List.assoc_opt x l ;;

end ;;  


module ThisBoat = struct 

let main_ref = ref Boat.empty_one ;; 

let add_constraint cstr = (main_ref:= Boat.add_constraint (!main_ref) cstr) ;;
let add_solution sol = (main_ref:= Boat.add_solution (!main_ref) sol) ;;
let get_opt x = Boat.get_opt (!main_ref) x ;;

end ;;

exception Missing_constraints of int list * int list;; 


let induction_step_in_partial_case old_f (x,treated) = (
  if List.length(x)<3 
  then Oar.of_solution(x@treated) 
  else
  let (n,ry) = Listennou.ht(List.rev x) in 
  let y = List.rev ry in 
  if is_not_admissible(n::treated)
  then old_f(false,y,treated)  
  else 
  let answer_for_y = old_f(true,y,[]) in 
  let soly = Oar.extract_solution answer_for_y in 
  let my = List.length soly in 
  match Oar.extract_constraint answer_for_y my with 
   None -> Oar.of_solution(soly @ [n]) 
  |Some (constraints) ->
    if List.for_all (fun z->is_not_admissible(z@[n])) constraints
    then Oar.of_solution(soly)
    else Oar.of_solution(soly@[n]) 
  ) ;;    

let induction_in_full_case old_f x =
    match ThisBoat.get_opt x with 
       Some(old_answer) -> old_answer 
     | None -> 
    let sol_for_x = (
      if List.length(x)<3 
      then x  
      else Oar.extract_solution(old_f(false,x,[]))) in 
    if is_not_admissible sol_for_x 
    then raise (Missing_constraints (x,sol_for_x))
    else   
    let _ = ThisBoat.add_solution (x,sol_for_x) in 
    Oar.of_solution sol_for_x ;;

let rec helper_for_solving (is_full,x,treated) =
   if is_full 
   then induction_in_full_case         helper_for_solving x 
   else induction_step_in_partial_case helper_for_solving (x,treated)  ;; 

let solve x = helper_for_solving (true,x,[]) ;;   

let ff n = solve (Int_range.range 1 n) ;;

ff 1 ;;
ff 2 ;;
ThisBoat.add_constraint ([1;2],2,[[1;2]]) ;;
ff 3 ;;
ff 4 ;;
ff 5 ;;
ThisBoat.add_constraint (Int_range.range 1 5,4,[[1;2;4;5]]) ;;
ff 6 ;;

let z1 = set_of_minimal_carriers [[5;6];[3;5];[1;4]] (sl 8 4);;
let z1 = set_of_minimal_carriers_with_extra [[5;6];[3;5];[1;4]] (sl 8 4);;

(************************************************************************************************************************
Snippet 61 : Musings on the 1234 problem, chapter II
************************************************************************************************************************)
let original_seed =
   [2; 3; 5; 7; 11; 13; 14; 15; 17; 19; 23; 27; 29; 31; 35; 37; 38] ;;
   
   let seed =
   [2; 3; 5; 6; 7; 10; 11; 13; 14; 15; 17; 19; 22; 23; 26; 27; 29; 30; 31; 33;
      34; 35; 37; 38] ;;
   
   let seed = 
     [2; 3; 5; 6; 7; 9; 10; 11; 13; 14; 15; 17; 19; 21; 22; 23; 25; 26; 27; 29;
      30; 31; 33; 34; 35; 37; 38] ;;
   
   let seed =    
   [2; 3; 5; 6; 7; 9; 10; 11; 13; 14; 15; 17; 18; 19; 21; 22; 23; 25; 26; 27;
      29; 30; 31; 33; 34; 35; 37; 38] ;;
   
   let oi = Total_ordering.for_integers ;;
   let oi2 = Total_ordering.product oi oi ;; 
   
   let reorder pairs =
      let temp1 = Listennou.partition_according_to_fst pairs in 
      let temp2 = Image.image (fun (x,ll)->
          (x,Ordered.safe_set oi2 (List.flatten ll))
         ) temp1 in 
      let temp3 = Ordered.sort Total_ordering.for_integers (Image.image fst temp2) in 
      Image.image (fun x->(x,List.assoc x temp2)) temp3 ;;
   
   let new_elts l=
     let temp1 = Uple.inclusive_list_of_pairs l in 
     let temp2 = Option.filter_and_unpack (
       fun (a,b)->
         if not(Ordered.mem oi (a+b) l) then None else
         let c= a*b  in 
         if Ordered.mem oi c l then None else Some(c,[a,b])
     ) temp1 in 
     reorder temp2 ;;
   
   let generic_push (old_state,old_explanations)=
      let temp1 = new_elts old_state in 
      let temp2 = Image.image fst temp1 in 
      ((Ordered.merge oi old_state temp2,old_explanations@temp1),temp1) ;;
   
   let walker = ref (seed,[]) ;;
   
   let push () = 
      let (new_state,explanations) = generic_push (!walker) in 
      let _ = (walker := new_state) in 
      explanations ;;
   
   let z1 = push () ;;
   let level2 = Image.image fst z1 ;;
   let z2 = List.filter (fun x->x<=38) level2 ;;
   let new_seed = Ordered.merge oi seed z2 ;;
   
   
   

(************************************************************************************************************************
Snippet 60 : Musings on the 1234 problem
************************************************************************************************************************)
open Needed_values ;;

type sensitive_t = {
   unsorted : (int * (int * int) list) list;
   sorted : int list ;
}  ;;  

type increase_t = 
    Inertia of int 
   |Forced of int * ((int*int) list)
   |Redundancy of int * ((int*int) list) 
   |Pass of int * (int list);;


let oi = Total_ordering.for_integers ;;
let oi2 = Total_ordering.product oi oi ;; 

let reorder pairs =
   let temp1 = Listennou.partition_according_to_fst pairs in 
   let temp2 = Image.image (fun (x,ll)->
       (x,Ordered.safe_set oi2 (List.flatten ll))
      ) temp1 in 
   let temp3 = Ordered.sort Total_ordering.for_integers (Image.image fst temp2) in 
   Image.image (fun x->(x,List.assoc x temp2)) temp3 ;;

exception Sore_wound of int list * int * sensitive_t;;

let special_obstructions =
   [
     [2;3;6;8;9]; (* because of [3; 6; 9; 12=2*6] *)
   ]
   @
   (Image.image (fun j->
      [2;j;2*j;2*j+2;3*j] (* because of [j; 2*j; 3*j; 4*j=2*2*j] *)
      
      ) [ 7;11;15]) ;;

let find_initial_obstruction_opt sorted_l =
     let a =List.hd sorted_l and b = List.hd(List.rev sorted_l) in 
     match Option.seek (fun j->Ordered.is_included_in oi [j;2*j;3*j;4*j] sorted_l) 
        (Int_range.range a (b/4)) with 
     None -> Option.seek (fun obstr-> Ordered.is_included_in oi obstr sorted_l) special_obstructions
     |Some(j) -> Some [j;2*j;3*j;4*j];;   



module Sensitive = struct 
 
   let check_before_adding x stv = 
      let new_sorted = Ordered.insert oi x stv.sorted in   
      match  find_initial_obstruction_opt new_sorted with 
      None -> (None,Some new_sorted) 
      |Some obstr -> (Some obstr,None) ;;

   let add (x,data_for_x) stv = 
     let (opt_bad,opt_good) = check_before_adding x stv in 
     match opt_bad with 
     Some obstr ->  raise( Sore_wound(obstr,x,stv))
      |None ->
     let new_sorted = Option.unpack opt_good in   
   {
      unsorted = (x,data_for_x) :: stv.unsorted;
      sorted = new_sorted ;
   }   ;;

   let coming_from_last_element stv last_elt =
       let temp1 = Option.filter_and_unpack (fun (x,_)-> 
         let y= last_elt -x in 
         if (x>1)&&(x<=y)&&(List.exists (fun (z,_)->z=y) stv.unsorted) 
         then Some(x*y,[x,y])
         else None   
         ) stv.unsorted in 
         reorder temp1 ;; 
    
   let default_increase stv forbidden_indices =
         let part = Ordered.merge oi stv.sorted (Image.image fst forbidden_indices) in 
         let max_val = (if part=[] then 1 else 1+(List.hd(List.rev part))) in 
         let whole = Int_range.range 1 max_val in  
         let possibilities = Ordered.setminus oi whole part in 
         List.hd possibilities ;;   
   
   let empty_one = {unsorted =[] ; sorted =[]} ;;      

   let has_index idx stv= Ordered.mem oi idx stv.sorted ;;

   let incorporate_redundancy (idx,new_decompositions) stv = 
      let new_unsorted = (Image.image (
                  fun old_pair ->
                     let (z,ll) = old_pair in 
                     if z = idx 
                     then (z,ll@new_decompositions)
                     else old_pair   
               ) stv.unsorted ) in 
      {stv with unsorted = new_unsorted ;};;


end ;;   


module Increase = struct 

   type t = increase_t ;;

   

   let string_of_int_pair (x,y)= "("^(string_of_int x)^","^(string_of_int y)^")";;
   let string_of_int_list l =
      "[" ^ (String.concat ";" (Image.image string_of_int l)) ^ "]" ;;
   let string_of_ipair_list l =
      "[" ^ (String.concat ";" (Image.image string_of_int_pair l)) ^ "]" ;;
   
   let message = function 
      Inertia (new_val) -> (string_of_int new_val)^" added by inertia"
      |Forced(new_val,l) -> (string_of_int new_val)^" forced by "^(string_of_ipair_list l)
      |Redundancy(new_val,l) -> "Redundancy : "^(string_of_int new_val)^", with "^(string_of_ipair_list l) 
      |Pass(new_val,l) -> (string_of_int new_val)^" refused because of "^(string_of_int_list l);;
   
   let next (treated,forbidden_indices,to_be_treated)=
      match to_be_treated with 
      [] -> let new_val = Sensitive.default_increase treated forbidden_indices in 
            let (opt_bad,opt_good) = Sensitive.check_before_adding new_val treated in 
            (match opt_bad with 
            Some obstr -> (Pass(new_val,obstr),(treated,forbidden_indices@[new_val,obstr],[])) 
            |None ->   
             (Inertia(new_val),(Sensitive.add (new_val,[]) treated,forbidden_indices,
                     Sensitive.coming_from_last_element treated new_val))) 
      |(new_x,ll) :: others ->
          if Sensitive.has_index new_x treated 
          then let treated2 = Sensitive.incorporate_redundancy (new_x,ll)  treated in 
               (Redundancy(new_x,ll),(treated2,forbidden_indices,others))
          else let addenda = Sensitive.coming_from_last_element treated new_x in 
               (Forced(new_x,ll),(Sensitive.add (new_x,ll) treated,
               forbidden_indices,reorder(others@addenda)));;            

end ;;   

   

let walker = ref (Sensitive.empty_one,[],[]) ;; 

let push () =
    let (incr,next_state) = Increase.next (!walker) in 
    let msg = (Increase.message incr)^"\n" in 
    let _ = (print_string msg;flush stdout) in 
    let _ = (walker:=next_state) in 
    next_state ;;

for j= 1 to 1000 do let _ = push () in () done ;;

let (a,b,c) = (!walker) ;;

let d = List.rev (Option.filter_and_unpack (fun (x,l)->
    if l=[] then Some x else None) a.unsorted);;


(*
walker:=([],[]) ;;
for j= 1 to 10 do let _ = push () in () done ;;
let z1 = fst (!walker) ;;
let z2 = Option.filter_and_unpack 
(fun (x,ll)->if ll=[] then Some x else None) z1;;
let z3 = Ordered.safe_set oi z2 ;;
let z4 = Basic.delta_list z3 ;;
let meas n = let q = (n/3) and r=(n mod 3) in 2*q+r ;;
let z5 = Image.image meas z3 ;;
let z6 = Basic.delta_list z5 ;;

walker:=([],[]) ;;
*)


(************************************************************************************************************************
Snippet 59 : Removes unnecessary blanks at the beginning of lines in an interval
************************************************************************************************************************)
open Needed_values ;;


let ap = Absolute_path.of_string "Filewatching/fw_with_githubbing.ml";;
let old_text = Io.read_whole_file ap ;;

let part1= Lines_in_string.interval old_text 1 171 ;;
let part2= Lines_in_string.interval old_text 172 254;;

let lines1 = Lines_in_string.lines part2 ;;
let lines2 = Image.image (Cull_string.cobeginning 5) lines1 ;;
let new_part2 = String.concat "\n" lines2 ;;
let new_text = part1 ^ "\n" ^ new_part2 ;;

Io.overwrite_with ap new_text ;;

(************************************************************************************************************************
Snippet 58 : Find all modules whose ml file contains a certain substring
************************************************************************************************************************)
open Needed_values ;;

let z1 = Fw_with_dependencies.all_mlx_files (!ucs) ;;
let z2 = List.filter (fun mlx -> (Dfn_full.to_ending mlx)= Dfa_ending.ml ) z1 ;;
let z3 = Explicit.filter (
   fun mlx -> 
    let ap = Dfn_full.to_absolute_path mlx in 
    let text = Io.read_whole_file ap in 
    Substring.is_a_substring_of "Automatic" text
) z2 ;;

(************************************************************************************************************************
Snippet 57 : Musings on the Szemeredi problem, chapter IV
************************************************************************************************************************)
open Needed_values ;;


let current_width = 3 ;; 
let max_width = Sz_max_width_t.MW current_width ;;


let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
let i_is_included_in = Ordered.is_included_in Total_ordering.for_integers ;;
let i_merge = Ordered.merge Total_ordering.for_integers ;;
let i_outsert = Ordered.outsert Total_ordering.for_integers ;;
let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;

let tag1 = Int_range.scale (fun x->[x;2*x-1]) 2 (1+current_width) ;;
let tag2 = il_sort (Ordered_misc.minimal_transversals tag1) ;;

let hashtbl_for_main = Hashtbl.create 100 ;;

let main_in_easy_case (n,avoided_elts) =
   let temp1 = Sz_preliminaries.restricted_power_set (max_width,Int_range.range 1 n) in 
   let temp2 = List.filter (
     fun y->i_does_not_intersect avoided_elts y
   ) temp1 in 
   Max.maximize_it_with_care List.length temp2 ;;

let translate1 t = Image.image (fun x->x+t) ;;
let translate2 t = Image.image (translate1 t) ;;

let first_break_without_1 avoided_elts =
   [(false,Option.filter_and_unpack 
    (fun x->if x > 1 then Some(x-1) else None) avoided_elts)];;
let first_break_with_1 avoided_elts = 
   if Listennou.extends avoided_elts [1] then None else 
   Some(Image.image ( fun x->
      (true,translate1 (-1) (i_merge avoided_elts x))
    ) tag2) ;;  

let first_break avoided_elts = 
   let part1 = first_break_without_1 avoided_elts in 
   match first_break_with_1 avoided_elts with 
   None -> part1 
   |Some(part2) -> part1 @ part2 ;;

let main_pusher old_f (n,avoided_elts) =
   if n<=15 
   then main_in_easy_case (n,avoided_elts) 
   else 
   let cases = first_break avoided_elts in 
   let temp1 = Image.image (
     fun (head_needed,new_avoided_elts) ->
        let (m2,sols2) = old_f(n-1,new_avoided_elts) in 
        let sols3 = translate2 1 sols2 in 
        if head_needed 
        then (m2+1,Image.image (fun x->1::x) sols3) 
        else (m2,sols3)   
   ) cases in 
   let (final_m,temp2) = Max.maximize_it_with_care fst temp1 in 
   (final_m,il_fold_merge (Image.image snd temp2)) ;;

exception Impatient_exn of int * (int list) ;;

let impatient_main (n,avoided_elts) =
   match Hashtbl.find_opt hashtbl_for_main (n,avoided_elts) with 
   Some res -> res 
   | None -> raise (Impatient_exn(n,avoided_elts)) ;; 


let main pair =
  match Hashtbl.find_opt hashtbl_for_main pair with 
   Some old_answer -> old_answer 
  | None -> 
    let answer = main_pusher impatient_main pair in 
    let _ = (Hashtbl.add hashtbl_for_main pair answer) in 
    answer ;; 

let sons avoided_elts = il_sort (Image.image snd (first_break avoided_elts));;

let iterator (already_treated,to_be_treated) =
   let temp1 = il_fold_merge (Image.image sons to_be_treated) in 
   let new_ones = List.filter (
     fun x->not(il_mem  x already_treated)
   ) temp1 in 
   (il_merge already_treated new_ones,new_ones) ;;

let rec computer pair =
   if snd pair = [] then fst pair else 
   computer(iterator pair) ;; 

let all_helpers = computer ([],[[]]) ;;   

let linear_main n = Image.image (fun y->main (n,y)) all_helpers ;;

let lm n = 
   let _ = linear_main n in 
   let (m,sols)=main (n,[]) in 
    (m,List.hd sols) ;;

let computation = Image.image (fun x->(x,lm x)) (Int_range.range 15 50);;


let check = List.filter (fun (n,(m,_))->m <> 
  Sz_precomputed.measure (Sz_max_width_t.MW current_width) n) computation;;

let easy_selector = Memoized.make(fun (n,k) ->
List.filter (fun x->List.length(x)=k) (Sz_preliminaries.restricted_power_set 
(Sz_max_width_t.MW current_width,Int_range.range 1 n))
) ;;  


let original_minimal_carriers carriers sols =
 let indexed_carriers = Int_range.index_everything carriers in 
 let shadow = (
     fun sol ->
        Option.filter_and_unpack (
         fun (idx,carrier) -> 
            if i_is_included_in carrier sol 
            then Some idx 
           else None 
       ) indexed_carriers 
 )  in     
 let all_shadows = Image.image shadow sols in 
 Ordered_misc.minimal_transversals all_shadows ;;

exception Nonunique_set_of_minimal_carriers ;;

let set_of_minimal_carriers carriers sols =
let version1 = original_minimal_carriers carriers sols in 
let m = List.length(List.hd version1) in 
let version2 = List.filter (fun x->List.length(x)=m) version1 in 
if (List.length version2)<>1
then raise Nonunique_set_of_minimal_carriers 
else 
Image.image (fun idx->List.nth carriers (idx-1)) (List.hd version2)
;;

let set_of_minimal_carriers_with_extra carriers sols =
try (Some(set_of_minimal_carriers carriers sols),None) with 
_ -> (None, Some carriers)   ;;

let original_carriers n = 
let temp = Int_range.scale (fun y->let x=current_width+1-y in [n-2*x;n-x]) 1 current_width in 
List.filter (fun l->List.hd(l)>0) temp;;  

let new_carriers_in_hard_case n carriers = 
let (temp1,temp2) = List.partition (fun l->List.mem n l) carriers in 
let temp3 = Image.image (i_outsert n) temp1 in 
let whole1 = temp2@temp3@(original_carriers n) in 
let whole2 = Ordered_misc.minimal_elts_wrt_inclusion whole1 in 
il_sort whole2 ;;

let easy_case (n,k,carriers) = 
(n-1,k,set_of_minimal_carriers carriers (easy_selector (n-1,k)));;

let hard_case (n,k,carriers) = 
let new_carriers = new_carriers_in_hard_case n carriers in 
(n-1,k-1,set_of_minimal_carriers new_carriers (easy_selector (n-1,k-1)));;  

let milton_product carriers1 carriers2 =
let temp1 = Cartesian.product carriers1 carriers2 in 
let temp2 = Image.image (fun (x,y)->i_merge x y) temp1 in 
let temp3 = Ordered_misc.minimal_elts_wrt_inclusion temp2 in 
il_sort temp3 ;;

let fold_milton = function 
[] -> []
| a :: b -> List.fold_left milton_product a b ;;

let meas = Sz_precomputed.measure max_width ;;  
let sample_size = 15 ;;
let current_a = 7 ;;
let base1 = Int_range.range 3 (meas current_a) ;;
let base2 = Cartesian.product base1 (Int_range.range 1 sample_size) ;;
let base3 = List.flatten(Image.image (
fun (sa,b) -> 
   Int_range.scale (fun sb->(sa,b,sb)) (meas(current_a+b)-sa+1) (meas b)
) base2);;
let base4 = List.flatten(Image.image (
fun (sa,b,sb) -> 
   Option.filter_and_unpack (
      fun zb -> 
        if List.length(zb) = sb 
        then Some(sa,Image.image (fun t->current_a+t) zb) 
        else None
   ) (Sz_preliminaries.restricted_power_set(max_width,Int_range.range 1 b))
) base3);;

let current_sa = 4 ;;
let current_left_component =
 List.filter (fun z->List.length(z)=current_sa) 
 (Sz_preliminaries.restricted_power_set(max_width,Int_range.range 1 current_a)) ;; 
let base5 = Option.filter_and_unpack (
fun (sa,zb) -> if sa = current_sa then Some zb else None
) base4 ;;
let base6 = Ordered_misc.minimal_elts_wrt_inclusion(il_sort base5) ;;
let base7 = Image.image (fun z->
let temp1 = Sz_preliminaries.force_subset_in_interval max_width z (1,List.hd(List.rev z)) in 
List.filter (fun l->Max.list(l)<=current_a) temp1
) base6 ;;
let base8 = Image.image (fun l->(l,
set_of_minimal_carriers_with_extra l current_left_component) 
) base7 ;;
let (first_half,other_half) = List.partition (fun (l,(opt1,opt2))->opt2=None) base8 ;;
let base9 = Image.image (fun (l,(opt1,opt2))->Option.unpack opt1) first_half ;;
let base10 = Image.image (fun (l,(opt1,opt2))->Option.unpack opt2) other_half ;;
let res1 = fold_milton base9 ;;

(************************************************************************************************************************
Snippet 56 : Musings on the Szemeredi problem 
************************************************************************************************************************)
open Needed_values ;;
module L3 = struct

   let current_width = 3 ;; 
   
   let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
   let i_merge = Ordered.merge Total_ordering.for_integers ;;
   let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
   let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
   let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
   let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;
   
   let tag1 = Int_range.scale (fun x->[x;2*x-1]) 2 (1+current_width) ;;
   let tag2 = il_sort (Ordered_misc.minimal_transversals tag1) ;;
   
   let hashtbl_for_main = Hashtbl.create 100 ;;
   
   let main_in_easy_case (n,avoided_elts) =
      let temp1 = Sz_preliminaries.restricted_power_set 
          (Sz_max_width_t.MW current_width,Int_range.range 1 n) in 
      let temp2 = List.filter (
        fun y->i_does_not_intersect avoided_elts y
      ) temp1 in 
      Max.maximize_it_with_care List.length temp2 ;;
   
   let translate1 t = Image.image (fun x->x+t) ;;
   let translate2 t = Image.image (translate1 t) ;;
   
   let first_break_without_1 avoided_elts =
      [(false,Option.filter_and_unpack 
       (fun x->if x > 1 then Some(x-1) else None) avoided_elts)];;
   let first_break_with_1 avoided_elts = 
      if Listennou.extends avoided_elts [1] then None else 
      Some(Image.image ( fun x->
         (true,translate1 (-1) (i_merge avoided_elts x))
       ) tag2) ;;  
   
   let first_break avoided_elts = 
      let part1 = first_break_without_1 avoided_elts in 
      match first_break_with_1 avoided_elts with 
      None -> part1 
      |Some(part2) -> part1 @ part2 ;;
   
   let main_pusher old_f (n,avoided_elts) =
      if n<=15 
      then main_in_easy_case (n,avoided_elts) 
      else 
      let cases = first_break avoided_elts in 
      let temp1 = Image.image (
        fun (head_needed,new_avoided_elts) ->
           let (m2,sols2) = old_f(n-1,new_avoided_elts) in 
           let sols3 = translate2 1 sols2 in 
           if head_needed 
           then (m2+1,Image.image (fun x->1::x) sols3) 
           else (m2,sols3)   
      ) cases in 
      let (final_m,temp2) = Max.maximize_it_with_care fst temp1 in 
      (final_m,il_fold_merge (Image.image snd temp2)) ;;
   
   exception Impatient_exn of int * (int list) ;;
   
   let impatient_main (n,avoided_elts) =
      match Hashtbl.find_opt hashtbl_for_main (n,avoided_elts) with 
      Some res -> res 
      | None -> raise (Impatient_exn(n,avoided_elts)) ;; 
   
   
   let main pair =
     match Hashtbl.find_opt hashtbl_for_main pair with 
      Some old_answer -> old_answer 
     | None -> 
       let answer = main_pusher impatient_main pair in 
       let _ = (Hashtbl.add hashtbl_for_main pair answer) in 
       answer ;; 
   
   let sons avoided_elts = il_sort (Image.image snd (first_break avoided_elts));;
   
   let iterator (already_treated,to_be_treated) =
      let temp1 = il_fold_merge (Image.image sons to_be_treated) in 
      let new_ones = List.filter (
        fun x->not(il_mem  x already_treated)
      ) temp1 in 
      (il_merge already_treated new_ones,new_ones) ;;
   
   let rec computer pair =
      if snd pair = [] then fst pair else 
      computer(iterator pair) ;; 
   
   let all_helpers = computer ([],[[]]) ;;   
   
   let linear_main n = Image.image (fun y->main (n,y)) all_helpers ;;
   
   let lm n = 
      let _ = linear_main n in 
      let (m,sols)=main (n,[]) in 
       (m,List.hd sols) ;;
   
   let computation = Image.image (fun x->(x,lm x)) (Int_range.range 15 50);;
   
   
   let check = List.filter (fun (n,(m,_))->m <> 
     Sz_precomputed.measure (Sz_max_width_t.MW current_width) n) computation;;
   
   
   end ;;
   
   
   
   module L4 = struct
   
     let current_width = 4 ;; 
     
     let i_does_not_intersect = Ordered.does_not_intersect Total_ordering.for_integers ;;
     let i_merge = Ordered.merge Total_ordering.for_integers ;;
     let il_fold_merge = Ordered.fold_merge Total_ordering.silex_for_intlists ;;
     let il_mem = Ordered.mem Total_ordering.silex_for_intlists ;;
     let il_merge = Ordered.merge Total_ordering.silex_for_intlists ;;
     let il_sort = Ordered.safe_set Total_ordering.silex_for_intlists ;;
     
     let tag1 = Int_range.scale (fun x->[x;2*x-1]) 2 (1+current_width) ;;
     let tag2 = il_sort (Ordered_misc.minimal_transversals tag1) ;;
     
     let hashtbl_for_main = Hashtbl.create 100 ;;
     
     let main_in_easy_case (n,avoided_elts) =
        let temp1 = Sz_preliminaries.restricted_power_set 
            (Sz_max_width_t.MW current_width,Int_range.range 1 n) in 
        let temp2 = List.filter (
          fun y->i_does_not_intersect avoided_elts y
        ) temp1 in 
        Max.maximize_it_with_care List.length temp2 ;;
     
     let translate1 t = Image.image (fun x->x+t) ;;
     let translate2 t = Image.image (translate1 t) ;;
     
     let first_break_without_1 avoided_elts =
        [(false,Option.filter_and_unpack 
         (fun x->if x > 1 then Some(x-1) else None) avoided_elts)];;
     let first_break_with_1 avoided_elts = 
        if Listennou.extends avoided_elts [1] then None else 
        Some(Image.image ( fun x->
           (true,translate1 (-1) (i_merge avoided_elts x))
         ) tag2) ;;  
     
     let first_break avoided_elts = 
        let part1 = first_break_without_1 avoided_elts in 
        match first_break_with_1 avoided_elts with 
        None -> part1 
        |Some(part2) -> part1 @ part2 ;;
     
     let main_pusher old_f (n,avoided_elts) =
        if n<=15 
        then main_in_easy_case (n,avoided_elts) 
        else 
        let cases = first_break avoided_elts in 
        let temp1 = Image.image (
          fun (head_needed,new_avoided_elts) ->
             let (m2,sols2) = old_f(n-1,new_avoided_elts) in 
             let sols3 = translate2 1 sols2 in 
             if head_needed 
             then (m2+1,Image.image (fun x->1::x) sols3) 
             else (m2,sols3)   
        ) cases in 
        let (final_m,temp2) = Max.maximize_it_with_care fst temp1 in 
        (final_m,il_fold_merge (Image.image snd temp2)) ;;
     
     exception Impatient_exn of int * (int list) ;;
     
     let impatient_main (n,avoided_elts) =
        match Hashtbl.find_opt hashtbl_for_main (n,avoided_elts) with 
        Some res -> res 
        | None -> raise (Impatient_exn(n,avoided_elts)) ;; 
     
     
     let main pair =
       match Hashtbl.find_opt hashtbl_for_main pair with 
        Some old_answer -> old_answer 
       | None -> 
         let answer = main_pusher impatient_main pair in 
         let _ = (Hashtbl.add hashtbl_for_main pair answer) in 
         answer ;; 
     
     let sons avoided_elts = il_sort (Image.image snd (first_break avoided_elts));;
     
     let iterator (already_treated,to_be_treated) =
        let temp1 = il_fold_merge (Image.image sons to_be_treated) in 
        let new_ones = List.filter (
          fun x->not(il_mem  x already_treated)
        ) temp1 in 
        (il_merge already_treated new_ones,new_ones) ;;
     
     let rec computer pair =
        if snd pair = [] then fst pair else 
        computer(iterator pair) ;; 
     
     let all_helpers = computer ([],[[]]) ;;   
     
     let linear_main n = Image.image (fun y->main (n,y)) all_helpers ;;
     
     let lm n = 
        let _ = linear_main n in 
        let (m,sols)=main (n,[]) in 
         (m,List.hd sols) ;;
     
     let computation = Image.image (fun x->(x,lm x)) (Int_range.range 15 50);;
     
     let check = List.filter (fun (n,(m,_))->m <> 
       Sz_precomputed.measure (Sz_max_width_t.MW current_width) n) computation;;
     
     end ;;
   
   
   let z1 =  List.filter (fun x->
     let m3 = Sz_precomputed.measure (Sz_max_width_t.MW 3) x 
     and m4 = Sz_precomputed.measure (Sz_max_width_t.MW 4) x in 
     m3<>m4) (Int_range.range 1 25);; 
   
   let g1 = L3.main (10,[]) ;;  
   let g2 = L4.main (10,[]) ;; 
   
   let h1 = L3.il_sort (Ordered_misc.minimal_transversals [[1;5;9];[2;6;10]]) ;;
   

(************************************************************************************************************************
Snippet 55 : 
************************************************************************************************************************)
open Needed_values ;;
let mixer (a,b,ll)= Image.image (fun l->a@(Image.image (fun t->t+b) l)) ll;;

let upwards_hat (a,n,b) =  
  let q1 = (n-a)/2 in 
  let central_move = (if (n-a) mod 2 = 0 then -1 else 1) in 
  let new_beginning = (a+2*q1)+central_move in 
  let q2 = (new_beginning-b)/2 in 
  (Int_range.scale (fun t->a+2*t) 0 q1)@(Int_range.scale (fun t->new_beginning-2*t) 0 q2) ;;

let downwards_hat (a,n,b) =  
  let q1 = (a-n)/2 in 
  let central_move = (if (a-n) mod 2 = 0 then 1 else -1) in 
  let new_beginning = (a-2*q1)+central_move in 
  let q2 = (b-new_beginning)/2 in 
  (Int_range.scale (fun t->a-2*t) 0 q1)@(Int_range.scale (fun t->new_beginning+2*t) 0 q2) ;;

exception Hat_definition_exn of int * int * int ;;

let hat (a,n,b) =
  if (((b-a) mod 2)=0)
    ||((n<a)&&(n>b))||((n>a)&&(n<b))
  then raise(Hat_definition_exn(a,n,b)) else  
  if a<n 
  then upwards_hat (a,n,b)
  else downwards_hat (a,n,b) ;;  

   
let eu_12  old_f n = mixer([1],1,old_f(n-1,1)) ;;
let eu_132 old_f n = if n=3 then [[1;3;2]] else mixer([1;3;2],3,old_f(n-3,1)) ;;
let eu_134 old_f n = if n=4 then [hat(1,4,2)] else [] ;;
let eu_135 old_f n = [hat(1,n,2)] ;;

let eu_21 old_f n = 
  if n=2
  then [[2;1]]
  else mixer([2;1],2,old_f(n-2,1));;

let eu_case1 i1 old_f n = mixer(hat(i1,1,i1-1),i1,old_f(n-i1,1)) ;;
let eu_case4 i1 old_f n = mixer(hat(i1,n,i1+1),0,old_f(i1-1,i1-1)) ;;  

let main_base n =
    (
      [
          [1;2],(fun old_f n_again ->mixer([1],1,old_f(n-1,1))) ;
          [1;3;2],eu_132 ;
          [1;3;4],eu_134 ; 
          [1;3;5],eu_135 ; 
          [2;1],eu_21;
          [2;3],(fun old_f n_again ->if n=3 then [[2;3;1]] else []); 
          [2;4],eu_case4 2; 
        ]  
    )  
    @
    (List.flatten(
      Int_range.scale (fun x->
        List.filter (fun (l,f)->(List.for_all(fun j->j>0)l)&&(List.hd(List.rev l)<= n)) [
          [x;x-2],eu_case1 x;
          [x;x+2],eu_case4 x; 
        ]
        ) 3 (n-2)
    ))
   @(
     [
      [n-1;n-3],eu_case1 (n-1);
      [n-1;n],(fun old_f n_again ->mixer([n-1;n],0,old_f(n-2,n-2)) ); 
      [n;n-2],(fun old_f n_again ->mixer([n],0,old_f(n-1,n-2)));
      [n;n-1],(fun old_f n_again ->mixer([n],0,old_f(n-1,n-1)));
     ]
   ) ;;

let small_values = [
   (1,1),[[1]];
   (2,1),[[1;2]];
   (2,2),[[2;1]];
   (3,1),[[1;2;3];[1;3;2]];
   (3,2),[[2;1;3];[2;3;1]];
   (3,3),[[3;1;2];[3;2;1]];
   (4,1),[[1;2;3;4];[1;2;4;3];[1;3;2;4];[1;3;4;2]];
   (4,2),[[2;1;3;4];[2;4;3;1]];
   (4,3),[[3;1;2;4];[3;4;2;1]];
   (4,4),[[4;2;1;3];[4;2;3;1];[4;3;1;2];[4;3;2;1]];
]   ;;

exception Main_parameter_exn of int * int ;;

let main = Memoized.recursive (fun old_f (n,i1)->
   match List.assoc_opt (n,i1) small_values with 
   Some(easy_answer) -> easy_answer 
   | None ->
     if (n<5)||(i1<1)||(i1>n) then raise(Main_parameter_exn(n,i1)) else 
     let temp1 = main_base n in 
     let temp2 = Image.image (fun (prefix,f)-> 
        if List.hd(prefix)=i1 
        then f old_f n
        else []  
     ) temp1  in 
     List.flatten temp2 
) ;;

let goal = List.flatten(Int_range.scale (fun m->(Int_range.scale (fun j->(m,j)) 1 m)) 1 25);;
exception Haddock of int * int ;;
let computation = Image.image (fun (n,i)-> try main(n,i) with _->raise(Haddock(n,i))) goal ;;

let whole = Memoized.make (fun n->
    List.flatten (Int_range.scale (fun j->main(n,j)) 1 n)
) ;;

let sizes = 
  let _ = whole 15 in 
  Int_range.scale (fun n->List.length(whole n)) 1 25;;
let check_sizes = (sizes = [1; 2; 6; 12; 20; 34; 56; 88; 136; 208; 314; 470; 700; 1038; 1534; 2262;
3330; 4896; 7192; 10558; 15492; 22724; 33324; 48860; 71630]) ;;

(************************************************************************************************************************
Snippet 54 : Musing on permutations satisfying |i-j|<1 -> |p(i)-p(j)|<=2, chapter III
************************************************************************************************************************)
open Needed_values ;;
let ointlist = Total_ordering.silex_compare Total_ordering.for_integers ;;

let extensions1 n l = match l with 
    [] -> Int_range.range 1 n 
   | a :: others ->
      List.filter (fun x->(x>0)&&(x<=n)&&(not(List.mem x l))) [a-2;a-1;a+1;a+2] ;;

let extensions2 n ll =
  List.flatten (Image.image (fun l->
    let temp1 = extensions1 n l in 
    Image.image (fun a-> a::l) temp1
    ) ll) ;;

let main = Memoized.make(fun n->
    let rec tempf = (fun j->
      if j=0 then [[]] else 
      extensions2 n (tempf (j-1))  
    ) in 
    Image.image List.rev (tempf n)
) ;;    

let selector = Memoized.make(fun (n,beginning)->
  List.filter (
   fun l->Listennou.extends l beginning  
) (main (n+1)));;

let old_sel beg n = List.length (selector (n-1,beg)) ;;

let sel beg = 
    let temp1 = Image.image string_of_int (Int_range.scale (old_sel beg) 1 18) in 
    let temp2 = String.concat "," temp1 in 
    let temp3 = "\n\n\n["^temp2^"]\n\n\n" in 
    print_string temp3;;

let mixer (a,b,ll)= Image.image (fun l->a@(Image.image (fun t->t+b) l)) ll;;

let upwards_hat (a,n,b) =  
  let q1 = (n-a)/2 in 
  let central_move = (if (n-a) mod 2 = 0 then -1 else 1) in 
  let new_beginning = (a+2*q1)+central_move in 
  let q2 = (new_beginning-b)/2 in 
  (Int_range.scale (fun t->a+2*t) 0 q1)@(Int_range.scale (fun t->new_beginning-2*t) 0 q2) ;;

let downwards_hat (a,n,b) =  
  let q1 = (a-n)/2 in 
  let central_move = (if (a-n) mod 2 = 0 then 1 else -1) in 
  let new_beginning = (a-2*q1)+central_move in 
  let q2 = (b-new_beginning)/2 in 
  (Int_range.scale (fun t->a-2*t) 0 q1)@(Int_range.scale (fun t->new_beginning+2*t) 0 q2) ;;

exception Hat_definition_exn of int * int * int ;;

let hat (a,n,b) =
  if (((b-a) mod 2)=0)
    ||((n<a)&&(n>b))||((n>a)&&(n<b))
  then raise(Hat_definition_exn(a,n,b)) else  
  if a<n 
  then upwards_hat (a,n,b)
  else downwards_hat (a,n,b) ;;  

   
let eu_12  old_f n = mixer([1],1,old_f(n-1,1)) ;;
let eu_132 old_f n = if n=3 then [[1;3;2]] else mixer([1;3;2],3,old_f(n-3,1)) ;;
let eu_134 old_f n = if n=4 then [hat(1,4,2)] else [] ;;
let eu_135 old_f n = [hat(1,n,2)] ;;

let eu_21 old_f n = 
  if n=2
  then [[2;1]]
  else mixer([2;1],2,old_f(n-2,1));;

let eu_case1 i1 old_f n = 
  if n=i1 
  then mixer([i1],0,old_f(i1-1,i1-2))
  else mixer(hat(i1,1,i1-1),i1,old_f(n-i1,1)) ;;
let eu_case2 i1 old_f n = 
  if n=i1 
  then mixer([i1],0,old_f(i1-1,i1-1))
  else [] ;;
let eu_case3 i1 old_f n = 
  if n=(i1+1) 
  then mixer([i1;(i1+1)],0,old_f(i1-1,i1-1))
  else [] ;;
let eu_case4 i1 old_f n = mixer(hat(i1,n,i1+1),0,old_f(i1-1,i1-1)) ;;  

let main_base n =
    (
      [
          [1;2],eu_12 ;
          [1;3;2],eu_132 ;
          [1;3;4],eu_134 ; 
          [1;3;5],eu_135 ; 
          [2;1],eu_21;
          [2;3],eu_case3 2; 
          [2;4],eu_case4 2; 
        ]  
    )  
    @
    (List.flatten(
      Int_range.scale (fun x->
        List.filter (fun (l,f)->(List.for_all(fun j->j>0)l)&&(List.hd(List.rev l)<= n)) [
          [x;x-2],eu_case1 x;
          [x;x-1],eu_case2 x;
          [x;x+1],eu_case3 x; 
          [x;x+2],eu_case4 x; 
        ]
        ) 3 n
    )) ;;

let small_values = [
   (1,1),[[1]];
   (2,1),[[1;2]];
   (2,2),[[2;1]];
   (3,1),[[1;2;3];[1;3;2]];
   (3,2),[[2;1;3];[2;3;1]];
   (3,3),[[3;1;2];[3;2;1]];
   (4,1),[[1;2;3;4];[1;2;4;3];[1;3;2;4];[1;3;4;2]];
   (4,2),[[2;1;3;4];[2;4;3;1]];
   (4,3),[[3;1;2;4];[3;4;2;1]];
   (4,4),[[4;2;1;3];[4;2;3;1];[4;3;1;2];[4;3;2;1]];
]   ;;

exception Main_parameter_exn of int * int ;;

let main = Memoized.recursive (fun old_f (n,i1)->
   match List.assoc_opt (n,i1) small_values with 
   Some(easy_answer) -> easy_answer 
   | None ->
     if (n<5)||(i1<1)||(i1>n) then raise(Main_parameter_exn(n,i1)) else 
     let temp1 = main_base n in 
     let temp2 = Image.image (fun (prefix,f)-> 
        if List.hd(prefix)=i1 
        then f old_f n
        else []  
     ) temp1  in 
     List.flatten temp2 
) ;;

let support = List.flatten (Int_range.scale (fun m->Int_range.scale(fun j->(m,j)) 1 m) 1 18);;
let check = List.filter (
  fun (n,i1) -> (main (n,i1)) <> selector (n-1,[i1])
) support ;;


let dbg1= main_base 5 ;;
let dbg2 = List.filter (fun (l,f) -> List.hd l = 2) dbg1 ;;
let dbg3 = Image.image (
  fun (l,f)-> f main 5
) dbg2 ;;

eu_case2 3 main 5 ;;


(************************************************************************************************************************
Snippet 53 : Musing on permutations satisfying |i-j|<1 -> |p(i)-p(j)|<=2 
************************************************************************************************************************)
open Needed_values ;;
let ointlist = Total_ordering.silex_compare Total_ordering.for_integers ;;

let extensions1 n l = match l with 
    [] -> Int_range.range 1 n 
   | a :: others ->
      List.filter (fun x->(x>0)&&(x<=n)&&(not(List.mem x l))) [a-2;a-1;a+1;a+2] ;;

let extensions2 n ll =
  List.flatten (Image.image (fun l->
    let temp1 = extensions1 n l in 
    Image.image (fun a-> a::l) temp1
    ) ll) ;;

let main = Memoized.make(fun n->
    let rec tempf = (fun j->
      if j=0 then [[]] else 
      extensions2 n (tempf (j-1))  
    ) in 
    Image.image List.rev (tempf n)
) ;;    

let aa = Memoized.make(fun n->
  List.filter (
   fun l->List.hd(l) = 1
  ) (main (n+1))
) ;;

let uu = Memoized.make(fun n->
    List.filter (
     fun l->let rl = List.rev l in
     Listennou.extends rl [n;n-1]
    ) (main n)
) ;;

let vv = Memoized.make(fun n->
  List.filter (
   fun l->let rl = List.rev l in
   Listennou.extends rl [n-1;n]
  ) (main n)
) ;;

let ww = Memoized.make(fun n->
  List.filter (
   fun l->
    let gl = (fun k->List.nth l (k-1)) in 
    ((gl 1)=(n-1))&&(gl (n-1)=(n-2))&&(gl n=n)
  ) (main n)
) ;;

let tt = Memoized.make(fun n->
  List.filter (
   fun l->
    let gl = (fun k->List.nth l (k-1)) in 
    ((gl 1)<>(n-1))&&(gl n=(n-2))
  ) (main n)
) ;;

let ss = Memoized.make(fun n->
  List.filter (
   fun l->
    let i = Listennou.find_index (n-1) l in 
    if (List.mem i [1;n-1;n])
    then false 
    else (List.nth l i)=n   
  ) (main n)
) ;;

let s_to_w =Memoized.make(fun n -> Ordered.fold_merge ointlist 
  [ss n;tt n;uu n;vv n;ww n] );;

let reversed_s_to_w =Memoized.make(fun n ->
    Ordered.safe_set ointlist (Image.image List.rev (s_to_w n)));;

let double_s_to_w =Memoized.make(fun n ->
      Ordered.intersect 
      ointlist  (s_to_w n) (reversed_s_to_w n) );;
  

let na n = List.length(aa n);;
let nk n = List.length(main n);;
let ns n = List.length(ss n);;
let nt n = List.length(tt n);;
let nu n = List.length(uu n);;
let nv n = List.length(vv n);;
let nw n = List.length(ww n);;

let zz n=(na n,nu n,nv n,nw n,ns n,nt n,nk n) ;;

let uv n = (uu n,vv(n+1)) ;;

let da n = (na (n+3))-(na(n+2)+na(n)+1) ;; 
let ka n = (nk(n)) -(2*na(n-1)) ;;

let selector = Memoized.make(fun (n,t)->
  List.filter (
   fun l->List.hd(l) = t
  ) (main (n+1))
) ;;

let aa = Memoized.make (fun n->selector(n,1)) ;;
let bb = Memoized.make (fun n->selector(n,2)) ;;
let cc = Memoized.make (fun n->selector(n,3)) ;;
let dd = Memoized.make (fun n->selector(n,4)) ;;
let ee = Memoized.make (fun n->selector(n,5)) ;;

let peggy n = Int_range.scale (fun j->List.length(selector(n,j))) 1 n ;;


let na n = List.length(aa n);;
let nb n = List.length(bb n);;
let nc n = List.length(cc n);;
let nd n = List.length(dd n);;
let ne n = List.length(ee n);;

let zz n=(na n,nb n,nc n,nd n,nk n,ne n);;




(************************************************************************************************************************
Snippet 52 : Rename scanned files
************************************************************************************************************************)
open Needed_values ;;

let ap1 = Absolute_path.of_string (home^"/Downloads/Building_site/");;
let s_ap1 = Absolute_path.to_string ap1 ;;
let u2 = More_unix.beheaded_simple_ls (Directory_name.of_string s_ap1) ;;
let u3 = Image.image (fun s->(int_of_string(Cull_string.interval s 8 14),s)) u2 ;;
let (Set_of_poly_pairs_t.S u4) = Set_of_poly_pairs.safe_set u3 ;;
let u5 = Int_range.index_everything (Image.image fst u4) ;;
let u6 = Image.image (
  fun (k,old_k)->
    let sk = string_of_int k 
     and sok = string_of_int old_k in 
  "mv "^s_ap1^"korres\\ "^sok^".jpg "^s_ap1^"p"^sk^".jpg"
) u5 ;; 
let u7 = Image.image Sys.command u6 ;;

Coherent_pdf.workspace_directory := s_ap1 ;;
Coherent_pdf.implode ("p","") ;;

(************************************************************************************************************************
Snippet 51 : Write repetitive code for PARI-GP
************************************************************************************************************************)
let s_ap = home^
"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn2.gp" ;;

let ap = Absolute_path.of_string s_ap ;;

let gtext t1 t2 = 
  String.concat "\n"
  ["c_fa="^(string_of_int t1); 
   "c_t1="^(string_of_int t2); 
   "c_for_t2=big_subst(for_t2,[t1,fa],[c_t1,c_fa])";
 "interm=subst(part3_for_t3,t1,c_t1)";
 "c_for_t3=factor(polresultant(interm,c_for_t2,t2))[2,1]";
 "around_t2=make_zero(special_euclid(interm,c_for_t2,t2,c_for_t3,t3),t2)";
 "listput(accu,[[c_fa,c_t1],around_t2])";
 "printf(Str(c_fa,\",\"c_t1,\" done\\n\"))"] ;;

let u1 = Cartesian.product (Int_range.range 11 50) (Int_range.range 101 140) ;;
let u2 = Image.image (fun (t1,t2)->gtext t1 t2) u1 ;; 
let u3 = String.concat "\n\n\n" u2 ;;

Io.Private.append_string_to_file u3 ap;;

(*

let z1 = home^
"/Teuliou/Bash_scripts/Pari_Programming/my_pari_code/follenn1.gp" ;;

let z2 = Lines_in_string.interval (rf z1) 60 67 ;;

*)

(************************************************************************************************************************
Snippet 50 : A useful shortcut using Lines_in_string.remove_interval_in_file 
************************************************************************************************************************)
let ri fn x y =
     Lines_in_string.remove_interval_in_file 
      (Absolute_path.of_string fn) x y ;;

(************************************************************************************************************************
Snippet 49 : Test the prepare_fw_with_dependencies.ml file
************************************************************************************************************************)
let the_other_one = 
   Absolute_path.of_string "../Idaho/Filewatching/fw_with_dependencies.ml" ;;
 
(*  #use "Githubbed_archive/prepare_fw_with_dependencies.ml";; 
   
 write_all_to_file the_other_one ;;  *)

(************************************************************************************************************************
Snippet 48 : Typical use of marked comments
************************************************************************************************************************)
open Needed_values ;;

let src1 = Absolute_path.of_string "../Idaho/Filewatching/file_watcher.ml";;

let dest1 = Absolute_path.of_string "../Idaho/Filewatching/fw_modular.ml";;

let act1 () = Mark_comments_for_copying_or_deletion.copy src1 dest1 ;;

let act2 () = Shorten_long_blank_intervals.in_file dest1 ;;


(************************************************************************************************************************
Snippet 47 : Replacements on several files
************************************************************************************************************************)
open Needed_values ;;

let aps_ref = ref [];;

 aps_ref := [
   Absolute_path.of_string "../Idaho/Compilation_management/coma_state.ml";
   Absolute_path.of_string "../Idaho/Compilation_management/modify_coma_state.ml";
   Absolute_path.of_string "../Idaho/Ocaml_analysis/read_needed_ocaml_files.ml"
];;

let rep (x,y) = 
   Image.image (
     fun ap -> Replace_inside.replace_inside_file (x,y) ap
   ) (!aps_ref) ;; 

rep ("subdir_at_module","subdir_for_module")   ;;
rep ("principal_ending_at_module","principal_ending_for_module")   ;;
rep ("mli_presence_at_module","mli_presence_for_module")   ;;
rep ("principal_mt_at_module","principal_mt_for_module")   ;;
rep ("mli_mt_at_module","mli_mt_for_module")   ;;
rep ("direct_fathers_at_module","direct_fathers_for_module")   ;;
rep ("ancestors_at_module","ancestors_for_module")   ;;
rep ("needed_libs_at_module","needed_libs_for_module")   ;;
rep ("needed_dirs_at_module","needed_dirs_for_module")   ;;
rep ("product_up_to_date_at_module","last_compilation_result_for_module")   ;;

let fix () =
   let _ = Sys.command (
      "cp Decomposed_filename/dfa_subdirectory.ml "^
      "../Idaho/Decomposed_filename/afd_sybdirectoru.ml") in
   let _ = Sys.command (
         "mv ../Idaho/Decomposed_filename/afd_sybdirectoru.ml "^
         "../Idaho/Decomposed_filename/dfa_subdirectory.ml") in   
   let _ =  Image.image (fun s->
       let ap= Absolute_path.of_string ("../Idaho/"^s) in 
       Replace_inside.replace_inside_file  
       ("Afd_sybdirectoru.","Dfa_subdirectory.") ap
       ) [
      "Decomposed_filename/dfn_rootless.ml";
      "Decomposed_filename/dfn_endingless.ml";
      "Decomposed_filename/dfn_middle.ml";
      "Decomposed_filename/dfn_full.ml";
      "find_suitable_ending.ml";"more_unix.ml";"node_project.ml";"prepare_dircopy_update.ml";
      "Compilation_management/coma_constant.ml";
      "Compilation_management/coma_state.ml";
      "Compilation_management/save_coma_state.ml";
      "Compilation_management/modify_coma_state.ml";
      "Compilation_management/create_world_copy.ml";
      "Filewatching/fw_configuration.ml";
      "Filewatching/file_watcher.ml";
      "Filewatching/fw_with_dependencies.ml";
   ] in    
   ruco () ;;

   let fix () =
      let _ = Sys.command (
         "cp Decomposed_filename/dfa_subdirectory.ml "^
         "../Idaho/Decomposed_filename/afd_sybdirectoru.ml") in
      let _ = Sys.command (
            "mv ../Idaho/Decomposed_filename/afd_sybdirectoru.ml "^
            "../Idaho/Decomposed_filename/dfa_subdirectory.ml") in   
      let _ =  Image.image (fun s->
          let ap= Absolute_path.of_string ("../Idaho/"^s) in 
          Replace_inside.replace_inside_file  
          ("Afd_sybdirectoru.","Dfa_subdirectory.") ap
          ) [
         "Decomposed_filename/dfn_rootless.ml";
         "Decomposed_filename/dfn_endingless.ml";
         "Decomposed_filename/dfn_middle.ml";
         "Decomposed_filename/dfn_full.ml";
         "find_suitable_ending.ml";"more_unix.ml";"node_project.ml";"prepare_dircopy_update.ml";
         "Compilation_management/coma_constant.ml";
         "Compilation_management/coma_state.ml";
         "Compilation_management/save_coma_state.ml";
         "Compilation_management/modify_coma_state.ml";
         "Compilation_management/create_world_copy.ml";
         "Filewatching/fw_configuration.ml";
         "Filewatching/file_watcher.ml";
         "Filewatching/fw_with_dependencies.ml";
      ] in    
      ruco () ;;   

      let fix () =
         let _ = Sys.command (
            "cp Padioleau/lexer_ml.ml "^
            "../Idaho/Padioleau/rexel_ml.ml") in
         let _ = Sys.command (
               "mv ../Idaho/Padioleau/rexel_ml.ml "^
               "../Idaho/Padioleau/lexer_ml.ml") in   
         let _ =  Image.image (fun s->
             let ap= Absolute_path.of_string ("../Idaho/"^s) in 
             Replace_inside.replace_inside_file  
             ("Rexel_ml.","Lexer_ml.") ap
             ) [
            "Padioleau/parse_ml.ml";
         ] in    
         ruco () ;;   


(*   
let aps_ref = ref [];;

 aps_ref := Image.image (fun s->Absolute_path.of_string ("../Idaho/"^s)) [
   "Decomposed_filename/dfn_rootless.ml";
   "Decomposed_filename/dfn_endingless.ml";
   "Decomposed_filename/dfn_middle.ml";
   "Decomposed_filename/dfn_full.ml";
   "find_suitable_ending.ml";"more_unix.ml";"node_project.ml";"prepare_dircopy_update.ml";
   "Compilation_management/coma_constant.ml";
   "Compilation_management/coma_state.ml";
   "Compilation_management/save_coma_state.ml";
   "Compilation_management/modify_coma_state.ml";
   "Compilation_management/create_world_copy.ml";
   "Filewatching/fw_configuration.ml";
   "Filewatching/file_watcher.ml";
   "Filewatching/fw_with_dependencies.ml";
];;

let rep (x,y) = 
   Image.image (
     fun ap -> Replace_inside.replace_inside_file (x,y) ap
   ) (!aps_ref) ;; 

rep ("Afd_sybdirectoru.","Dfa_subdirectory.")   ;;
*)


(************************************************************************************************************************
Snippet 46 : Extract a line interval from a file and treat it
************************************************************************************************************************)
open Needed_values ;;

let z1 = Image.image (
   fun (methname,_,_,_)->"mod_details \""^methname^"\" ([\n   \"\"\n],\"\") ;;"
) [] ;;
let z2=""::(z1@[""]);;
let z3 = String.concat "\n\n" z2 ;;
let z4 () = print_string z3 ;;

let fn = "Filewatching/fw_with_dependencies.ml";;
let fn = "Fads/jug.ml";;
let z5 = Lines_in_string.interval (rf fn) 9 92 ;;
let z6 = Lines_in_string.lines z5;;
let z7 = Image.image (fun line->"   \""^line^"\";") z6;;
let z8 = "\n\n\n"^(String.concat "\n" z7)^"\n\n\n" ;;
let z9  = print_string z8 ;;

let z6 = Image.image (Cull_string.cobeginning 6) z6;;

let g1 = Image.image (fun s->
   let j = Strung.char_finder_from (fun c->c='/') s 1 in 
   Cull_string.beginning j s  ) z6;;
let g2 = Ordered.sort Total_ordering.lex_for_strings g1 ;;
let g3 = String.concat " " g2;;

(************************************************************************************************************************
Snippet 45 : Remove all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd = Dfa_subdirectory.of_line "Van_der_Waerden";;
let z1 = ae () ;;
let z2 = List.filter (
  fun (Dfn_endingless_t.J(r,s,m)) ->
    Dfa_subdirectory.begins_with s sd
) z1 ;;
let z3 = Image.image (
   fun (Dfn_endingless_t.J(r,s,m)) ->
    Dfa_module.to_line m
  ) z2 ;;
let act () = fgs z3 ;;  

(************************************************************************************************************************
Snippet 44 : Replacing a long interval in a file with another
************************************************************************************************************************)
open Needed_values ;;

let ap1 = Absolute_path.of_string "../Idaho/Compilation_management/coma_state.ml" ;;
let ap1_text = Io.read_whole_file ap1 ;;
let to_be_replaced = Lines_in_string.interval ap1_text 1 676 ;;

let towards_complement = rf "Fads/pan.ml";;
let replacement = Lines_in_string.interval towards_complement 9 240 ;;

let act7 () = Replace_inside.replace_inside_file (to_be_replaced,replacement) ap1;;

(************************************************************************************************************************
Snippet 43 : Visualize Git tree
************************************************************************************************************************)
open Needed_values ;;

let gc = "git -C "^home^"/Teuliou/OCaml/Idaho_backup ";;

let cmd_for_z0 = gc ^ "ls-tree -r HEAD > ~/Downloads/temp.txt";;
let z0 = Sys.command cmd_for_z0 ;;

let z1 = rf "~/Downloads/temp.txt";;
let z2 = Lines_in_string.lines z1 ;;
let z3 = List.filter (fun line->
   Supstring.contains line "depth_one"
  ) z2;;
let z4 = Image.image (Cull_string.cobeginning 53) z3;;


let cmds1 = Image.image (fun x->gc^"rm --cached "^x) z4 ;;
let cmds2 = Image.image (fun x->
  let cx = String.capitalize_ascii x in
  gc^"add "^cx) z4 ;;
let cmds3 = cmds1 @ cmds2 ;;
let anse1 = Image.image Sys.command cmds3 ;;

(************************************************************************************************************************
Snippet 42 : Miscellaneous tests on compilation management
************************************************************************************************************************)
open Needed_values ;;

let u1 = ae();;
let u2 = Image.image (fun el->Dfa_module.to_line(Dfn_endingless.to_module el)) u1 ;;
let u3 = Max.maximize_it_with_care (fun mn->
   min(List.length(abo mn))(List.length(bel mn))
  ) u2;;

let test1 = rv "Dfa_subdirectory.connectable_to_subpath" "cannectoble_to_sabputh" ;;
let exit_test1 = rv "Dfa_subdirectory.cannectoble_to_sabputh" "connectable_to_subpath" ;;

let test2= ren "dfa_subdirectory" "afd_sybdirectoru" ;;
let exit_test2= ren "afd_sybdirectoru" "dfa_subdirectory";;

let test3 = rensub "Depth_one_testdir/Depth_two_testdir" "Dopth_twe_tistder" ;;
let exit_test3 = rensub "Depth_one_testdir/Dopth_twe_tistder" "Depth_two_testdir" ;;

let create_ml_file_with_text (fname,text) =
   let s_ap = "Depth_one_testdir/Depth_two_testdir/adhoc/"^fname^".ml" in 
   let ap = Absolute_path.create_file_if_absent s_ap in 
   let _ = Io.overwrite_with ap text in ap ;;

let tf1 = create_ml_file_with_text("tf_one","let a= 2;;") ;;
let tf2 = create_ml_file_with_text("tf_two","let b= 3;;") ;;
let tf3 = create_ml_file_with_text("tf_three","let c= 7;;") ;;
let tf4 = create_ml_file_with_text("tf_four","let d= Tf_three.c+4;;") ;;
let tf5 = create_ml_file_with_text("tf_five","let e= Tf_four.d+5;;") ;;

regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_one.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_two.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_three.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_four.ml";;
regi "Depth_one_testdir/Depth_two_testdir/adhoc/tf_five.ml";;


Io.overwrite_with tf3 "let c=Tf_one.a+Tf_two.b;;" ;;

reco "1";;

fgs ["tf_one";"tf_two";"tf_three";"tf_four";"tf_five"] ;;

(************************************************************************************************************************
Snippet 41 : Extracting lines from a file and modifying them
************************************************************************************************************************)
open Needed_values ;;

let w2 = Image.image fst (vfm "Fw_with_dependencies") ;;
let z1 = rf "Filewatching/fw_with_dependencies.ml";;
let z2 = Lines_in_string.interval z1 1304 1351 ;;

let w2 = Image.image fst (vfm "Fw_with_small_details") ;;
let z1 = rf "Filewatching/fw_with_small_details.ml";;
let z2 = Lines_in_string.interval z1 309 334 ;;

let w2 = Image.image fst (vfm "Fw_with_archives") ;;
let z1 = rf "Filewatching/fw_with_archives.ml";;
let z2 = Lines_in_string.interval z1 285 317 ;;

let w2 = Image.image fst (vfm "File_watcher") ;;
let z1 = rf "Filewatching/file_watcher.ml";;
let z2 = Lines_in_string.interval z1 526 549 ;;

let w2 = Image.image fst (vfm "Fw_configuration") ;;

let z3 = Lines_in_string.lines z2 ;;
let z4 = Image.image (
  fun line->
    let j1=(try String.index_from line 1 ' ' with _->0)
    and j2=String.index line '=' in 
    Cull_string.trim_spaces (Cull_string.interval line (j1+1) (j2-1)) 
) z3 ;;
let z5 = Image.image (
  fun line->
       if not(String.contains line ' ')
       then line 
       else let j3 =  String.index line ' ' in 
             Cull_string.beginning j3 line
) z4 ;;
let bad_in_z5=List.filter (fun x->not(List.mem x w2)) z5;;

let z4=[];;
let check_z4 = Ordered.sort Total_ordering.lex_for_strings (Image.image snd z4) ;;

let write (fun_name,lbl)=
 let addendum=(
   if lbl = "constructor" then " dummy_fw" else
   if lbl = "zerovariate_producer" then " dummy_arg" else ""

) in 
 "    let "^fun_name^" = extract_"^lbl^" All_printables."^fun_name^addendum^" ;;" ;;

let z5 = "\n\n\n" ^(String.concat "\n" (Image.image write z4)) ^ "\n\n\n";; 
let z6 () = print_string z5 ;;


let write2 (fun_name,lbl)=
 "let "^fun_name^" = Private.Exit."^fun_name^" ;;" ;;

let z7 = "\n\n\n" ^(String.concat "\n" (Image.image write2 z4)) ^ "\n\n\n";; 
let z8 () = print_string z7 ;;

(************************************************************************************************************************
Snippet 40 : Get a list of value names from an interval of lines in a file
************************************************************************************************************************)
open Needed_values ;;

let u1 = rf "Filewatching/fw_with_dependencies.ml";;
let u2 = Lines_in_string.interval u1 33 48 ;;
let u3 = Lines_in_string.lines u2 ;;
(*
let compute_names = Image.image (
  fun line ->
     let temp1 = Cull_string.two_sided_cutting ("    let ","") line in 
     let j1 = Strung.char_finder_from (fun c->List.mem c [' ';'\r';'\t']) temp1 1 in
     Cull_string.interval temp1 1 (j1-1)
) u3;;
*)


(************************************************************************************************************************
Snippet 39 : Using intervals of line indices to extract values from a module
************************************************************************************************************************)
open Needed_values ;;

let u1 = Needed_values.rf "Compilation_management/coma_state.ml";;
let u2 = Lines_in_string.indexed_lines u1 ;; 

let extract_interval ((i,j),_) =
   let temp1 = List.filter (fun (k,_)->(i<=k) && (k<=j)) u2 in 
   let temp2 = Image.image snd temp1 in 
   String.concat "\n" temp2 ;;

let ref_for_colombo = ref ([
   ((961, 985),   "compute_principal_ending");
   ((1024, 1031), "registrations_for_lonely_ending");
   ((2267, 2350), "Simplified_ts_creation")
]:(((int * int) * string) list)) ;;
let ref_for_curcuma = ref ([
  ((5, 616), "Automatic"); ((634, 634), "needed_libs_at_module");
   ((636, 636), "ancestor_at_module"); ((637, 637), "needed_dirs_at_module");
   ((659, 659), "ordered_list_of_modules"); ((671, 671), "root");
   ((792, 801), "find_needed_data"); ((925, 959), "PrivateTwo");
   ((1034, 1055), "complete_id_during_new_module_registration");
   ((1394, 1466), "register_mlx_file_on_monitored_modules");
   ((1842, 1874), "Try_to_register")
]:(((int * int) * string) list)) ;;
let ref_for_replacements = ref([]:((string * string) list));;

ref_for_replacements:=[
  "=registrations_for_lonely_ending ","=Colombo.registrations_for_lonely_ending ";
  "=md_compute_modification_times ","=Colombo.md_compute_modification_times ";
  "=md_associated_modification_time ","=Colombo.md_associated_modification_time ";
  "= md_compute_modification_time ","= Colombo.md_compute_modification_time ";
  "=compute_principal_ending ","=Colombo.compute_principal_ending ";
  "= ocamldebug_printersfile_path ","= Colombo.ocamldebug_printersfile_path ";
] ;;


type spice = Colombo | Curcuma ;;

let associated_ref = function 
   Colombo -> ref_for_colombo 
  |Curcuma -> ref_for_curcuma ;; 

let spice_to_string = function 
  Colombo -> "Colombo" 
 |Curcuma -> "Curcuma" ;; 

let haddock_order = 
    let oi = Total_ordering.for_integers 
    and prod = Total_ordering.product in 
    prod (prod oi oi) Total_ordering.lex_for_strings ;; 

 let add_interval ((i,j),name) spice=
  let raf = associated_ref spice in 
    (raf:= Ordered.insert haddock_order ((i,j),name)
      (!raf)) ;;

let copy_whole spice=
   let temp1 = Image.image (extract_interval) (!(associated_ref spice)) in
   let whole = String.concat "\n\n" temp1 
   and s = spice_to_string spice in 
   let corrected_whole = Replace_inside.replace_several_inside_string
    (!ref_for_replacements) whole in 
   Replace_inside.overwrite_between_markers_inside_file
   (Overwriter.of_string corrected_whole)
   ("(* Beginning of "^s^" *)\n\n","\n\n(* End of "^s^" *)")
   (Absolute_path.of_string "Fads/pan.ml") ;;

let main ((i,j),name) spice=
   let _= add_interval ((i,j),name) spice in copy_whole spice ;;


(*


*)

(*

main ((961,985),"compute_principal_ending") Colombo;;  
main ((1024,1031),"registrations_for_lonely_ending") Colombo;; 
main ((1655,1658),"ocamldebug_printersfile_path") Colombo;;  
main ((2267,2350),"Simplified_ts_creation") Colombo;;     

main ((5,616),"Automatic") Curcuma;; 
main ((654,654),"set_product_up_to_date_at_module") Curcuma;; 
main ((629,629),"subdir_at_module") Curcuma;; 
main ((630,630),"principal_ending_at_module") Curcuma;; 
main ((631,631),"mli_presence_at_module") Curcuma;; 
main ((634,634),"needed_libs_at_module") Curcuma;; 
main ((636,636),"ancestor_at_module") Curcuma;; 
main ((637,637),"needed_dirs_at_module") Curcuma;; 
main ((649,649),"set_needed_libs") Curcuma;; 
main ((651,651),"set_ancestors_at_module") Curcuma;; 
main ((653,653),"set_needed_dirs") Curcuma;; 
main ((655,656),"set_directories") Curcuma;; 
main ((659,659),"ordered_list_of_modules") Curcuma;; 
main ((660,660),"follows_it") Curcuma;; 
main ((661,661),"all_used_subdirs") Curcuma;; 
main ((671,671),"root") Curcuma;; 
main ((680,685),"endingless_at_module") Curcuma;; 
main ((691,697),"check_ending_in_at_module") Curcuma;; 
main ((720,723),"registered_endings_at_module") Curcuma;; 
main ((781,790),"modules_with_their_ancestors") Curcuma;; 
main ((792,801),"find_needed_data") Curcuma;; 
main ((805,813),"needed_dirs_and_libs_in_command") Curcuma;; 
main ((913,917),"compute_subdirectories_list") Curcuma;; 
main ((919,922),"check_registrations") Curcuma;; 
main ((925,959),"PrivateTwo") Curcuma;; 
main ((987,1007),"complete_info") Curcuma;; 
main ((1034,1055),"complete_id_during_new_module_registration") Curcuma;; 
main ((1378,1389),"printer_equipped_types_from_data") Curcuma;; 
main ((1394,1466),"register_mlx_file_on_monitored_modules") Curcuma;; 
main ((1468,1653),"Modern") Curcuma;; 
main ((1661,1797),"Ocaml_target_making") Curcuma;; 
main ((1842,1874),"Try_to_register") Curcuma;; 

*)

(************************************************************************************************************************
Snippet 38 : Remove all snippets containing a given substring (todo : integrate it
into the Manage_diary module directly)
************************************************************************************************************************)
open Needed_values ;;

let ap_for_diary = Absolute_path.of_string "Githubbed_archive/diary_archive.ml";;
let (g1,Manage_diary.Private.D g2) =  Manage_diary.Private.read_and_parse ap_for_diary ;;
let g3 = Int_range.index_everything g2;;
let g4 = List.filter (fun (j,(x,y))->Substring.is_a_substring_of "Vdw_" y) g3 ;;
let g5 = Image.image fst g4 ;;
let act1 () = Manage_diary.remove_snippets g5;;

let ap1 = Absolute_path.of_string "Fads/cloth.ml" ;;
let act1 () = Manage_diary.extract_at_index_and_append_to_file 
   84 ap1 ;;


let ap1 = Manage_diary.Private.usual_path ;;
let (prologue,diary1) = Manage_diary.Private.read_and_parse ap1 ;;
let (Manage_diary.Private.D l_diary1) = diary1 ;;
let z1 = List.nth l_diary1 (84-1) ;;



(************************************************************************************************************************
Snippet 37 : Search/replace following some module refactoring
************************************************************************************************************************)
open Needed_values ;;

let aps = ref [] ;;
let list_for_reps = ref [] ;;
aps := (Image.image (fun s->Absolute_path.of_string s) 
  [
    "ordered.ml";
    "Van_der_Waerden/Width_up_to_four/vdw_nonempty_index.ml";
    "Van_der_Waerden/vdw_common.ml";
    "Ordered_Lists/functor_for_sets.ml";
    "Ocaml_analysis/follow_ocaml_values.ml";
  ] );;
list_for_reps := [
  "Total_ordering.t)","Total_ordering_t.t)";
  "Total_ordering.t )","Total_ordering_t.t )";
  "Total_ordering.t\r","Total_ordering_t.t\r";
] ;;


let act1 () = List.iter 
  (Replace_inside.replace_several_inside_file 
     (!list_for_reps)) (!aps);

(************************************************************************************************************************
Snippet 36 : Extracting modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line "Van_der_Waerden/Width_up_to_four";;

let u1 =ae () ;;

let u2 = List.filter (
  fun eless ->
    Dfa_subdirectory.begins_with (Dfn_endingless.to_subdirectory eless) sd1
) u1;; 

let u3 = Image.image (
   fun eless -> Dfa_module.to_line(Dfn_endingless.to_module eless)
) u2 ;;

(************************************************************************************************************************
Snippet 35 : Remove all "automatic" modules 
************************************************************************************************************************)
open Needed_values ;;

let u1 = ae ();;
let u2 = Image.image (fun eless ->
   Dfa_module.to_line(Dfn_endingless.to_module eless)  
) u1;;
let u3 = List.filter (
  fun x-> Supstring.ends_with x "_automatic"
) u2 ;;

let computed_u3 = ["concrete_object_automatic"; "fw_wrapper_automatic"; "coma_state_automatic";
"fw_nonmodular_wrapper_automatic"; "hex_flattened_end_strategy_automatic"];;

let g1 = vfm "hex_flattened_end_strategy_automatic" ;;
let g2 = Image.image fst g1 ;;

let g3 = Image.image (fun x-> Replace_inside.replace_inside_string ("x",x) "let x = Automatic.x ;;") g2;;
let g4 = String.concat "\n" g3 ;;
let g5 = "\n\n\n" ^ g4 ^ "\n\n\n" ;; 

let h1 = List.flatten (Image.image snd g1) ;;
let h2 = Ordered.sort Total_ordering.standard h1 ;;
let h3 = List.iter (
  fun fn -> Replace_inside.replace_inside_file ("Hex_flattened_end_strategy_automatic.","Hex_flattened_end_strategy.") fn
) h2 ;;


(************************************************************************************************************************
Snippet 34 : Typical use of the Manage_diary module
************************************************************************************************************************)
let act1 () = Manage_diary.fix_indexation ();;

let act2 () = Manage_diary.remove_snippets [ (* put indices here *)];;


let diary_text = Io.read_whole_file ap_for_diary ;;

let (g1,g2) =  Manage_diary.Private.read_and_parse ap_for_diary ;;

(************************************************************************************************************************
Snippet  33 : Deduce the lower measure from the usual measure (related to Vdw)
************************************************************************************************************************)
let measure n =
  if n<1 then 0 else 
  let q=(n/9) in 
  match n mod 9 with
   0 -> 4*q+1 
  |1 -> 4*q+1
  |2 -> 4*q+2  
  |3 -> 4*q+2
  |4 -> 4*q+3
  |5 -> 4*q+4
  |6 -> 4*q+4  
  |7 -> 4*q+4
  |8 -> 4*q+4 
  | _ -> failwith("unforeseen");;   

let lower_measure n =
   if n<1 then 0 else 
   let q=(n/9) in 
   match n mod 9 with
    0 -> 4*q
   |1 -> 4*q
   |2 -> 4*q 
   |3 -> 4*q
   |4 -> 4*q+1
   |5 -> 4*q+1
   |6 -> 4*q+2  
   |7 -> 4*q+2
   |8 -> 4*q+3 
   | _ -> failwith("unforeseen");;  



let compute_lower_measure n = 
  let tempf = (fun t->measure(n+t)-measure(t)) in 
  snd(Min.minimize_it tempf (Int_range.range 1 20)) ;;   


(************************************************************************************************************************
Snippet  32 : Relocate all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line 
   "Van_der_Waerden" ;;

let sd2 = Dfa_subdirectory.of_line 
   "Van_der_Waerden/First_try" ;;

let u1 = ae () ;;   
let u2 = List.filter ( fun
  (Dfn_endingless_t.J(r,sd,m)) -> Dfa_subdirectory.begins_with sd sd1
) u1 ;;
let u3 = Image.image ( fun
(Dfn_endingless_t.J(r,sd,m)) -> Dfa_module.to_line m
) u2 ;;

let act1 () = Explicit.image (fun mn->relo mn sd2) u3 ;;

(************************************************************************************************************************
Snippet  31 : Delete all modules in a subdirectory
************************************************************************************************************************)
open Needed_values ;;

let sd1 = Dfa_subdirectory.of_line 
   "Ocaml_analysis/Standardized_concrete_ocaml_types" ;;

let u1 = ae () ;;   
let u2 = List.filter ( fun
  (Dfn_endingless_t.J(r,sd,m)) -> Dfa_subdirectory.begins_with sd sd1
) u1 ;;
let u3 = List.rev_map ( fun
(Dfn_endingless_t.J(r,sd,m)) -> Dfa_module.to_line m
) u2 ;;

let act1 () = fgs u3 ;;


(************************************************************************************************************************
Snippet  30 : Code from an abandoned, self-contained module
************************************************************************************************************************)
exception Too_many_arguments of int ;;

let wrap_in_parentheses_if_needed typename =
    if String.contains typename ' '
    then "( "^typename^" )"
    else  typename ;;    

let max_nbr_of_arguments = 7 ;;    

let arguments_in_input argname n=
    if n> max_nbr_of_arguments 
    then raise(Too_many_arguments(n))
    else let temp1 = Int_range.scale (fun k->
          if k<=n then argname^(string_of_int k) else "_") 1 max_nbr_of_arguments in 
         "(" ^ (String.concat "," temp1) ^ ")" ;;       

let listify is_a_list name =
    if not(is_a_list) 
    then name 
    else (wrap_in_parentheses_if_needed name)^" list" ;;     

let add_appendix_to_last_line appendix lines =
      let (last_line,other_lines) = Listennou.ht (List.rev lines) in 
      List.rev ((last_line^appendix)::other_lines) ;;    

(************************************************************************************************************************
Snippet  29 : Permutations far (wrt Hamming distance) from shift with constants. 
************************************************************************************************************************)
open Needed_values ;;

let hamming_distance perm1 perm2 =
  let temp1 = List.combine perm1 perm2 in 
  List.length(List.filter (fun (x,y)->x<>y) temp1);;

let generic_translate n t  = (Int_range.range t n) @ (Int_range.range 1 (t-1))  ;;

let all_translates =Memoized.make (fun n -> Int_range.scale (generic_translate n) 1 n);;

let measure n perm = snd(Min.minimize_it (hamming_distance perm) (all_translates n)) ;;


let iii = Memoized.make Permutation.iii ;;

let ff = Memoized.make(fun n->
   let whole = iii n 
   and meas = Memoized.make (measure n) in 
   let m = snd(Max.maximize_it meas whole) in 
   Explicit.filter (fun perm->meas(perm)=m) whole);;   


let gg n = Chronometer.it ff n;;

let hh n = (measure n (List.hd(ff n)));;

Int_range.scale (fun x->(x,hh x)) 3 10;;

let hf n = List.hd(ff n) ;;

(************************************************************************************************************************
Snippet  28 : Mass inheritance from a Private submodule 
************************************************************************************************************************)
let z1 = 
  ["conventional_files_with_full_content";
   "conventional_files_with_minimal_content"; "debug_build_subdir";
   "exec_build_subdir"; "full_set_of_needed_dirs"; "git_ignored_subdirectories";
   "minimal_set_of_needed_dirs"; "rootless_path_for_loadingsfile";
   "rootless_path_for_parametersfile"; "rootless_path_for_printersfile";
   "rootless_path_for_targetfile"; "usual_build_subdir"; "utility_files_subdir"] ;;
  
let z2 = Image.image (fun x->" let "^x^" = Private."^x^" ;;") z1;; 
  
let z3 = "\n\n\n" ^ (String.concat "\n" z2) ^ "\n\n\n" ;; 

(************************************************************************************************************************
Snippet  27 : Typical use of the Other_coma_state module 
************************************************************************************************************************)
let act1 () = 
   Other_coma_state.repopulate 
   (Needed_data_summary_t.Everything);;

(* or Other_coma_state.initialize () ;; *)   

let see = Other_coma_state.see_yet_unofficial_changes ();; 
let act2 () = Other_coma_state.officialize_changes ();;

Other_coma_state.Private.ref_for_unofficial_changes :=
(
   Some [ ]
)
;;

Other_coma_state.Private.ref_for_unofficial_changes :=
(
   Some ["Decomposed_filename/dfa_subdirectory.ml";
   "Filewatching/fw_with_batch_compilation.ml";
   "Filewatching/fw_with_githubbing.ml"; "Filewatching/fw_with_persisting.ml";
   "Compilation_management/modify_coma_state.ml";
   "Compilation_management/usual_coma_state.ml";
   "Ocaml_analysis/read_needed_ocaml_files.ml";
   "Ocaml_analysis/compute_all_ocaml_items.ml";
   "Compilation_management/needed_data_summary.ml";
   "Compilation_management/create_world_copy.ml";
   "Compilation_management/other_coma_state.ml"; "needed_values.ml";
   "self_contained_module_copy.ml"]
)
;;




(************************************************************************************************************************
Snippet  26 : Testing freezing and unfreezing of world copies
************************************************************************************************************************)
open Needed_values ;;

let remote_dir = Dfa_root.of_line 
   (home^"/Teuliou/OCaml/Forgotten_projects/Html_scraping_project") ;;

(*

To store a "frozen" copy of the project in a separate directory.
You can combine this with a cp -R (which often will not suffice by itself since you 
also need the dependecies from other subdirectories).

*)

let sd= Dfa_subdirectory.of_line "Text_editing/Html_scraping";;

let g1 = Create_world_copy.frozen_copy (!ucs)
    ~destination:remote_dir 
    (Needed_data_summary_t.Selection([],
    [sd])) ;;
   
(*

Much later, you can "unfreeze" the project as follows

*)    

let g2 = Create_world_copy.unfreeze_copy (!ucs) remote_dir ;;

(*

Then, you can cd to the separate dir, launch utop in it, and enjoy.

*)


(************************************************************************************************************************
Snippet  25 : Remove interval of lines in a file 
************************************************************************************************************************)
let ap = Absolute_path.of_string "Imported/Aantron/aantron_markup.ml";;
let old_text = Io.read_whole_file ap ;;
let v1 = Lines_in_string.indexed_lines old_text ;;
let v2 = List.filter (fun (j,line)->(299<=j)&&(j<=338) ) v1 ;;
let v3 = Image.image (
   fun (j,line)->
      let i1 = Substring.leftmost_index_of_in "val " line 
      and i2 = Substring.leftmost_index_of_in ":" line in 
      Cull_string.trim_spaces(Cull_string.interval line (i1+4) (i2-1))
) v2 ;;
let tab = String.make 5 ' ' ;;
let v3 = Image.image (fun (j,line) -> 
  if Supstring.begins_with line tab
  then Cull_string.two_sided_cutting (tab,"") line
  else line   
    ) v2;;

let tab = String.make 7 ' ' ;;
let v4 = Ordered.sort Total_ordering.lex_for_strings v3;;
let v5 = Image.image (fun name -> tab^"let "^name^" = Aantron_encoding."^name^" ;;") v4;;
let old_snippet = String.concat "\n" (Image.image snd v2) ;;
let new_snippet = String.concat "\n"  v5;;
let act () = Replace_inside.replace_inside_file (old_snippet,new_snippet) ap ;; 

let ap = Absolute_path.of_string "Imported/Aantron/aantron_markup.ml";;
let old_text = Io.read_whole_file ap ;;
let v1 = Lines_in_string.indexed_lines old_text ;;
let v2 = List.filter (fun (j,line)->(299<=j)&&(j<=338) ) v1 ;;
let v3 = Image.image (fun (j,line)->Cull_string.trim_spaces line) v2 ;;
let v4 = List.filter (Substring.is_the_beginning_of "The value ") v3;;
let v5 = Image.image (
   fun line->
      let i1 = Substring.leftmost_index_of_in "`" line 
      and i2 = Substring.leftmost_index_of_in "'" line in 
      Cull_string.trim_spaces(Cull_string.interval line (i1+1) (i2-1))
) v4 ;;
let v6 = Ordered.sort Total_ordering.lex_for_strings v5;;
let v7 = Image.image (fun name -> "let "^name^" = Aantron_utility."^name^" ;;") v6;;
let v8 = "\n\n\n" ^ (String.concat "\n" v7) ^ "\n\n\n";;
let v9 = print_string v8 ;;

(************************************************************************************************************************
Snippet  24 : Removing module wrappers in a set of files
************************************************************************************************************************)
let remove_module_wrapper_in_text text =
  let lines = Lines_in_string.indexed_lines text in 
  let (i1,_)= Listennou.force_find (fun (_,line)->
    Supstring.begins_with (Cull_string.trim_spaces line) "module "
  ) lines in
  let (i2,_)= Listennou.force_find (fun (_,line)->
    Supstring.begins_with (Cull_string.trim_spaces line) "end"
  ) (List.rev lines) in 
  let selected_lines = Option.filter_and_unpack (
    fun (i,line)->if List.mem i [i1;i2] then None else Some line
  ) lines in 
  String.concat "\n" selected_lines ;;

let remove_module_wrapper_in_file ap =
  let old_text = Io.read_whole_file ap in 
  let new_text = remove_module_wrapper_in_text old_text in 
  Io.overwrite_with ap new_text ;;

let the_dir = Directory_name.of_string ((Sys.getcwd())^"/Imported/Aantron/Temp"  ) ;;
let u1 = More_unix.simple_ls the_dir ;;

let act1 () = List.iter remove_module_wrapper_in_file u1 ;;

(************************************************************************************************************************
Snippet  23 : Sorting names in the dictionary order
************************************************************************************************************************)
let z1 = Ordered.sort Total_ordering.lex_for_strings 
[
  "current_state";
  "emit";
  "push_and_emit";
  "pop";
  "emit_end";
  "initial_state";
  "document_state";
  "doctype_state";
  "root_state";
  "after_root_state";

] ;;

let z2 = "\n\n\n" ^ (String.concat "\n" z1) ^ "\n\n\n" ;;

print_string z2;;

(************************************************************************************************************************
Snippet  22 : Remove phpbb links to footnotes 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "\n[b][color=blue]("^sk^")[/color][/b]\n" ;;

let reps = Int_range.scale (fun j->(write1 j,"")) 1 43  ;;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Translations/";;  
let ap1 =   Absolute_path.create_file_if_absent (dir^"/notes_to_dot.txt") ;;

let text1= Io.read_whole_file ap1;;
let lines1 = Lines_in_string.indexed_lines text1;;

let act1 () = Replace_inside.replace_several_inside_file reps ap1;;




(************************************************************************************************************************
Snippet  21 : Typical use of Html_to_phpbb.translate
************************************************************************************************************************)
open Needed_values ;;

let u1 = rf (home^"/Teuliou/html_files/Fenton/divine_origin.html");;

let u2 = Html_to_phpbb.translate u1;;

let ap1 = Absolute_path.of_string 
  (home^"/Teuliou/html_files/Translations/divine_origine_translated.txt") ;;

Io.overwrite_with ap1 u2;;  

(************************************************************************************************************************
Snippet  20 : Interaction between "beginning" and "end" of a large tex file
************************************************************************************************************************)
open Needed_values;;

let beg_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/beginning_of_text.txt");; 
 
let end_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/end_of_text.txt");; 

let tex_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/blet_pius_xii.tex");;   

let cmd_for_texshop = "osascript "^home^"/Teuliou/Bash_scripts/Automation/compile_with_texshop.scpt";;

let loop () =
    let beg_part = Io.read_whole_file beg_ap 
    and end_part = Io.read_whole_file end_ap in 
    let whole = beg_part ^ "\n" ^ end_part in 
    let _ =Io.overwrite_with tex_ap whole in 
    Sys.command cmd_for_texshop;;

let tr k = More_io.transfer_first_lines_of_to k end_ap beg_ap;;

let ll k = let temp = Lines_in_string.interval (Io.read_whole_file end_ap) k k in 
  (temp,Strung.explode temp);;

let rye (a,b) = Replace_inside.replace_inside_file (a,b) end_ap ;; 

let rblap () = Remove_blank_lines_around_percents.in_file end_ap ;;

let rlc pattern = 
   let _ = Lines_in_string.remove_lines_containing_substring_in_file 
   pattern end_ap in rblap ();;

let usual_cleaning () =
   Replace_inside.replace_several_inside_file 
   [
     (".! ",".1 ");
     (".!\n",".1 ");
    ("\012","");
    ("_","");
    ("#","");
    ("\\Vhat","What");
    ("\\Vhen","When");
    ("\\Vhile","While");
    ("\194\162","c");
    ("\226\128\156","\194\171");
    ("\226\128\157","\194\187");
    ("$","\194\167");
    (" & "," \\& ");
    ("&\226\128\153","d'");
    ("\\xii. ","xii. ");
    ("\194\165","V");
    ("\n1}","\n1 ");
    ("\n}","\n1 ");
    ] end_ap ;;


(************************************************************************************************************************
Snippet  19 : Add blank space at the beginning of lines (to make copy&paste easier )
************************************************************************************************************************)
open Needed_values;;

let blanks = String.make 3 ' ';; 

let reform_line x=
  if (x="")||(Supstring.begins_with x blanks) then x else blanks^x;; 

let reform_string s=
  let temp1 = Lines_in_string.lines s in 
  let temp2 = Image.image reform_line temp1 in 
  String.concat "\n" temp2 ;;


let the_ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/PDF_files/Printable/Preparation/greek_in_vl.txt");; 

let old_text = Io.read_whole_file the_ap ;;

let new_text = reform_string old_text ;;

Io.overwrite_with the_ap new_text;; 


(************************************************************************************************************************
Snippet  18 : Delete some HTML footnotes (with their links) and reindex
************************************************************************************************************************)
open Needed_values;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 
let old_text = Io.read_whole_file ap1 ;;

let u1 = Enumerate_html_links_to_footnotes.main old_text ;;

let see = Image.image (fun ((i_start,i_end),link_idx)->
    Cull_string.interval old_text i_start i_end) u1 ;; 

let bad_indices = [1;3;4;5;10;11;18;20] ;;

let u2 = List.filter (fun ((i_start,i_end),link_idx)-> 
    (List.mem link_idx bad_indices) ) u1;;
let u3 = Image.image fst u2 ;;
let u4 = Int_range.index_everything u3 ;; 
let u5 = Image.image (
  fun (k,(i_start,i_end))->((i_start,i_end),k)
) u4;;
let u6 = Image.image (fun ((i_start,i_end),link_idx)-> 
    ((i_start,i_end),List.assoc_opt (i_start,i_end) u5) 
) u1;;
let write_link opt = match opt with 
  None -> ""
  |Some(k) -> let sk=string_of_int k in 
              "<span id=\"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>";;

let u7 = Image.image ( fun (pair,opt)->(pair,write_link opt) ) u6;;

let new_text = Strung.replace_ranges_in u7 old_text ;;

Io.overwrite_with ap1 new_text ;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 
let old_text = Io.read_whole_file ap1 ;;
let v1 = Enumerate_html_footnotes.main old_text ;;
let see = Image.image (fun ((i_start,i_end),_)->
    Cull_string.interval old_text i_start i_end) v1 ;;   
let good_indices = List.filter (fun k->not(List.mem k bad_indices )) (Int_range.range 1 (List.length v1));;
let reindexation = Image.image (fun (i,j)->(j,i)) (Int_range.index_everything good_indices) ;;
let v2 = Image.image (
  fun ((footnote_idx,html_content),(i_start,i_end))->
    ((footnote_idx,html_content),(i_start,i_end),List.assoc_opt footnote_idx reindexation)
) v1;;
let write_reindexed_version ((i_start,i_end),(footnote_idx,html_content),opt_idx)=
   let new_text = (match opt_idx with 
      None -> ""
      |Some(k)->let sk=string_of_int k in 
      "<div id=\"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a> "^html_content^"</div>"
   ) in 
   ((i_start,i_end),new_text);;
let v3 = Image.image write_reindexed_version v2;;
let new_text = Strung.replace_ranges_in v3 old_text ;;
Io.overwrite_with ap1 new_text ;;





(************************************************************************************************************************
Snippet  17 : Remove contiguous lines in a file
************************************************************************************************************************)
open Needed_values ;;

let the_ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/Fortescue/papacy_451.html");; 

let old_text = Io.read_whole_file the_ap ;;

let to_be_deleted = Lines_in_string.interval old_text 3387 4948 ;;

Replace_inside.replace_inside_file (to_be_deleted,"") the_ap ;; 


(************************************************************************************************************************
Snippet  16 : Put fillable footnotes in an html draft 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "<span id=\"ln"^sk^"\"><a href=\"#n"^sk^"\">("^sk^")</a></span>"^
  "\n\n\n"^
  "<div id=\"n"^sk^"\"><a href=\"#ln"^sk^"\">("^sk^")</a>   \n\n "^
  "</div>" ;;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Fortescue";;  
let ap =   Absolute_path.create_file_if_absent (dir^"/pra_filled.html") ;;

let memo = String.concat "\n\n" (Int_range.scale write1 121 170) ;;

Io.overwrite_with ap memo ;; 

(************************************************************************************************************************
Snippet  15 : Aggregate pages
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let workdir = home^"/Downloads/Adrifor";;

Coherent_pdf.workspace_directory := workdir ;;

Coherent_pdf.extract_page_range "main" (5,5) ;;

Coherent_pdf.implode ("p","") ;;

Coherent_pdf.merge ["part1";"part2";"part3"] "whole";;

(************************************************************************************************************************
Snippet  14 : Cleaning up and fixing a chaotic mass download
************************************************************************************************************************)
let downloads_s_dir = home ^ "/Downloads";; 

let u1 = More_unix.quick_beheaded_complete_ls downloads_s_dir  ;;
let u2 = List.filter (Substring.is_the_beginning_of "iau") u1;;

let p_value s =
     let j1 = Substring.leftmost_index_of_in_from "-" s 5 in 
     let j2 = Substring.leftmost_index_of_in_from "-" s (j1+1) in
     int_of_string(Cull_string.interval s (j1+1) (j2-1));; 

let min_pageNumber = 9 and max_pageNumber = 70 ;; 

let pre_u3 = Image.image (fun s->(s,p_value s)) u2 ;;
let (bad_ones1,u3) = List.partition (fun (s,p)->(p<min_pageNumber) || (p>max_pageNumber)) pre_u3 ;;
let cmds1 = Image.image (fun (s,_)->"rm "^downloads_s_dir^"/"^s) bad_ones1;;
let act1 () = Image.image Sys.command cmds1 ;;

let reached_page_numbers = Ordered.sort Total_ordering.for_integers (Image.image snd u3) ;; 

let u4 = Int_range.scale (
   fun p->(p,Option.filter_and_unpack (fun (s,q)->if q=p then Some s else None) u3)
) min_pageNumber max_pageNumber;;

let u5 = List.filter (fun (p,representatives) -> List.length(representatives)>1) u4 ;;
let bad_ones2 =List.flatten 
  (Image.image (fun (p,representatives) -> List.tl(representatives)) u5);;
let cmds2 = Image.image (fun s->"rm "^downloads_s_dir^"/"^s) bad_ones2;;
let act2 () = Image.image Sys.command cmds2 ;;

let bad_ones3 = Option.filter_and_unpack 
  (fun (p,representatives) -> 
     if List.length(representatives)=0 then Some p else None) u4 ;;

    
let cmds3 = Image.image (fun (p,l)->
    let fn = List.hd l in 
    let sk = string_of_int(p-6) in 
    "mv "^downloads_s_dir^"/"^fn^" "^downloads_s_dir^"/p"^sk^".pdf") u4;;
let act3 () = Image.image Sys.command cmds3 ;;

let workdir = home^"/Downloads/";;

Coherent_pdf.workspace_directory := workdir ;;

Coherent_pdf.implode ("p","") ;;



(************************************************************************************************************************
Snippet  13 : Update footnote format in old phpbb text
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME" ;;

let ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/OCRed_texts/barenton_on_loisy.txt");;

let main_text = Io.read_whole_file ap ;; 

let opening_tag= "[color=blue]";;
let closing_tag = "[/color]";;

let u1 = Substring.occurrences_of_in opening_tag main_text ;;
let u2 = Substring.occurrences_of_in closing_tag main_text ;;

let u3 = List.combine u1 u2;;
let opening_length = String.length opening_tag ;;
let closing_length = String.length closing_tag ;;
let u4 = Image.image (fun (old_a,old_b)->
    let a = old_a+opening_length and b=old_b-1 in 
    ((a,b),Cull_string.interval main_text a b)
    ) u3;;
exception RA of string ;; 
    
let rhine_analysis s=
  let j1 = Substring.leftmost_index_of_in "(" s in 
  let j2 = Substring.leftmost_index_of_in ")" s in 
  if (j1<0)||(j2<0) then raise(RA(s)) else    
  let idx = int_of_string(Cull_string.interval s (j1+1) (j2-1)) in 
  (idx,Cull_string.cobeginning j2 s);;

let u5 = Image.image (
   fun ((a,b),text) -> (((a,b),text),rhine_analysis text)
)  u4 ;; 

let (redundant_u6,redundant_u7)=List.partition (fun (_,(idx,content))->content="") u5;;

let u6 = Image.image (fun (((a,b),text),(idx,content))->
      ((a-opening_length,b+closing_length),idx) ) redundant_u6;;

let u7 = Image.image (fun (((a,b),text),(idx,content))->
        ((a-opening_length,b+closing_length),idx,content) ) redundant_u7;;      

let check1 = ( (List.length u6) = (List.length u7) ) ;;        

let u8 = Image.image (
   fun ((a,b),idx,content) ->
      let s_idx=string_of_int idx in 
      ((a,b),"[size=90][b][color=blue]("^s_idx^")[/color][/b]"^content^"[/size]")
) u7;;
let corrected_text = Strung.replace_ranges_in u8 main_text;;

Io.overwrite_with ap corrected_text ;;



(************************************************************************************************************************
Snippet  12 : Combinatorial musings
************************************************************************************************************************)
exception Hard_computation of string * int ;;

let translate_all t (n,sols)=
  let increment = (if t="1" then 1 else 0) in  
  (n+increment,Image.image (fun u->t^u) sols) ;;

let synthesize_after_translating (n1,sols1) (n2,sols2) =
    if n1 < n2 then (n2,sols2) else 
    if n2 < n1 then (n1,sols1) else (n1,sols1@sols2);; 

let synthesize res1 res2 = 
    synthesize_after_translating 
     (translate_all "0" res1) (translate_all "1" res2);; 


let main_hashtbl = ((Hashtbl.create 50): (string * int, int * (string list)) Hashtbl.t);; 

let am x y = Hashtbl.add main_hashtbl x y;;

let eval_at_one pattern =
     if pattern="" then (1,["1"]) else 
     (if (String.get pattern 0)='F' then  (1,["1"]) else (0,["0"]) );;

let enforce_conditions pattern = 
    let m = String.length pattern in 
    let temp2 = Int_range.scale (fun j->
        if (j<5)&&(j<>2) then "N" else 
        if j>m then "F" else Cull_string.interval pattern j j) 1 (max 4 m) in 
    String.concat "" temp2;;     

(*

enforce_conditions "A";;
enforce_conditions "ANPE";;

*)

let prepare_computation pattern=
   if pattern="" then ("",Some"NFNN") else 
   let tail = Cull_string.cobeginning 1 pattern in 
   (
    if (String.get pattern 0)='F'
    then (tail,Some(enforce_conditions tail))  
    else (tail,None)    
   )      ;;

let left_n_decomposition pattern =
     let j1 = Strung.char_finder_from (fun c->c<>'N') pattern 1 in 
     if j1=0 
     then (String.length pattern,"") 
     else (j1-1,Cull_string.cobeginning (j1-1) pattern);; 

(*

left_n_decomposition "ABC";;
left_n_decomposition "NNABC";;

*)


let eval_quickly pattern n =
    if n=0 then (0,[""]) else
    if n=1 then eval_at_one pattern else
    match Hashtbl.find_opt main_hashtbl (pattern,n) with 
     Some(l,sol)->(l,sol)
     |None -> raise(Hard_computation(pattern,n)) ;;

let eval_using_left_n_decomposition old_pattern n =
    if n=0 then (0,[""]) else
    if n=1 then eval_at_one old_pattern else
    let old_length = String.length old_pattern in 
    let pattern = (if old_length > n 
                   then Cull_string.beginning n old_pattern
                   else old_pattern) in 
    let (number_of_ns,core) = left_n_decomposition pattern in 
    let (size_of_sols,old_sols) = eval_quickly core (n-number_of_ns) in 
    let new_sols =(
       if number_of_ns=0 
      then old_sols 
      else  let offset = String.make number_of_ns '0' in 
      Image.image (fun t->offset^t) old_sols
      ) in 
    (size_of_sols,new_sols);; 


let eval_slowly pattern n =
       try eval_using_left_n_decomposition pattern n with _->
         let (passive_case,opt_active_case) = prepare_computation pattern in 
         let case0 =  translate_all "0" (eval_using_left_n_decomposition passive_case (n-1)) in 
         match opt_active_case with 
         None -> case0
        |Some(active_case) ->
          let case1 =  translate_all "1" (eval_using_left_n_decomposition active_case (n-1)) in 
          synthesize_after_translating case0 case1;;     

let consider pattern n=
   let res = eval_slowly pattern n in 
   let _= (am (pattern,n) res) in res ;;

let ff n = eval_slowly "" n;;

let bf n = Image.image ff (Int_range.range 1 n);;



consider "" 2 ;;
consider "" 3 ;;
consider "FN" 2;;
consider "" 4;;
for k=3 to 30 do let _ = consider "FNN" k in ();let _=consider "" (k+2) in () done ;;


let res1 = Int_range.scale (fun x->fst(ff x)) 1 30;;




(************************************************************************************************************************
Snippet  11 : Massive conversion of audios into videos using ffmepgs
************************************************************************************************************************)
let base1 =
  [
  
  "001_Ier_D_de_l_Avent_01_12_2013_27min33.mp";
  "002_IIe_dimanche_de_l_Avent_Moulins_09_12_1990_35min32.mp";
  "003_IIIe_dimanche_de_l_Avent_Gaudete_En_Anjou_16_12_1990_29min26.mp";
  "004_IIIe_dim_de_l_Avent_Vendee_16_12_2018.mp";
  "005_IVe_dimanche_de_l_Avent_Comparaison_du_temps_des_Patriarches_avec_le_notre_Moulins_23_12_1990_28min19_Copie_en_conflit_de_debian_2019_11_13.mp";
  "006_Vigile_Nativite_Choix_et_gouts_de_Dieu_Moulins_24_12_1990_18min52.mp";
  "007_Vigile_de_Noel_En_Vendee_24_12_2012_29min31.mp";
  "008_Messe_de_Minuit_Anniversaire_naissance_de_la_fille_ainee_de_l_Eglise_Moulins_25_12_1996_28min04.mp";
  "009_Messe_du_jour_de_Noel_Divinite_du_Christ_Seigneur_demontree_par_S_Paul_aux_Hebreux_a_partir_de_l_Ancien_Testament_Moulins_25_12_1996_19min48.mp";
  "010_Nativite_Messe_de_minuit_En_Vendee_25_12_2012_14min17.mp";
  "011_Nativite_Messe_du_jour_En_Vendee_25_12_2012_11min26.mp";
  "012_Dimanche_dans_l_Octave_de_la_Nativite_Dum_medium_silentium_Tours_31_12_1989_21min22.mp";
  "013_Dimanche_dans_l_octave_de_NoeI_30_12_2012_21min08.mp";
  "014_Dimanche_dans_l_Octave_de_NoeI_ND_de_l_Epine_29_12_2019_28_min.mp";
  "015_Circoncision_En_Vendee_01_01_2013_26min22.mp";
  "016_Saint_Nom_de_Jesus_En_Vendee_02_01_2013_22min24.mp";
  "017_Epiphanie_06_01_1996_38min27.mp"; "018_Epiphanie_07_01_90_29min34.mp";
  "019_Octave_de_l_Epiphanie_Manifestation_de_la_Divinite_de_NS_Moulins_13_01_1991_38min17.mp";
  "020_La_Sainte_Famille_Moulins_07_01_1996_31min33.mp";
  "021_IIe_dimanche_apres_l_Epiphanie_Mayenne_ND_de_l_Epine_19_01_2014_30min50.mp";
  "022_IIIe_dimanche_apres_l_Epiphanie_Noli_vinci_a_malo_sed_vince_in_bono_malum_25_01_90_34min31.mp";
  "023_Septuagesime_Deux_genres_de_conversion_Moulins_27_01_1991_28min27.mp";
  "024_Sexagesime_ND_de_Lourdes_Montee_de_l_esprit_anti_chretien_et_apparitions_de_ND_Moulins_11_02_1996_33min36.mp";
  "025_Quinquagesime_Annonce_prophetique_de_la_Passion_Moulins_10_02_1991_24min39.mp";
  "026_Ier_dimanche_de_Careme_Sens_mystique_des_montees_vers_Jerusalem_16_02_1997_15min18.mp";
  "027_Ier_dimanche_de_Careme_Sur_la_Penitence_Moulins_04_03_1990_24min53.mp";
  "028_IIe_Dim_de_Careme_Transfiguration_Equilibre_entre_desolations_et_consolations_Moulins_24_02_1991_34min49.mp";
  "029_IIIe_dimanche_de_Careme_Contre_le_demon_muet_Moulins_10_03_1996_26min07.mp";
  "030_IIIe_dimanche_de_Careme_Sur_l_Annonciation_Maternite_virginale_et_voeu_de_virginite_22_03_92.mp";
  "031_IVe_dimanche_de_Careme_Joie_dans_la_penitence_et_Montee_du_Carmel_10_03_91_26min06.mp";
  "032_Ier_dimanche_de_la_Passion_ReveIation_progressive_de_la_divinite_de_NS_Moulins_05_04_1992_40min02.mp";
  "033_Dimanche_des_Rameaux_En_Vendee_01_04_2012_16min12.mp";
  "034_Veillee_pascale_Sur_l_illogisme_de_l_attitude_actuelle_des_Juifs_talmudistes_Moulins_11_04_1998_26min11.mp";
  "035_Paques_Moulins_04_04_1996_44min07.mp";
  "036_Dimanche_in_albis_Quasi_modo_Les_corps_glorieux_Tours_02_04_1989_19min22.mp";
  "037_IIe_dimanche_apres_Paques_Misericorde_et_Justice_de_Dieu_equilibre_de_l_esprit_chretien_Fete_de_saint_Pierre_de_Verone_29_04_1990_30min12.mp";
  "038_IIIe_dimanche_apres_Paques_Modicum_et_videbitis_Me_ND_de_l_Epine_Mayenne_21_04_2013_26min12.mp";
  "039_IVe_dimanche_apres_Paques_Attachement_apostolique_a_NS_pour_remonter_de_sa_nature_humaine_a_sa_Divinite_28_04_1991_23min03.mp";
  "040_Ve_dimanche_apres_Paques_Dieu_console_par_les_siens_Tours_30_04_1989_20min42.mp";
  "041_Ascension_24_05_1990_25min18.mp";
  "042_Ascension_Moulins_09_05_1991_27min55.mp";
  "043_Ascension_2011_Vendee_20min32.mp";
  "044_Ascension_ND_de_l_Epine_10_05_2018_26min12.mp";
  "045_Dimanche_dans_l_Octave_de_l_Ascension_12_05_2013_Mayenne_ND_de_l_Epine_15min28.mp";
  "046_Dimanche_dans_l_Octave_de_l_Ascension_2014_Vendee_19min.mp";
  "047_Pentecote_Moulins_19_05_1991_29min18.mp";
  "048_Pentecote_Vendee_19_05_2013_30min45.mp";
  "049_Pentecote_ND_de_l_Epine_15_05_2016_30min_50.mp";
  "050_Tres_Sainte_Trinite_Moulins_16_06_1990_33min05.mp";
  "051_Fete_du_Tres_Saint_Sacrement_Moulins_02_06_1991_28min53.mp";
  "052_Dimanche_dans_l_octave_du_Saint_Sacrement_Notre_Dame_de_l_Epine_2_06_2013_14min26.mp";
  "053_Solennite_du_Sacre_Coeur_et_Saint_Jean_Baptiste_Il_faut_qu_Il_croisse_et_que_je_diminue_Moulins_24_06_1990_24min55.mp";
  "054_Dimanche_dans_l_Octave_du_Sacre_Coeur_29min09.mp";
  "055_IVe_dimanche_apres_la_Pentecote_Moulins_16_06_1991_31min54.mp";
  "056_Ve_dimanche_apres_la_Pentecote_Sur_reparation_et_componction_Tours_18_06_1989_34min59.mp";
  "057_VIe_dimanche_apres_la_Pentecote_Saint_Henri_et_la_sanctification_dans_le_monde_Moulins_15_07_1990_37min39.mp";
  "058_VIIe_Dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_07_07_2013_36min04.mp";
  "059_VIIIe_dimanche_apres_la_Pentecote_Saint_Bonaventure_et_le_bonheur_en_Dieu_seul_Moulins_14_07_1991_42min06.mp";
  "060_IXe_dimanche_apres_la_Pentecote_Mayenne_ND_de_l_Epine_21_07_2013_34min55.mp";
  "061_Xe_dimanche_apres_la_Pentecote_Sur_le_Principe_et_Fondement_Tours_31_07_1988_22min08.mp";
  "062_XIe_dimanche_apres_la_Pentecote_04_08_1991_33min37.mp";
  "063_XIe_Dimanche_apres_Pentecote_12_08_2012_15min44.mp";
  "064_XIIe_dimanche_apres_la_Pentecote_11_08_2013_27min32.mp";
  "065_XIIIe_dimanche_apres_la_Pentecote_18_08_2013_27min34.mp";
  "066_XIVe_dimanche_apres_la_Pentecote_Saint_Louis_25_08_1991.mp";
  "067_XVe_dimanche_apres_la_Pentecote_Foi_en_la_Divinite_de_NS_Moulins_16_09_90_27min23.mp";
  "068_XVIe_dimanche_apres_la_Pentecote_03_09_89_24min15.mp";
  "069_XVIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_16_09_2018_40_min.mp";
  "070_XVIIIe_Dim_apres_la_Pentecote_ND_de_l_Epine_8_10_2017_38min30.mp";
  "071_XIXe_dimanche_apres_la_Pentecote_Sur_la_colere_24_09_89_22min01.mp";
  "072_XXe_D_ap_Pent_22_10_2017_ND_de_l_Epine_1h.mp";
  "073_Fete_du_Christ_Roi_27_10_1991_27min22.mp";
  "074_XXIe_dimanche_apres_Pentecote_25min24.mp";
  "075_XXIIe_Dimanche_apres_la_Pentecote_ND_de_l_Epine_21_10_2018_36_min_35.mp";
  "076_XXIIIe_dimanche_apres_la_Pentecote_Sur_le_Purgatoire_11_11_90.mp";
  "077_XXIVe_et_dernier_dimanche_apres_la_Pentecote_Moulins_26_11_1989_26min38.mp";
  "078_XXIVe_ap_Pent_IVe_ap_Eph_Tempete_apaisee_Moulins_29min55.mp";
  "079_XXVe_dim_ap_Pent_Ve_ap_Epiphanie_Moulins_04_02_1990_20min.mp";
  "080_XXVIe_dim_apres_la_Pentecote_VIe_ap_Epiphanie_17_11_1991_30min38.mp";
  "081_Solennite_du_Tres_Saint_Rosaire_Tours_08_10_1989_23min31.mp";
  "082_Maternite_Divine_de_Notre_Dame_11_10_2015_Mayenne_ND_de_l_Epine_28min20.mp";
  "083_Toussaint_Moulins_01_11_1996_30min49.mp";
  "084_Commemoration_des_defunts_Vendee_2_novembre_2012_21min07.mp";
  "085_Fete_de_la_Dedicace_de_Saint_Jean_de_Latran_09_11_2014_33min47.mp";
  "086_Solennite_de_l_Immaculee_Conception_Moulins_1989_30min05.mp";
  "087_Presentation_de_NS_au_temple_et_Purification_de_Marie_Mayenne_ND_de_l_Epine_02_02_2014_23min13.mp";
  "088_Decouverte_de_la_Sainte_Croix_ND_de_l_Epine_3_05_2015_34min28.mp";
  "089_St_Philippe_et_st_Jacques_ND_de_l_Epine_11_05_2014_18min42_.mp";
  "090_Solennite_de_Ste_Jeannes_d_Arc_Moulins_13_05_1990_27min49.mp";
  "091_VIe_dimanche_apres_la_Pentecote_Solennite_de_St_Pierre_et_St_Paul_Monde_conquis_de_haute_lutte_par_papes_et_martyrs_Moulins_30_06_1991_42min50.mp";
  "092_Fete_de_Sainte_Anne_Mayenne_ND_de_l_Epine_26_07_2015_42min41.mp";
  "093_Saint_Laurent_diacre_et_martyr_Vendee_10_08_2014_33min24.mp";
  "094_Fete_de_saint_Luc_Mayenne_ND_de_l_Epine_18_10_2015_27min09.mp";
  "095_Sur_la_maniere_de_precher_1988_22min32.mp";
  "096_Assomption_Moulins_15_08_1991_29min15.mp"; "097_Assomption_2012.mp";
  "098_Assomption_2013_Vendee_21min34.mp";
  "099_Saint_Joachim_Pere_de_la_TS_Vierge_Marie_Vendee_16_08_2015_24min03.mp";
  "100_Tres_Precieux_Sang_Mayenne_ND_de_l_Epine_1er_juillet_2013_27min03.mp";
  "101_Nativite_de_Notre_Dame_Moulins_08_09_1991_30min28.mp";
  "102_Notre_Dame_des_sept_douleurs_15_09_1996_15min51.mp";
  "103_Solennite_de_Ste_Therese_de_l_Enfant_Jesus_et_de_la_Ste_Face_Moulins_30_09_1990_22min47.mp";
  "104_Solennite_de_saint_Michel_Archange_Moulins_29_09_1991_32min31.mp";
  "105_IVe_D_ap_Pentecote_ND_de_l_Epine_28_juin_2020_35min.mp";
  "106_VIe_D_ap_Pentecote_ND_de_l_Epine_12_7_2020_39min25.mp";
  "107_refutation_T_de_M_ete_2013_Intro_10min.mp";
  "108_refutation_T_de_M_ete_2013_partie_1_18min19.mp";
  "109_refutation_T_de_M_ete_2013_partie_2_18min41.mp";
  "110_refutation_T_de_M_ete_2013_partie_3_23min22.mp";
  "111_refutation_T_de_M_ete_2013_partie_4_14min54.mp";
  "112_refutation_T_de_M_ete_2013_partie_5_21min57.mp";
  "113_refutation_T_de_M_ete_2013_partie_6_21min51.mp";
  "114_refutation_T_de_M_ete_2013_partie_7_17min02.mp";
  "115_VIIIe_D_Ap_Pent_7_8_2011_a_La_Boutouere_Mayenne_21min57.mp";
  "116_XVIe_Dimanche_apres_la_Pentecote_ND_de_lEpine_20_09_2020_22min14.mp";
  "117_Christ_Roi_25_10_2020_ND_de_lEpine_45min29.mp";
  "118_Presentation_des_ouvrages_de_labbe_Zins_video_001_52min.mp";
  "119_Presentation_des_ouvrages_de_labbe_Zins_video_002_1h.mp";
  "120_Presentation_des_ouvrages_de_labbe_Zins_video_003_55min.mp";
  "121_Presentation_des_ouvrages_de_labbe_Zins_video_004_42min.mp"
  
  ];;

let n1 = List.length base1 ;;

let main_list = String.concat "\n" base1 ;; 




let write1 x =  
    let idx = string_of_int(int_of_string(String.sub x 0 3)) in 
    "wget -c http://larchange.org/audio/"^x^"3\n"^
    "ffmpeg -loop 1 -i STP.jpg -i "^x^"3 -acodec copy -vcodec libx265 -shortest "^x^"4\n"^
    "rm "^x^"3\n"^
    "echo $'\\n\\n\\n\\n\\n\\n Step "^idx^" of 121 finished\\n\\n\\n\\n\\n\\n'" ;; 

let home = Sys.getenv "HOME" ;;    
let ap_for_main_script = Absolute_path.of_string 
    (home^"/Teuliou/Bash_scripts/Convert_audio_to_video/audiotovideo.sh");;    

let main_script = String.concat "\n\n\n" (Image.image write1 base1) ;;      

let fill_main_script () =
   Io.overwrite_with ap_for_main_script main_script ;;

fill_main_script () ;;


let base2 = Int_range.index_everything base1;;
let (pre_part1,remains1) = List.partition (fun (j,x)->j<=40) base2;;
let (pre_part2,pre_part3) = List.partition (fun (j,x)->j<=80) remains1;;
let part1 = Image.image (fun (_,s)->s^"4") pre_part1;;
let part2 = Image.image (fun (_,s)->s^"4") pre_part2;;
let part3 = Image.image (fun (_,s)->s^"4") pre_part3;;

let main_dir = (Sys.getenv "HOME")^"/Teuliou/Bash_scripts/Convert_audio_to_video/Sermons";;
let cmds1 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_1_a_40/"
) part1 ;;
let cmds2 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_41_a_80/"
) part2 ;;
let cmds3 = Image.image (
  fun x->"mv "^main_dir^"/"^x^" "^main_dir^"_81_a_117/"
) part3 ;;
let cmds = cmds1 @ cmds2 @ cmds3 ;;





(************************************************************************************************************************
Snippet  10 : Removing misinterpreted characters from a freshly OCR-ed doc
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME" ;;

let dirname = "Lossky";;
let num_of_pages = 196 ;;    

let partial_texts = Int_range.scale (fun k->
    let sk = string_of_int k in 
    let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
    let prelude="% Beginning of page "^sk^"\n"
    and postlude="\n% End of page "^sk in 
    prelude^(rf fn)^postlude)  1 num_of_pages ;;
      
let full_ap = Absolute_path.create_file_if_absent  
(home^"/Downloads/"^dirname^"/full.txt");;  
let tex_ap = Absolute_path.of_string 
  (home^"/Teuliou/html_files/PDF_files/Printable/Preparation/pre_vladimir_lossky.txt");; 
 

let full_text = "\n"^(String.concat "\n" partial_texts)^"\n" ;;


let adjusted_text = Replace_inside.replace_several_inside_string 
   ["&","\\&";
    "$","\\$";
    "#","\\#";
    "_","\\_";
    "\194\162","c";
    "\194\176","o";
    "\226\130\172","E"] full_text ;;   
 
Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string adjusted_text) ("% BEGINNING MARKER","%END MARKER") tex_ap;;


let text1 = Io.read_whole_file tex_ap ;;
let u1 = Substring.occurrences_of_in "% End of page 7\n" text1 ;;
let i1 = List.hd u1 ;;
let u2 = Cull_string.interval text1 (i1-20) i1;;

Replace_inside.replace_several_inside_file
   ["\n\012\n","\n"] tex_ap ;;

(************************************************************************************************************************
Snippet  9 : Typical use of the Trim_text_between_tags module
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let ap = Absolute_path.of_string 
(home^"/Teuliou/html_files/Translations/act_of_body_translated.txt");;

Trim_text_between_tags.in_file [("[i]","[/i]")] ap;;


(************************************************************************************************************************
Snippet  8 : Put fillable footnotes in a phpbb draft 
************************************************************************************************************************)
let write1 k=
  let sk = string_of_int k in 
  "[b][color=blue]("^sk^")[/color][/b]\n\n"^
  "[size=90][b][color=blue]("^sk^")[/color][/b]   [i]   [/i]   [/size]";;

let dir = (Sys.getenv "HOME")^"/Teuliou/html_files/Translations";;  
let ap =   Absolute_path.create_file_if_absent (dir^"/temp.txt") ;;

let memo = String.concat "\n\n" (Int_range.scale write1 1 5) ;;

Io.overwrite_with ap memo ;; 

(************************************************************************************************************************
Snippet  7 : Finding extremal vertices in a polytope
************************************************************************************************************************)
open Needed_values ;;


let small_n=1;;

let u1 = Cartesian.fifth_power (Int_range.range 0 small_n);;

let u2 = Option.filter_and_unpack (
  fun (a1,a2,a3,a4,a5)->
      let a6 = a3+a4-a5
      and a7 = a2+a4-a5
      and a8 = a1-a4+a5 in
      if List.for_all (fun x->(x>=0)) [a6;a7;a8] 
      then Some(a1,a2,a3,a4,a5,a6,a7,a8)   
    else None
) u1 ;;


let u3 = List.tl u2 ;; 

let shadow (a1,a2,a3,a4,a5,a6,a7,a8) =
    let l = [a1;a2;a3;a4;a5;a6;a7;a8] in 
    Set_of_polys.safe_set(List.filter (fun j->List.nth l (j-1)=0) [1; 2; 3; 4; 5; 6; 7; 8]) ;;

let supporting_rel uple uple2 =
    if uple=uple2 then false else 
    Set_of_polys.is_included_in (shadow uple) (shadow uple2);;  
     
let supporters uple = List.filter (supporting_rel uple ) u3;;

let u4 = Option.filter_and_unpack (
   fun uple -> let r= supporters uple in 
   if r<>[]
    then Some(uple,r)
  else None
) u3;;

let u5 = List.filter (fun uple->(supporters uple)=[]) u3;;

[[0, 0, 0, 1, 1, 0, 0, 0],
   [0, 0, 1, 0, 0, 1, 0, 0], [0, 0, 1, 1, 1, 1, 0, 0],
   [0, 1, 0, 0, 0, 0, 1, 0], [0, 1, 0, 1, 1, 0, 1, 0],
   [0, 1, 1, 0, 0, 1, 1, 0], [0, 1, 1, 0, 1, 0, 0, 1],
   [0, 1, 1, 1, 1, 1, 1, 0], [1, 0, 0, 0, 0, 0, 0, 1],
   [1, 0, 0, 1, 0, 1, 1, 0], [1, 0, 0, 1, 1, 0, 0, 1],
   [1, 0, 1, 0, 0, 1, 0, 1], [1, 0, 1, 1, 0, 2, 1, 0],
   [1, 0, 1, 1, 1, 1, 0, 1], [1, 1, 0, 0, 0, 0, 1, 1],
   [1, 1, 0, 1, 0, 1, 2, 0], [1, 1, 0, 1, 1, 0, 1, 1],
   [1, 1, 1, 0, 0, 1, 1, 1], [1, 1, 1, 0, 1, 0, 0, 2],
   [1, 1, 1, 1, 0, 2, 2, 0], [1, 1, 1, 1, 1, 1, 1, 1]]

(*

let small_n=2;;

let u1 = Cartesian.fourth_power (Ennig.ennig 0 small_n);;

let u2 = Option.filter_and_unpack (
  fun (a1,a2,a3,a5)->
      let a4 = small_n-(a1+a2+a3) 
      and a6 = small_n-(a1+a2+a5)
      and a7 = small_n-(a1+a3+a5) 
      and a8 = (2*a1+a2+a3+a5)-small_n in
      if List.for_all (fun x->(x>=0)&&(x<=small_n)) [a4;a6;a7;a8] 
      then Some(a1,a2,a3,a4,a5,a6,a7,a8)   
    else None
) u1 ;;

let u3 = List.filter (
   fun (a1,a2,a3,a4,a5,a6,a7,a8) -> 
    List.exists (fun (x,y)->x<>y) [a1,a8;a2,a7;a3,a6;a4,a5]
) u2;;

*)

(************************************************************************************************************************
Snippet  6 : Abandoned code snippet to remove paragraph containing footnotes.
It is much simpler to add html paragraph tags only when the region of text
does not contain footnotes (see the Htmlize module and snippet 4)
************************************************************************************************************************)
exception Unbalanced_html_paragraph_tags of int * int ;;
exception Nested_html_paragraphs of (int * int) * (int * int) ;;

let footnote_marker = ref " nowfeetneto ";;


let html_par_opening_tag = "<p>";;
let html_par_closing_tag = "</p>";;

let op_tag_length = (String.length html_par_opening_tag)-1 ;;
let cl_tag_length = (String.length html_par_closing_tag)-1 ;;

let detect_nested_paragraphs l=
   let temp1 = Listennou.universal_delta_list l in 
   match Option.seek (fun 
     (((i1,j1),(i2,j2)),((i3,j3),(i4,j4)))->i3<j2
   ) temp1 with 
   None -> ()
   |Some(((i1,j1),(i2,j2)),((i3,j3),(i4,j4))) ->
       raise(Nested_html_paragraphs((i2,j2),(i3,j3)));;

let locate_all_paragraphs_in_html txt=
  (* paragraphs are assumed to be non-nested *)
  let temp1 = Substring.occurrences_of_in html_par_opening_tag txt 
  and temp2 = Substring.occurrences_of_in html_par_closing_tag txt in 
  let o1 = List.length temp1 and c1 = List.length temp2 in 
  if o1<>c1 
  then raise(Unbalanced_html_paragraph_tags(o1,c1))
  else   
  let temp3 = Image.image (fun i->(i,i+op_tag_length)) temp1
  and temp4 = Image.image (fun j->(j,j+cl_tag_length)) temp2 in 
  let temp5 = List.combine temp3 temp4 in 
  let _ = detect_nested_paragraphs temp5 in 
  temp5 ;; 

let remove_paragraphs_containing_footnotes txt l= ();;
        



(************************************************************************************************************************
Snippet  5 : Mass deletion of modules 
************************************************************************************************************************)
open Needed_values;;

let u1 = ae ();;

let u2 = List.filter (fun
   (Dfn_endingless_t.J(r,s,m)) -> Dfa_subdirectory.begins_with s 
   (Dfa_subdirectory_t.SD "Text_editing")
) u1 ;;

let u3 = Image.image (fun
(Dfn_endingless_t.J(r,s,m)) -> match m with 
  (Dfa_module_t.M m0) -> m0
) u2;;

let u4 = List.filter (
  fun m0->not(List.mem m0 ["control_pdf_size";"read_russian"])
) u3;;

let u5 = Image.image (
  fun m0->(m0, bel m0)
) u4;;

let u6 = List.filter (
  fun (m0,b0)->List.for_all (fun m1->(List.mem m1 u4)) b0
) u5;;

let u7=List.rev(Image.image fst u6);;

(************************************************************************************************************************
Snippet  4 : Code to OCR-size PDF's into .txt (and later html)
************************************************************************************************************************)
open Needed_values ;;


let write1 k =
    let sk = string_of_int k in 
    "pdftoppm main.pdf p"^sk^" -png -f "^sk^" -singlefile\n"^
    "tesseract -l fra p"^sk^".png p"^sk;;

let dirname = "Pius_XII";;
let num_of_pages = 326 ;;

let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let text1 = "\n\n\n"^(String.concat "\n" (Int_range.scale write1 1 num_of_pages))^"\n\n\n" ;;   
    
Io.overwrite_with ap1 text1;;


let partial_texts = Int_range.scale (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  "%\n% Page "^sk^" \n%\n"^(rf fn))  7 num_of_pages ;;


let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/full.txt");;  

let full_text = String.concat "\n" partial_texts ;;
let full_text = Htmlize.pages partial_texts ;;

Io.overwrite_with full_ap full_text;;

let (page1,page2,ranges_for_lfm,ranges_for_fm) =
   Option.unpack(!(Htmlize.Private.error_handling_ref ));;

(* Re-indexed version *)

let main_list = 
  [51;149;187;189;201;203;231;249;257;261;263;265;269;271;297] ;;

let write1 j =
 let k =List.nth main_list (j-1) in 
 let sj = string_of_int j 
 and sk = string_of_int k in 
 "pdftoppm main.pdf p"^sk^" -png -f "^sj^" -singlefile\n"^
 "tesseract -l fra p"^sk^".png p"^sk;;

let dirname = "Blet_again";;
let num_of_pages = 15 ;;

let ap1 = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/script.sh");;

let text1 = "\n\n\n"^(String.concat "\n" (Int_range.scale write1 1 num_of_pages))^"\n\n\n" ;;   
 
Io.overwrite_with ap1 text1;;

let partial_texts = Int_range.scale (fun k->
  let sk = string_of_int k in 
  let fn = home^"/Downloads/"^dirname^"/p"^sk^".txt" in 
  let announcer = "%\n% Page "^sk^" \n%\n" in 
  let uncompressed_pagetext = rf fn in 
  let pagetext = Make_paragraphs_one_lined.in_string 
  (Remove_hyphens.in_string uncompressed_pagetext) in  
  announcer^pagetext)  1 num_of_pages ;;

let full_ap = Absolute_path.create_file_if_absent (home^"/Downloads/"^dirname^"/full.txt");;  

let full_text = String.concat "\n" partial_texts ;;
let full_text = Htmlize.pages partial_texts ;;

Io.overwrite_with full_ap full_text;;




(************************************************************************************************************************
Snippet  3 : Typical use of the Read_russian module
************************************************************************************************************************)
open Needed_values ;;

let home = Sys.getenv "HOME";;
let txt1 = rf (home^"/Downloads/temp.txt");;

let z1 = Read_russian.read txt1;;
let z2= Read_russian.prepare_dictation txt1;;

let ap1 = Absolute_path.of_string (home^"/Teuliou/LaTeX/Moullapl/archipelago.tex");;

let act () = 
Replace_inside.overwrite_between_markers_inside_file 
  (Overwriter.of_string z2)
  ("\\begin{document}","\\end{document}") ap1;;

(************************************************************************************************************************
Snippet  2 : Convert footnotes between phpBB and HTML
************************************************************************************************************************)
let peggy j =
   let sj=string_of_int j in 
   "<span id=\""^"ln"^sj^"\"><a href=\"#n"^sj^"\">("^sj^")</a></span>";;
 
 let u1 = Int_range.scale peggy 3 43;;  
 
 let u2 ="\n\n\n"^(String.concat "\n\n" u1) ^"\n\n\n";;
 
 
 let peggy j =
   let sj=string_of_int j in 
   "<div id=\""^"n"^sj^"\"><a href=\"#ln"^sj^"\">("^sj^")</a> <i> </i>  </div>";;
 
 let u1 = Int_range.scale peggy 3 43;;  
 
 let u2 ="\n\n\n"^(String.concat "\n\n" u1) ^"\n\n\n";;
 

(************************************************************************************************************************
Snippet  1 : Typical use of the Coherent_pdf module on a freshly scanned doc
************************************************************************************************************************)
let home = Sys.getenv "HOME" ;;
let workdir = home^"/Downloads/Building_Site";;

let workdir = home^"/Downloads/Arno/Towards";;

Coherent_pdf.workspace_directory := workdir ;;

let cmd1 = "cp "^workdir^"/moncunill.pdf "^workdir^"/whole.pdf";;

let act1 () = Sys.command cmd1;;

let act2 () = Coherent_pdf.remove_page_range_in_in_a_total_of 
~range_start:1 ~range_end:1 ~deflated_one:"whole" 
~total_length:28;;

let act3 () = Coherent_pdf.append_on_the_right "whole" "velaza" ;;

let act5 () = Coherent_pdf.transfer_range_to_rightmost
~range_start:91 ~range_end:91 ~deflated_one:"whole" 
~total_length:592 ~receiving_one:"p90" ;;

let act6 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "p90" ~receiving_one: "whole"
     ~page_number:89 ~initial_total_length:591;;

let act7 () = Coherent_pdf.delete_file "p90";;     

let act8 k = Coherent_pdf.replace_page_number_in_by 
~page_number:k ~receiving_one:"whole"  ~inserted_one:("p"^(string_of_int k)) 
~total_length:133;;

Image.image act8 [36;40;42;44;68;70;72;90;100] ;;

let act9 () = Coherent_pdf.extract_page_range "whole" (10,10);;

let act10 () = Coherent_pdf.extract_odd_pages "russia" ;;

let act11 () = Coherent_pdf.intertwine 
~odd_pages:"odd" ~even_pages:"even" 
~num_odd:81  ~num_even:81 ~final_name:"118_to_279" ;;

let phoebe n=
  let q =(n/4) in 
  let r= n - 4*q in 
  (4*q)+List.assoc r [0,(-3);1,2;2,3;3,4;];; 
    
let act12 () =   
   Explicit.image (
     fun k-> 
      let j = phoebe k in 
      Coherent_pdf.rename 
      ("p"^(string_of_int k)) ("q"^(string_of_int j))
   ) (Int_range.range 1 260) ;;

let act13 ()= Coherent_pdf.implode ("q","") ;; 

let workdir = home^"/Downloads";;

Coherent_pdf.workspace_directory := workdir ;;

let act1 () = Coherent_pdf.extract_page_range "mariage" (24,24);;

let act2 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "xx" ~receiving_one: "mariage"
     ~page_number:22 ~initial_total_length:732;;

let act3 () = Coherent_pdf.remove_page_range_in_in_a_total_of 
~range_start:25 ~range_end:25 ~deflated_one:"mariage" 
~total_length:733;;

let act4 ()= Coherent_pdf.insert_in_just_after 
    ~inserted_one: "p272" ~receiving_one: "mariage"
     ~page_number:315 ~initial_total_length:732;;


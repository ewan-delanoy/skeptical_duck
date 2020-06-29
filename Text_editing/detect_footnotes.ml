(*


#use"Text_editing/detect_footnotes.ml";;

*)


let check_for_footnote_at_idx s start_idx =
  let n=String.length s in 
  if (Strung.get s start_idx)<>'(' then None else 
  let opt1=Option.seek (
     fun k->let c=Strung.get s k in 
     not(List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'])
  ) (Ennig.ennig (start_idx+1) n) in 
  if opt1=None then None else 
  let idx1=Option.unpack opt1 in 
  if (Strung.get s idx1)<>')' then None else   
  let pagenumber_description=Cull_string.interval s (start_idx+1) (idx1-1) in 
  let pagenumber=int_of_string pagenumber_description in 
  Some(pagenumber,(start_idx,idx1));;

(*

check_for_footnote_at_idx "(56)abc ...\n" 1;;

*)


let check_for_whitened_footnote_at_idx s start_idx =
   let n=String.length s in 
   let opt1=Option.seek (
     fun k->let c=Strung.get s k in 
     not(List.mem c [' ';'\n';'\t';'r'])
   ) (Ennig.ennig start_idx n) in 
   if opt1=None then None else 
   let idx1=Option.unpack opt1 in 
   check_for_footnote_at_idx s idx1;;

(*

check_for_whitened_footnote "(56)abc ...\n" 1;;

*)

let extract_footnote_zone_from_page page=
  let temp1=Lines_in_string.core page in 
  let temp2=Image.imagination ( fun (_,line)->
    (line,check_for_whitened_footnote_at_idx line 1)
  ) temp1 in 
  let (first_one,usual_ones)=Three_parts.decompose_according_to_beginning_markers 
    (fun (line,opt)->opt<>None) temp2 in 
  let tempf1=(fun l->String.concat "\n" (Image.imagination fst l)) in    
  (tempf1 first_one, Image.imagination (fun ((first_line,_),l)->first_line^"\n"^(tempf1 l)) usual_ones);;  

(*

extract_footnote_zone_from_page
"abc\nAny(5)more\nI can(6)hear(a)\n (1)ghi\n kl\n (7) def\n (63)\n\np49\n\n kl";;

extract_footnote_zone_from_page
"abc\nAny(5)more\nI can(6)hear(7)(8)that(9)again(a)(10)(b)\n  (1)ghi\n kl\n (7) def\n\n(63)\n\n";;


*)


let extract_footnotes_from_main_text main_text =
   let n=String.length main_text in 
   let rec tempf=(
     fun (treated,idx_to_be_treated)->
       if idx_to_be_treated > n 
       then List.rev treated
       else 
       let opt1=Option.find_and_stop 
         (check_for_footnote_at_idx main_text) 
           (Ennig.ennig idx_to_be_treated n) in 
       if opt1=None 
       then List.rev ((false,Cull_string.interval main_text idx_to_be_treated n)::treated) 
       else 
       let (_,(i_start,i_end))=Option.unpack opt1 in 
       let temp1=(
         if i_start=idx_to_be_treated 
         then  treated
         else  (false,Cull_string.interval main_text idx_to_be_treated (i_start-1))::treated
       )  in 
       tempf((true,Cull_string.interval main_text i_start i_end)::temp1,i_end+1)
   ) in 
   tempf([],1);;

(*

extract_footnotes_from_main_text
"abc\nAny(5)more\nI can(6)hear(7)(8)that(9)again(a)(10)(b)\n ";;

*)

let colorize_footnotes_in_page page =
  let (main_text,footnote_zone)=extract_footnote_zone_from_page page in 
  let decomposed_main_text = extract_footnotes_from_main_text main_text in
  let colorized_zone = String.concat "\n" (Image.imagination (
     fun footnote->"[color=green]"^(Cull_string.trim_spaces_on_the_right footnote)^"[/color]"
  ) footnote_zone) in 
  let colorized_main_text = String.concat "" (Image.imagination (
   fun (is_a_reference,part)->
     if is_a_reference then "[color=green]"^part^"[/color]" else part 
  ) decomposed_main_text) in 
  colorized_main_text^"\n"^colorized_zone;;


(*

colorize_footnotes_in_page
"abc\nAny(5)more\nI can(6)hear(7)(8)that(9)again(a)(10)(b)\n  (1)ghi\n kl\n (7) def\n\n(63)\n\n";;

*)  

let colorize_footnotes_in_string s=
   let (prologue,pages)=Decompose_into_pages.dip s in 
   let colorized_prologue=colorize_footnotes_in_page prologue in 
   let colorized_pages = Image.imagination (
     fun (pg_nbr,pg_line,pg_content)->
       "\n"^pg_line^"\n"^
       (colorize_footnotes_in_page pg_content)
   ) pages in 
   String.concat "\n" (colorized_prologue::colorized_pages);;

(*

colorize_footnotes_in_string
(
"abc\nAny(5)more\nI can(6)hear(7)(8)that(9)again(a)(10)(b)\n  (1)ghi\n kl\n (7) def\n\n(63)\n\n"^
"p76 \n"^
"def\nAny(5)more\nI can(6)hear(7)(8)that(9)again(a)(10)(b)\n  (1)ghi\n kl\n (7) def\n\n(63)\n\n"^
"p87 \n"^
"ghi\nAny(5)more\nI can(6)hear(7)(8)that(9)again(a)(10)(b)\n  (1)ghi\n kl\n (7) def\n\n(63)\n\n"^
"p87 \n"
);;

*)     

let colorize_footnotes_in_file fn=
   let old_text=Io.read_whole_file fn in
   let new_text=colorize_footnotes_in_string old_text in
   Io.overwrite_with fn new_text;;
  
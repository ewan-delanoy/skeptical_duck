(*

#use"Text_editing/read_russian.ml";;

*)

module Private = struct 
let ref_for_main_list = ref [
   "\n"; (* ascii 10 *)
   " ";  (* ascii 32 *)
   "!";  (* ascii 33 *)
   "\""; (* ascii 34 *)
   "(";  (* ascii 40 *)
   ")";  (* ascii 41 *)
   ",";  (* ascii 44 *)
   "-";  (* ascii 45 *)
   ".";  (* ascii 44 *)
   "0";  (* ascii 48 *)
   "1";  (* ascii 49 *)
   "2";  (* ascii 50 *)
   "3";  (* ascii 51 *)
   "4";  (* ascii 52 *)
   "5";  (* ascii 53 *)
   "6";  (* ascii 54 *)
   "7";  (* ascii 55 *)
   "8";  (* ascii 56 *)
   "9";  (* ascii 57 *)
   ":";  (* ascii 58 *)
   ";";  (* ascii 59 *)
   "<";  (* ascii 60 *)
   "=";  (* ascii 61 *)
   ">";  (* ascii 62 *)
   "?";  (* ascii 63 *)
   "\208\144"; (* A character *)
   "\208\145"; (* B character *)
   "\208\146"; (* V character *)
   "\208\147"; (* G character *)
   "\208\148"; (* D character *)
   "\208\149"; (* E character *)
   "\208\150"; (* ZH character *)
   "\208\151"; (* Z character *)
   "\208\152"; (* I character *)
   "\208\153"; (* I-kratkaya character *)
   "\208\154"; (* K character *)
   "\208\155"; (* L character *)
   "\208\156"; (* M character *)
   "\208\157"; (* N character *)
   "\208\158"; (* O character *)
   "\208\159"; (* P character *)
   "\208\160"; (* R character *)
   "\208\161"; (* S character *)
   "\208\162"; (* T character *)
   "\208\163"; (* Y character *)
   "\208\164"; (* Y character *)
   "\208\165"; (* X character *)
   "\208\166"; (* TS character *)
   "\208\167"; (* CH character *)
   "\208\168"; (* SH character *)
   "\208\169"; (* SHCH character *)
   "\208\170"; (* Uppercase Miarki-znak character *)
   "\208\171"; (* U character *)
   "\208\172"; (* Uppercase Tviordi-znak character *)
   "\208\173"; (* AY character *)
   "\208\174"; (* OO character *)
   "\208\175"; (* YAH character *)

   "\208\176"; (* a character *)
   "\208\177"; (* b character *)
   "\208\178"; (* v character *)
   "\208\179"; (* g character *)
   "\208\180"; (* d character *)
   "\208\181"; (* e character *)
   "\208\182"; (* zh character *)
   "\208\183"; (* z character *)
   "\208\184"; (* i character *)
   "\208\185"; (* i-kratkaya character *)
   "\208\186"; (* k character *)
   "\208\187"; (* l character *)
   "\208\188"; (* m character *)
   "\208\189"; (* n character *)
   "\208\190"; (* o character *) 
   "\208\191"; (* p character *)

   "\209\128"; (* r character *)
   "\209\129"; (* s character *)
   "\209\130"; (* t character *)
   "\209\131"; (* y character *)
   "\209\132"; (* f character *)
   "\209\133"; (* x character *)
   "\209\134"; (* ts character *)
   "\209\135"; (* ch character *)
   "\209\136"; (* sh character *)
   "\209\137"; (* shch character *)
   "\209\138"; (* tviordi-znak  character *)
   "\209\139"; (* u character *)
   "\209\140"; (* miarki-znak  character *)
   "\209\141"; (* ay character *)
   "\209\142"; (* oo character *)
   "\209\143"; (* yah character *)
];;


let seek_russian_char (already_treated,text,cursor) =
    match Option.seek (fun long_char->
       Substring.is_a_substring_located_at  long_char text cursor
    ) (!ref_for_main_list) with
     None -> None 
    |Some(long_char)->Some(long_char::already_treated,text,cursor+(String.length long_char));;

let analize_end (text,cursor) =
     let n = String.length text in 
     if cursor > n then () else 
     let m=min(cursor+20)(n) in 
     let prelude1 = Cull_string.interval text cursor m in 
     let temp1 = Strung.explode prelude1 in 
     let temp2 = Image.image (fun c->
       "\\"^(string_of_int(int_of_char c)) 
    ) temp1 in 
    let prelude2 = String.concat "" temp2 in 
    let msg = "\n\n Current prelude : \n"^prelude1^",\n  or\n\n"^prelude2^"\n\n" in 
    (print_string msg;flush stdout);;

 

let rec seek_russian_chars walker =
    let (already_treated,text,cursor) = walker in 
    match seek_russian_char walker with 
    None -> let _ = analize_end (text,cursor) in 
            (List.rev already_treated)
    |Some(next_walker)->seek_russian_chars next_walker;;

let rec helper_for_linking (treated,a0,others) =
    match others with 
     [] -> List.rev treated 
    |a1::others1 -> (
         if a1<>"-"
         then helper_for_linking(a0::treated,a1,others1)
         else let treated2 = (a0^a1)::treated in   
          (match others1 with 
         [] -> List.rev treated2 
         |a2::others2 -> helper_for_linking (treated2,a2,others2)
         ) );;
let link = function [] -> [] |a::others ->   helper_for_linking ([],a,others);;  

let read txt= 
  let temp1 = seek_russian_chars ([],txt,1) in 
  let temp2 = Listennou.separate_according_to temp1 ["\n";" "] in 
  let temp3 = Image.image (String.concat "") temp2 in 
  temp3 ;; 

let prepare_dictation txt = 
  let temp1 = link (read txt) in 
  let temp2 = Image.image(fun x->x^"\\\\newline") temp1 in 
  String.concat "\n" temp2 ;;  


end ;;

let prepare_dictation = Private.prepare_dictation;;
let read = Private.read;; 

(*    
let home = Sys.getenv "HOME";;
let txt1 = rf (home^"/Downloads/temp.txt");;

let z1 = seek_russian_chars ([],txt1,1);;
let z2=prepare_dictation txt1;;


*)



(*

#use"Text_editing/improve_readiris_ocr.ml";;

*)

let replace_fixed_length_pattern_with_constant_in_string
   pattern_tester 
     pattern_length 
      replacement 
        argument_string=
    let n=String.length argument_string in 
    let cases = List.filter (pattern_tester argument_string) (Ennig.ennig 2 (n-pattern_length)) in 
    if cases=[] then argument_string else
    let temp1=(1-pattern_length)::(cases@[n+1]) in 
    let temp2=Listennou.universal_delta_list temp1 in 
    let temp3=Image.image (fun (a,b)->
        let unchanged_part=(
            let i= a+pattern_length
            and j= b-1 in 
            if i>j then "" else
            Cull_string.interval argument_string (a+pattern_length) (b-1) 
        ) in 
        if b=n+1 then unchanged_part else unchanged_part^replacement
    ) temp2 in
    String.concat "" temp3;; 

let replace_fixed_length_pattern_with_constant_in_file
   pattern_tester 
     pattern_length 
      replacement 
        argument_file=
    let old_text=Io.read_whole_file argument_file in 
    let new_text=replace_fixed_length_pattern_with_constant_in_string
    pattern_tester pattern_length replacement old_text in 
    Io.overwrite_with argument_file new_text;; 

let decompose_into_words =
   Str.full_split (Str.regexp"[\"'.,;: \n\r\t]+");;

let modify_words_in_string f s=
  let temp1=decompose_into_words s in 
  let temp2=Image.image (function
     Str.Delim(delim)->delim
     |Str.Text(text)->f text
  ) temp1 in 
  String.concat "" temp2;;

let  modify_words_in_file f argument_file=
    let old_text=Io.read_whole_file argument_file in 
    let new_text=modify_words_in_string f old_text in 
    Io.overwrite_with argument_file new_text;; 

exception Unmatched_left_parenthesis of int;;

let naive_paren_decomposer (left_paren,right_paren) s=
   let n=String.length s 
   and m_left=String.length left_paren 
   and m_right=String.length right_paren in 
   let rec tempf=(
      fun (graet,j)->
        if j>n
        then List.rev(graet)
        else
        let k=Substring.leftmost_index_of_in_from left_paren s j in 
        if k<0
        then List.rev(((j,n),Cull_string.interval s j n,false)::graet)
        else
        let k2=k+m_left in 
        let k3=Substring.leftmost_index_of_in_from right_paren s k2 in 
        if k3<0
        then raise(Unmatched_left_parenthesis(k))
        else 
        let k4=k3+m_right in 
        let temp=(if k=j then graet else ((j,k-1),Cull_string.interval s j (k-1),false)::graet) in 
        tempf(((k,k4-1),Cull_string.interval s k2 (k3-1),true)::temp,k4) 
   ) in 
   tempf([],1);;

let change_paren_denotation_in_string 
  old_parens (new_lparen,new_rparen) s=
  let temp1=naive_paren_decomposer old_parens s in 
  let temp2=Image.image(
    fun (_,content,is_parenthesized)->
      if is_parenthesized 
      then new_lparen^(Cull_string.trim_spaces content)^new_rparen
      else content
  ) temp1 in 
  String.concat "" temp2;;

let  change_paren_denotation_in_file 
    old_parens new_parens argument_file=
    let old_text=Io.read_whole_file argument_file in 
    let new_text=change_paren_denotation_in_string old_parens new_parens old_text in 
    Io.overwrite_with argument_file new_text;; 



(*

replace_fixed_length_pattern_with_constant_in_string
   (fun s k->
       if k<2 then false else
       (Strung.get s k=';')&&
       (List.mem (Strung.get s (k-1)) Charset.alphanumeric_characters)
   )
     1
      " ;" 
        "abc;def ;gh ;ijk ;lmn";;

let is_illegal s=
  let n=String.length(s) in 
  let tempf1=(fun l k->
    List.mem (Strung.get s k) l
  ) in 
  let tempf2=(fun l i j->List.exists(tempf1 l)(Ennig.ennig i j)) in 
  if tempf1 Charset.lowercase_letters 1
  then tempf2 Charset.uppercase_letters 2 n 
  else (tempf2 Charset.lowercase_letters 2 n)&&
       (tempf2 Charset.uppercase_letters 2 n);;

let make_legal s= 
  if is_illegal s then String.lowercase_ascii s else s;;

modify_words_in_string make_legal "aBc\t\nDe\n\t\tFGh klmp";;
  
naive_paren_decomposer ("[i]","[/i]") "123[i]45[/i]67[i]8[/i][i]9[/i]01234[i]56[/i]7";;


*)
      

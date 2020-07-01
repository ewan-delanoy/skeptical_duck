(*

#use"more_uchar.ml";;

*)

let unicode_point uchar= 
  Printf.sprintf "U+%04x" (Uchar.to_int uchar);;

let utf8_coordinates uchar=
    let i=Uchar.to_int uchar in
    let temp1=Utf_eight.encode i in
    Image.vorstellung (Printf.sprintf "%x") temp1;;  

let utf8_encode uchar_l=
   let temp1=Image.vorstellung Uchar.to_int uchar_l in
   let temp2=Image.vorstellung Utf_eight.encode temp1 in
   let temp3=List.flatten temp2 in
   let temp4=Image.vorstellung char_of_int temp3 in
   Strung.implode temp4;;
    
let utf8_decode s=
   let temp1=Strung.explode s in
   let temp2=Image.vorstellung int_of_char temp1 in
   let temp3=Utf_eight.decode_with_chronometer temp2 in
   Image.vorstellung Uchar.of_int temp3;;   

let locate s=
    let temp1=utf8_decode s in
    Image.vorstellung (fun u->
      (Uchar.to_int u,unicode_point u,utf8_coordinates u)
    ) temp1;;   
(*

#use"Imported/Aantron/aantron_encoding.ml";;

*)


(* adapted from Markup.ml/ at https://github.com/aantron/markup.ml *)

  let u_rep = Aantron_markup_common.u_rep ;;
  let construct = Aantron_kstream.construct ;; 
  let make = Aantron_kstream.make ;; 
  let next = Aantron_kstream.next ;; 
  let next_n = Aantron_kstream.next_n ;; 
  let peek_n = Aantron_kstream.peek_n ;; 
   
   type t = ?report:Aantron_markup_error.parse_handler -> char Aantron_kstream.t -> int Aantron_kstream.t ;;
   
   let wrap f = fun ?(report = Aantron_markup_error.ignore_errors) s -> f report s ;;
   
   let bytes_empty = Bytes.create 0 ;;
   
   (* Decoders based on the Uutf library. *)
   let uutf_decoder encoding name =
     (fun report bytes ->
       let decoder = Buenzli_uutf.decoder ~encoding `Manual in
   
       (fun throw empty k ->
         let rec run () =
           match Buenzli_uutf.decode decoder with
           | `End -> empty ()
           | `Uchar c -> k (Uchar.to_int c)
           | `Malformed s ->
             let location = Buenzli_uutf.decoder_line decoder, Buenzli_uutf.decoder_col decoder in
             report location (`Decoding_error (s, name)) throw (fun () ->
             k u_rep)
           | `Await ->
             next bytes throw
               (fun () -> Buenzli_uutf.Manual.src decoder bytes_empty 0 0; run ())
               (fun c -> Buenzli_uutf.Manual.src decoder (Bytes.make 1 c) 0 1; run ())
         in
         run ())
       |> make)
     |> wrap ;;
   
   let utf_8 : t = uutf_decoder `UTF_8 "utf-8" ;; 
   let utf_16be : t = uutf_decoder `UTF_16BE "utf-16be" ;;
   let utf_16le : t = uutf_decoder `UTF_16LE "utf-16le" ;;
   let iso_8859_1 : t = uutf_decoder `ISO_8859_1 "iso-8859-1" ;;
   let us_ascii : t = uutf_decoder `US_ASCII "us-ascii" ;;
   
   (* Chooses UTF-16LE unless the BE BOM is present, as in
      http://www.w3.org/TR/encoding/ *)
   let utf_16 : t =
     (fun report bytes ->
       let constructor =
         fun throw k ->
           peek_n 2 bytes throw (function
           | ['\xFE'; '\xFF'] -> k (utf_16be ~report bytes)
           | _ -> k (utf_16le ~report bytes))
       in
       construct constructor)
     |> wrap ;;
   
   let ucs_4_decoder arrange name =
     (fun report bytes ->
       let first = ref true in
       let line = ref 1 in
       let column = ref 1 in
   
       let char k c =
         column := !column + 1;
         k c
       in
   
       let newline k c =
         column := 1;
         line := !line + 1;
         k c
       in
   
       (fun throw empty k ->
         let rec run () =
           next_n 4 bytes throw begin function
             | [b1; b2; b3; b4] ->
               let low, b2', b3', high = arrange (b1, b2, b3, b4) in
               let low, b2', b3', high =
                 Char.code low, Char.code b2', Char.code b3', Char.code high in
   
               if high land 0x80 <> 0 then
                 let s = Printf.sprintf "%c%c%c%c" b1 b2 b3 b4 in
                 report (!line, !column) (`Decoding_error (s, name)) throw
                   (fun () ->
                 char k u_rep)
               else
                 let scalar =
                   (high lsl 24) lor (b3' lsl 16) lor (b2' lsl 8) lor low in
   
                 let skip =
                   if !first then begin
                     first := false;
                     scalar = Uchar.to_int Buenzli_uutf.u_bom
                   end
                   else
                     false
                 in
   
                 if skip then run ()
                 else
                   if scalar = 0x000A then
                     newline k scalar
                   else
                     char k scalar
   
             | [] -> empty ()
   
             | l ->
               let buffer = Buffer.create 4 in
               l |> List.iter (Buffer.add_char buffer);
               report (!line, !column)
                 (`Decoding_error (Buffer.contents buffer, name)) throw (fun () ->
               char k u_rep)
           end
         in
         run ())
       |> make)
     |> wrap ;;
   
   let ucs_4be : t =
     ucs_4_decoder (fun (b1, b2, b3, b4) -> b4, b3, b2, b1) "ucs-4be" ;;
   let ucs_4le : t =
     ucs_4_decoder (fun bs -> bs) "ucs-4le" ;;
   let ucs_4be_transposed : t =
     ucs_4_decoder (fun (b1, b2, b3, b4) -> b3, b4, b1, b2) "ucs-4be-transposed" ;;
   let ucs_4le_transposed : t =
     ucs_4_decoder (fun (b1, b2, b3, b4) -> b2, b1, b4, b3) "ucs-4le-transposed" ;;
   
   let code_page table =
     if Array.length table < 256 then
       raise (Invalid_argument
         "Markup.Encoding.code_page: array does not have 256 entries");
   
     (fun _ bytes ->
       (fun throw empty k ->
         next bytes throw empty (fun c -> k table.(Char.code c)))
       |> make)
     |> wrap ;; 
   
   let windows_1251_table = [|
       (* ASCII *)
       0x0000; 0x0001; 0x0002; 0x0003; 0x0004; 0x0005; 0x0006; 0x0007;
       0x0008; 0x0009; 0x000A; 0x000B; 0x000C; 0x000D; 0x000E; 0x000F;
       0x0010; 0x0011; 0x0012; 0x0013; 0x0014; 0x0015; 0x0016; 0x0017;
       0x0018; 0x0019; 0x001A; 0x001B; 0x001C; 0x001D; 0x001E; 0x001F;
       0x0020; 0x0021; 0x0022; 0x0023; 0x0024; 0x0025; 0x0026; 0x0027;
       0x0028; 0x0029; 0x002A; 0x002B; 0x002C; 0x002D; 0x002E; 0x002F;
       0x0030; 0x0031; 0x0032; 0x0033; 0x0034; 0x0035; 0x0036; 0x0037;
       0x0038; 0x0039; 0x003A; 0x003B; 0x003C; 0x003D; 0x003E; 0x003F;
       0x0040; 0x0041; 0x0042; 0x0043; 0x0044; 0x0045; 0x0046; 0x0047;
       0x0048; 0x0049; 0x004A; 0x004B; 0x004C; 0x004D; 0x004E; 0x004F;
       0x0050; 0x0051; 0x0052; 0x0053; 0x0054; 0x0055; 0x0056; 0x0057;
       0x0058; 0x0059; 0x005A; 0x005B; 0x005C; 0x005D; 0x005E; 0x005F;
       0x0060; 0x0061; 0x0062; 0x0063; 0x0064; 0x0065; 0x0066; 0x0067;
       0x0068; 0x0069; 0x006A; 0x006B; 0x006C; 0x006D; 0x006E; 0x006F;
       0x0070; 0x0071; 0x0072; 0x0073; 0x0074; 0x0075; 0x0076; 0x0077;
       0x0078; 0x0079; 0x007A; 0x007B; 0x007C; 0x007D; 0x007E; 0x007F;
       (* 0x8_ *)
       0x0402; 0x0403; 0x201A; 0x0453; 0x201E; 0x2026; 0x2020; 0x2021;
       0x20AC; 0x2030; 0x0409; 0x2039; 0x040A; 0x040C; 0x040B; 0x040F;
       (* 0x9_ *)
       0x0452; 0x2018; 0x2019; 0x201C; 0x201D; 0x2022; 0x2013; 0x2014;
       0xFFFD; 0x2122; 0x0459; 0x203A; 0x045A; 0x045C; 0x045B; 0x045F;
       (* 0xA_ *)
       0x00A0; 0x040E; 0x045E; 0x0408; 0x00A4; 0x0490; 0x00A6; 0x00A7;
       0x0401; 0x00A9; 0x0404; 0x00AB; 0x00AC; 0x00AD; 0x00AE; 0x0407;
       (* 0xB_ *)
       0x00B0; 0x00B1; 0x0406; 0x0456; 0x0491; 0x00B5; 0x00B6; 0x00B7;
       0x0451; 0x2116; 0x0454; 0x00BB; 0x0458; 0x0405; 0x0455; 0x0457;
       (* 0xC_ *)
       0x0410; 0x0411; 0x0412; 0x0413; 0x0414; 0x0415; 0x0416; 0x0417;
       0x0418; 0x0419; 0x041A; 0x041B; 0x041C; 0x041D; 0x041E; 0x041F;
       (* 0xD_ *)
       0x0410; 0x0421; 0x0422; 0x0423; 0x0424; 0x0425; 0x0426; 0x0427;
       0x0428; 0x0429; 0x042A; 0x042B; 0x042C; 0x042D; 0x042E; 0x042F;
       (* 0xE_ *)
       0x0430; 0x0431; 0x0432; 0x0433; 0x0434; 0x0435; 0x0436; 0x0437;
       0x0438; 0x0439; 0x043A; 0x043B; 0x043C; 0x043D; 0x043E; 0x043F;
       (* 0xF_ *)
       0x0440; 0x0441; 0x0442; 0x0443; 0x0444; 0x0445; 0x0446; 0x0447;
       0x0448; 0x0449; 0x044A; 0x044B; 0x044C; 0x044D; 0x044E; 0x044F
     |] ;;
   
   let windows_1251 : t = code_page windows_1251_table ;;
   
   let windows_1252_table = [|
       (* ASCII *)
       0x0000; 0x0001; 0x0002; 0x0003; 0x0004; 0x0005; 0x0006; 0x0007;
       0x0008; 0x0009; 0x000A; 0x000B; 0x000C; 0x000D; 0x000E; 0x000F;
       0x0010; 0x0011; 0x0012; 0x0013; 0x0014; 0x0015; 0x0016; 0x0017;
       0x0018; 0x0019; 0x001A; 0x001B; 0x001C; 0x001D; 0x001E; 0x001F;
       0x0020; 0x0021; 0x0022; 0x0023; 0x0024; 0x0025; 0x0026; 0x0027;
       0x0028; 0x0029; 0x002A; 0x002B; 0x002C; 0x002D; 0x002E; 0x002F;
       0x0030; 0x0031; 0x0032; 0x0033; 0x0034; 0x0035; 0x0036; 0x0037;
       0x0038; 0x0039; 0x003A; 0x003B; 0x003C; 0x003D; 0x003E; 0x003F;
       0x0040; 0x0041; 0x0042; 0x0043; 0x0044; 0x0045; 0x0046; 0x0047;
       0x0048; 0x0049; 0x004A; 0x004B; 0x004C; 0x004D; 0x004E; 0x004F;
       0x0050; 0x0051; 0x0052; 0x0053; 0x0054; 0x0055; 0x0056; 0x0057;
       0x0058; 0x0059; 0x005A; 0x005B; 0x005C; 0x005D; 0x005E; 0x005F;
       0x0060; 0x0061; 0x0062; 0x0063; 0x0064; 0x0065; 0x0066; 0x0067;
       0x0068; 0x0069; 0x006A; 0x006B; 0x006C; 0x006D; 0x006E; 0x006F;
       0x0070; 0x0071; 0x0072; 0x0073; 0x0074; 0x0075; 0x0076; 0x0077;
       0x0078; 0x0079; 0x007A; 0x007B; 0x007C; 0x007D; 0x007E; 0x007F;
       (* 0x8_ *)
       0x20AC; 0x0081; 0x201A; 0x0192; 0x201E; 0x2026; 0x2020; 0x2021;
       0x02C6; 0x2030; 0x0160; 0x2039; 0x0152; 0x008D; 0x017D; 0x008F;
       (* 0x9_ *)
       0x0090; 0x2018; 0x2019; 0x201C; 0x201D; 0x2022; 0x2013; 0x2014;
       0x02DC; 0x2122; 0x0161; 0x203A; 0x0153; 0x009D; 0x017E; 0x0178;
       (* ISO-8859-1 *)
       0x00A0; 0x00A1; 0x00A2; 0x00A3; 0x00A4; 0x00A5; 0x00A6; 0x00A7;
       0x00A8; 0x00A9; 0x00AA; 0x00AB; 0x00AC; 0x00AD; 0x00AE; 0x00AF;
       0x00B0; 0x00B1; 0x00B2; 0x00B3; 0x00B4; 0x00B5; 0x00B6; 0x00B7;
       0x00B8; 0x00B9; 0x00BA; 0x00BB; 0x00BC; 0x00BD; 0x00BE; 0x00BF;
       0x00C0; 0x00C1; 0x00C2; 0x00C3; 0x00C4; 0x00C5; 0x00C6; 0x00C7;
       0x00C8; 0x00C9; 0x00CA; 0x00CB; 0x00CC; 0x00CD; 0x00CE; 0x00CF;
       0x00D0; 0x00D1; 0x00D2; 0x00D3; 0x00D4; 0x00D5; 0x00D6; 0x00D7;
       0x00D8; 0x00D9; 0x00DA; 0x00DB; 0x00DC; 0x00DD; 0x00DE; 0x00DF;
       0x00E0; 0x00E1; 0x00E2; 0x00E3; 0x00E4; 0x00E5; 0x00E6; 0x00E7;
       0x00E8; 0x00E9; 0x00EA; 0x00EB; 0x00EC; 0x00ED; 0x00EE; 0x00EF;
       0x00F0; 0x00F1; 0x00F2; 0x00F3; 0x00F4; 0x00F5; 0x00F6; 0x00F7;
       0x00F8; 0x00F9; 0x00FA; 0x00FB; 0x00FC; 0x00FD; 0x00FE; 0x00FF
     |]
   
   let windows_1252 : t = code_page windows_1252_table
   
   let ebcdic_37_table = [|
       (* 0x0_ *)
       0x0000; 0x0001; 0x0002; 0x0003; 0x009C; 0x0009; 0x0086; 0x007F;
       0x0097; 0x008D; 0x008E; 0x000B; 0x000C; 0x000D; 0x000E; 0x000F;
       (* 0x1_ *)
       0x0010; 0x0011; 0x0012; 0x0013; 0x009D; 0x0085; 0x0008; 0x0087;
       0x0018; 0x0019; 0x0092; 0x008F; 0x001C; 0x001D; 0x001E; 0x001F;
       (* 0x2_ *)
       0x0080; 0x0081; 0x0082; 0x0083; 0x0084; 0x000A; 0x0017; 0x001B;
       0x0088; 0x0089; 0x008A; 0x008B; 0x008C; 0x0005; 0x0006; 0x0007;
       (* 0x3_ *)
       0x0090; 0x0091; 0x0016; 0x0093; 0x0094; 0x0095; 0x0096; 0x0004;
       0x0098; 0x0099; 0x009A; 0x009B; 0x0014; 0x0015; 0x009E; 0x001A;
       (* 0x4_ *)
       0x0020; 0x00A0; 0x00E2; 0x00E4; 0x00E0; 0x00E1; 0x00E3; 0x00E5;
       0x00E7; 0x00F1; 0x00A2; 0x002E; 0x003C; 0x0028; 0x002B; 0x007C;
       (* 0x5_ *)
       0x0026; 0x00E9; 0x00EA; 0x00EB; 0x00E8; 0x00ED; 0x00EE; 0x00EF;
       0x00EC; 0x00DF; 0x0021; 0x0024; 0x002A; 0x0029; 0x003B; 0x00AC;
       (* 0x6_ *)
       0x002D; 0x002F; 0x00C2; 0x00C4; 0x00C0; 0x00C1; 0x00C3; 0x00C5;
       0x00C7; 0x00D1; 0x00A6; 0x002C; 0x0025; 0x005F; 0x003E; 0x003F;
       (* 0x7_ *)
       0x00F8; 0x00C9; 0x00CA; 0x00CB; 0x00C8; 0x00CD; 0x00CE; 0x00CF;
       0x00CC; 0x0060; 0x003A; 0x0023; 0x0040; 0x0027; 0x003D; 0x0022;
       (* 0x8_ *)
       0x00D8; 0x0061; 0x0062; 0x0063; 0x0064; 0x0065; 0x0066; 0x0067;
       0x0068; 0x0069; 0x00AB; 0x00BB; 0x00F0; 0x00FD; 0x00FE; 0x00B1;
       (* 0x9_ *)
       0x00B0; 0x006A; 0x006B; 0x006C; 0x006D; 0x006E; 0x006F; 0x0070;
       0x0071; 0x0072; 0x00AA; 0x00BA; 0x00E6; 0x00B8; 0x00C6; 0x00A4;
       (* 0xA_ *)
       0x00B5; 0x007E; 0x0073; 0x0074; 0x0075; 0x0076; 0x0077; 0x0078;
       0x0079; 0x007A; 0x00A1; 0x00BF; 0x00D0; 0x00DD; 0x00DE; 0x00AE;
       (* 0xB_ *)
       0x005E; 0x00A3; 0x00A5; 0x00B7; 0x00A9; 0x00A7; 0x00B6; 0x00BC;
       0x00BD; 0x00BE; 0x005B; 0x005D; 0x00AF; 0x00A8; 0x00B4; 0x00D7;
       (* 0xC_ *)
       0x007B; 0x0041; 0x0042; 0x0043; 0x0044; 0x0045; 0x0046; 0x0047;
       0x0048; 0x0049; 0x00AD; 0x00F4; 0x00F6; 0x00F2; 0x00F3; 0x00F5;
       (* 0xD_ *)
       0x007D; 0x004A; 0x004B; 0x004C; 0x004D; 0x004E; 0x004F; 0x0050;
       0x0051; 0x0052; 0x00B9; 0x00FB; 0x00FC; 0x00F9; 0x00FA; 0x00FF;
       (* 0xE_ *)
       0x005C; 0x00F7; 0x0053; 0x0054; 0x0055; 0x0056; 0x0057; 0x0058;
       0x0059; 0x005A; 0x00B2; 0x00D4; 0x00D6; 0x00D2; 0x00D3; 0x00D5;
       (* 0xF_ *)
       0x0030; 0x0031; 0x0032; 0x0033; 0x0034; 0x0035; 0x0036; 0x0037;
       0x0038; 0x0039; 0x00B3; 0x00DB; 0x00DC; 0x00D9; 0x00DA; 0x009F
     |]
   
   let ebcdic : t = code_page ebcdic_37_table
   
   let iso_8859_15_table = [|
       (* ASCII *)
       0x0000; 0x0001; 0x0002; 0x0003; 0x0004; 0x0005; 0x0006; 0x0007;
       0x0008; 0x0009; 0x000A; 0x000B; 0x000C; 0x000D; 0x000E; 0x000F;
       0x0010; 0x0011; 0x0012; 0x0013; 0x0014; 0x0015; 0x0016; 0x0017;
       0x0018; 0x0019; 0x001A; 0x001B; 0x001C; 0x001D; 0x001E; 0x001F;
       0x0020; 0x0021; 0x0022; 0x0023; 0x0024; 0x0025; 0x0026; 0x0027;
       0x0028; 0x0029; 0x002A; 0x002B; 0x002C; 0x002D; 0x002E; 0x002F;
       0x0030; 0x0031; 0x0032; 0x0033; 0x0034; 0x0035; 0x0036; 0x0037;
       0x0038; 0x0039; 0x003A; 0x003B; 0x003C; 0x003D; 0x003E; 0x003F;
       0x0040; 0x0041; 0x0042; 0x0043; 0x0044; 0x0045; 0x0046; 0x0047;
       0x0048; 0x0049; 0x004A; 0x004B; 0x004C; 0x004D; 0x004E; 0x004F;
       0x0050; 0x0051; 0x0052; 0x0053; 0x0054; 0x0055; 0x0056; 0x0057;
       0x0058; 0x0059; 0x005A; 0x005B; 0x005C; 0x005D; 0x005E; 0x005F;
       0x0060; 0x0061; 0x0062; 0x0063; 0x0064; 0x0065; 0x0066; 0x0067;
       0x0068; 0x0069; 0x006A; 0x006B; 0x006C; 0x006D; 0x006E; 0x006F;
       0x0070; 0x0071; 0x0072; 0x0073; 0x0074; 0x0075; 0x0076; 0x0077;
       0x0078; 0x0079; 0x007A; 0x007B; 0x007C; 0x007D; 0x007E; 0x007F;
       (* ISO-8859-1 *)
       0x0080; 0x0081; 0x0082; 0x0083; 0x0084; 0x0085; 0x0086; 0x0087;
       0x0088; 0x0089; 0x008A; 0x008B; 0x008C; 0x008D; 0x008E; 0x008F;
       0x0090; 0x0091; 0x0092; 0x0093; 0x0094; 0x0095; 0x0096; 0x0097;
       0x0098; 0x0099; 0x009A; 0x009B; 0x009C; 0x009D; 0x009E; 0x009F;
       (* 0xA_ *)
       0x00A0; 0x00A1; 0x00A2; 0x00A3; 0x20AC; 0x00A5; 0x0160; 0x00A7;
       0x0161; 0x00A9; 0x00AA; 0x00AB; 0x00AC; 0x00AD; 0x00AE; 0x00AF;
       (* 0xB_ *)
       0x00B0; 0x00B1; 0x00B2; 0x00B3; 0x017D; 0x00B5; 0x00B6; 0x00B7;
       0x017E; 0x00B9; 0x00BA; 0x00BB; 0x0152; 0x0153; 0x0178; 0x00BF;
       (* ISO-8859-1 *)
       0x00C0; 0x00C1; 0x00C2; 0x00C3; 0x00C4; 0x00C5; 0x00C6; 0x00C7;
       0x00C8; 0x00C9; 0x00CA; 0x00CB; 0x00CC; 0x00CD; 0x00CE; 0x00CF;
       0x00D0; 0x00D1; 0x00D2; 0x00D3; 0x00D4; 0x00D5; 0x00D6; 0x00D7;
       0x00D8; 0x00D9; 0x00DA; 0x00DB; 0x00DC; 0x00DD; 0x00DE; 0x00DF;
       0x00E0; 0x00E1; 0x00E2; 0x00E3; 0x00E4; 0x00E5; 0x00E6; 0x00E7;
       0x00E8; 0x00E9; 0x00EA; 0x00EB; 0x00EC; 0x00ED; 0x00EE; 0x00EF;
       0x00F0; 0x00F1; 0x00F2; 0x00F3; 0x00F4; 0x00F5; 0x00F6; 0x00F7;
       0x00F8; 0x00F9; 0x00FA; 0x00FB; 0x00FC; 0x00FD; 0x00FE; 0x00FF
     |] ;;
   
   let iso_8859_15: t = code_page iso_8859_15_table ;;

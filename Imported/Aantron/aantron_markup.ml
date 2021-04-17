(*

#use"Imported/Aantron/aantron_markup.ml";;

*)


(* adapted  from Markup.ml at https://github.com/aantron/markup.ml. *)

  
(*
   module type IO_TYPE =
   sig
     type 'a t
   
     val return : 'a -> 'a t
     val of_cps : ((exn -> unit) -> ('a -> unit) -> unit) -> 'a t
     val to_cps : (unit -> 'a t) -> ((exn -> unit) -> ('a -> unit) -> unit)
   end
*)
   
   
   (* : IO_TYPE with type 'a t = 'a *)

   module Synchronous  =
   struct
     type 'a t = 'a ;;
   
     exception Not_synchronous ;;
   
     let return x = x ;;
   
     let of_cps f =
       let result = ref None in
       f raise (fun v -> result := Some v);
       match !result with
       | None -> raise Not_synchronous
       | Some v -> v ;;
   
     (* Used in to_cps to avoid the need for a match .. with | exception ..
        expression, which would break compatibility with OCaml < 4.02. Flambda
        seems to optimizes the allocation of these results away completely. There
        is a small performance penalty when not using Flambda. *)
     type 'a result = Value of 'a | Exn of exn ;;
   
     let to_cps f =
       fun throw k ->
         let result =
           try Value (f ())
           with exn -> Exn exn
         in
         match result with
         | Value v -> k v
         | Exn exn -> throw exn ;;
   end ;;
   
   
   
   type async = unit ;;
   type sync = unit ;;
   
   type ('data, 'sync) stream = 'data Aantron_kstream.t ;; 
   
   let kstream s = s ;;
   let of_kstream s = s ;;
   
   let of_list = Aantron_kstream.of_list ;;
   
   
   
   type location = Aantron_markup_common.location ;;
   let compare_locations = Aantron_markup_common.compare_locations ;;
   
  
   type name = Aantron_markup_common.name ;;
   
   type xml_declaration = Aantron_markup_common.xml_declaration ;;
   
   type doctype = Aantron_markup_common.doctype ;;
   
   type signal = Aantron_markup_common.signal ;;
   
   let signal_to_string = Aantron_markup_common.signal_to_string ;;
   
   type 's parser =
     {mutable location : location;
      mutable signals  : (signal, 's) stream} ;;
   
   let signals parser = parser.signals ;;
   let location parser = parser.location ;;
   
   let stream_to_parser s =
     let parser = {location = (1, 1); signals = Aantron_kstream.empty ()} in
     parser.signals <-
       s |> Aantron_kstream.map (fun (l, v) _ k -> parser.location <- l; k v);
     parser ;;
   
   module Cps =
   struct
     let parse_xml
         report ?encoding namespace entity context source =
       let with_encoding (encoding : Aantron_encoding.t) k =
         source
         |> encoding ~report
         |> Aantron_input.preprocess Aantron_markup_common.is_valid_xml_char report
         |> Aantron_xml_tokenizer.tokenize report entity
         |> Aantron_xml_parser.parse context namespace report
         |> k
       in
   
       let constructor throw k =
         match encoding with
         | Some encoding -> with_encoding encoding k
         | None ->
           Aantron_detect.select_xml source throw (fun encoding ->
           with_encoding encoding k)
       in
   
       Aantron_kstream.construct constructor
       |> stream_to_parser ;;
   
     let write_xml report prefix signals =
       signals
       |> Aantron_xml_writer.write report prefix
       |> Aantron_utility.strings_to_bytes ;;
   
     let parse_html report ?encoding context source =
       let with_encoding (encoding : Aantron_encoding.t) k =
         source
         |> encoding ~report
         |> Aantron_input.preprocess Aantron_markup_common.is_valid_html_char report
         |> Aantron_html_tokenizer.tokenize report
         |> Aantron_html_parser.parse context report
         |> k
       in
   
       let constructor throw k =
         match encoding with
         | Some encoding -> with_encoding encoding k
         | None ->
           Aantron_detect.select_html source throw (fun encoding ->
           with_encoding encoding k)
       in
   
       Aantron_kstream.construct constructor
       |> stream_to_parser ;;
   
     let write_html ?escape_attribute ?escape_text signals =
       signals
       |> Aantron_html_writer.write ?escape_attribute ?escape_text
       |> Aantron_utility.strings_to_bytes ;;
   end ;;
   
   
   
   let string = Aantron_stream_io.string ;;
   let buffer = Aantron_stream_io.buffer ;;
   let channel = Aantron_stream_io.channel ;;
   let file = Aantron_stream_io.file ;;
   
   let to_channel c bytes = Aantron_stream_io.to_channel c bytes |> Synchronous.of_cps ;;
   let to_file f bytes = Aantron_stream_io.to_file f bytes |> Synchronous.of_cps ;;
   
   
   
   let preprocess_input_stream source =
     Aantron_input.preprocess (fun _ -> true) Aantron_markup_error.ignore_errors source ;;
   
   
  
   
   
   module Ns =
   struct
     let html = Aantron_markup_common.html_ns
     let svg = Aantron_markup_common.svg_ns
     let mathml = Aantron_markup_common.mathml_ns
     let xml = Aantron_markup_common.xml_ns
     let xmlns = Aantron_markup_common.xmlns_ns
     let xlink = Aantron_markup_common.xlink_ns
   end ;;
   
   module Error =
   struct
   type t = Aantron_markup_error.t ;;
   
   let to_string = Aantron_markup_error.to_string ;;
   end ;;
    

module IO =struct
     type 'a t = 'a Synchronous.t ;;
   
     let return = Synchronous.return ;;
     let of_cps = Synchronous.of_cps ;;
     let to_cps = Synchronous.to_cps ;;
end ;;

let wrap_report report = fun l e -> IO.to_cps (fun () -> report l e) ;;
   

module Encoding =
struct
  (* include Aantron_encoding *)
  type t =  Aantron_encoding.t ;; 
  let ebcdic = Aantron_encoding.ebcdic ;;
  let iso_8859_1 = Aantron_encoding.iso_8859_1 ;;
  let ucs_4be = Aantron_encoding.ucs_4be ;;
  let ucs_4be_transposed = Aantron_encoding.ucs_4be_transposed ;;
  let ucs_4le = Aantron_encoding.ucs_4le ;;
  let ucs_4le_transposed = Aantron_encoding.ucs_4le_transposed ;;
  let us_ascii = Aantron_encoding.us_ascii ;;
  let utf_16 = Aantron_encoding.utf_16 ;;
  let utf_16be = Aantron_encoding.utf_16be ;;
  let utf_16le = Aantron_encoding.utf_16le ;;
  let utf_8 = Aantron_encoding.utf_8 ;;
  let windows_1251 = Aantron_encoding.windows_1251 ;;
  let windows_1252 = Aantron_encoding.windows_1252 ;;
   
  let decode ?(report = fun _ _ -> IO.return ()) (f : Aantron_encoding.t) s =
    f ~report:(wrap_report report) s ;;
end ;;


let parse_xml
    ?(report = fun _ _ -> IO.return ())
    ?encoding
    ?(namespace = fun _ -> None)
    ?(entity = fun _ -> None)
    ?context
    source =
   
  Cps.parse_xml
    (wrap_report report) ?encoding namespace entity context source ;;
   
let write_xml
    ?(report = fun _ _ -> IO.return ())
    ?(prefix = fun _ -> None)
    signals =
   
  Cps.write_xml (wrap_report report) prefix signals ;;
   
let parse_html
    ?(report = fun _ _ -> IO.return ())
    ?encoding
    ?context
    source =
   
  Cps.parse_html (wrap_report report) ?encoding context source ;;
   
let write_html ?escape_attribute ?escape_text signals =
  Cps.write_html ?escape_attribute ?escape_text signals ;;
   
let to_string bytes = Aantron_stream_io.to_string bytes |> IO.of_cps ;;
let to_buffer bytes = Aantron_stream_io.to_buffer bytes |> IO.of_cps ;;
   
let stream f =
  let f = IO.to_cps f in
  (fun throw e k ->
    f throw (function
      | None -> e ()
      | Some v -> k v))
  |> Aantron_kstream.make ;;
   
let fn = stream ;;
   
let next s = Aantron_kstream.next_option s |> IO.of_cps ;;
let peek s = Aantron_kstream.peek_option s |> IO.of_cps ;;
   
(* Without Flambda, thunks are repeatedly created and passed on IO.to_cps,
   resulting in a performance penalty. Flambda seems to optimize this away,
   however. *)
   
let transform f v s =
  Aantron_kstream.transform (fun v s -> IO.to_cps (fun () -> f v s)) v s ;;
   
let fold f v s =
  Aantron_kstream.fold (fun v v' -> IO.to_cps (fun () -> f v v')) v s |> IO.of_cps ;;
   
let map f s = Aantron_kstream.map (fun v -> IO.to_cps (fun () -> f v)) s ;;
   
let filter f s = Aantron_kstream.filter (fun v -> IO.to_cps (fun () -> f v)) s ;;
   
let filter_map f s = Aantron_kstream.filter_map (fun v -> IO.to_cps (fun () -> f v)) s ;;
   
let iter f s =
  Aantron_kstream.iter (fun v -> IO.to_cps (fun () -> f v)) s |> IO.of_cps ;;
   
let drain s = iter (fun _ -> IO.return ()) s ;;
   
let to_list s = Aantron_kstream.to_list s |> IO.of_cps ;;
   
let load s =
  (fun throw k -> Aantron_kstream.to_list s throw (fun l -> k (Aantron_kstream.of_list l)))
  |> IO.of_cps ;;
   
let tree ?text ?element ?comment ?pi ?xml ?doctype s =
  Aantron_utility.tree ?text ?element ?comment ?pi ?xml ?doctype s |> IO.of_cps ;;
 
  let content = Aantron_utility.content ;;
  let elements = Aantron_utility.elements ;;
  let from_tree = Aantron_utility.from_tree ;;
  let html5 = Aantron_utility.html5 ;;
  let normalize_text = Aantron_utility.normalize_text ;;
  let pretty_print = Aantron_utility.pretty_print ;;
  let strings_to_bytes = Aantron_utility.strings_to_bytes ;;
  let text = Aantron_utility.text ;;
  let trees = Aantron_utility.trees ;;
  let trim = Aantron_utility.trim ;;
  let xhtml = Aantron_utility.xhtml ;;
  let xhtml_entity = Aantron_utility.xhtml_entity ;;
  
type backtrack_length = int
type regexp 
val unveil : regexp -> string
val veil : string -> regexp
val set_backtrack : backtrack_length -> regexp -> regexp
val regexp_string : string -> regexp
val plus : regexp -> regexp
val star : regexp -> regexp
val concat : regexp -> regexp -> regexp
val big_concat : regexp list -> regexp
val big_or : regexp list -> regexp
val ore : regexp -> regexp -> regexp
val string_match : regexp -> string -> int -> int option

type left_regexp = regexp
type center_regexp = regexp
type right_regexp = regexp
type centered_regexp = left_regexp * center_regexp * right_regexp

val create_centered_regexp :
  left_regexp -> center_regexp -> right_regexp -> centered_regexp
val centered_regexp_match :
  centered_regexp -> string -> int -> (int * int) option  
val centered_regexp_decorated_list_match :
  ('a * centered_regexp) list ->
  string ->
  backtrack_length -> ('a * (backtrack_length * backtrack_length)) option  
val find_all_decorated_occurrences :
  ('a * centered_regexp) list ->
  string ->
  backtrack_length -> ('a * (backtrack_length * backtrack_length)) list  
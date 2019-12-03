(*

#use"Filewatching/fw_final_cleaner_t.ml";;

Acts as a final cleaner during refresh in a Fw_config_t object.

*)

type t ={
  redundant_dependencies : (Dfa_ending_t.t * Dfa_ending_t.t) list;
};;
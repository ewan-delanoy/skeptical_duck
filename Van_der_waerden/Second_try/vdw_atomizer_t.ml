(*

#use"Van_der_Waerden/Second_try/vdw_atomizer_t.ml";;

*)

type t = {
   atoms     : (Vdw_atom_t.t * (int list list)) list;
   molecules : (Vdw_molecule_t.t * ((Vdw_atom_t.t * Vdw_translation_t.t) list)) list;
   history   : ( (int * Vdw_translation_t.t) * ((int * Vdw_translation_t.t) * (int * Vdw_translation_t.t)) ) list ;
   gains     : ( (int * Vdw_translation_t.t) * ((int * Vdw_translation_t.t) * (int * Vdw_translation_t.t)) ) list ;
} ;;

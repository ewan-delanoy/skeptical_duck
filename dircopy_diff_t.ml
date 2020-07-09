(*

#use"dircopy_diff_t.ml";;

*)

type t={
   recently_deleted : Dfn_rootless_t.t list;
   recently_changed : Dfn_rootless_t.t list;
   recently_created : Dfn_rootless_t.t list;
};;


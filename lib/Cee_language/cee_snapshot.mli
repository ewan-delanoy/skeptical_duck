
  type t 

  val commands : t -> Cee_compilation_command_t.t list
  val destination : t -> Directory_name_t.t
  val parameters : t -> Cee_snapshot_parameters_t.t 
  val source : t -> Directory_name_t.t
  val take_possession : Cee_snapshot_parameters_t.t -> t  

  val all_h_or_c_files : t -> string list
  val directly_compiled_files : t -> string list
  val separate_commands : t -> Cee_compilation_command_t.separate_t list
  
  val create_file : t -> string -> ?new_content_description:string -> is_temporary:bool -> string -> unit
  val read_file : t -> string -> string
  val modify_file : t -> string -> string -> unit
  val reinitialize_destination_directory : t -> unit  

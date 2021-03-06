(** Plugin system *)

(** Plugin in mlorg can be either dynamically loaded
    or statically loaded. mlorg comes itself with a few plugins to export and operate on AST.

    The problem to be solved is:
    - Consistent interface for different kind of plugins (exporters, transformers, ...)
    - Make the configuration access to the configuration
*)

(** {2 Plugin definition} *)
module type Plugin = sig
  val name : string
  (** The name of the module *)

  val config : Config.t
  (** The configuration of the module. See {!Config} on how to create a configuration *)

  (** The interface type of the plugin *)
  type interface

  val data : interface
  (** The data of the plugin *)
end
(** A plugin is module *)

(** A plugin of unspecified type *)
type _plugin = (module Plugin)

(** A plugin whose interface type is ['a] *)
type 'a plugin = (module Plugin with type interface = 'a)

(** {2 Specific plugins type} *)

type 'a output = 'a BatIO.output
(* type output = | Unit of unit BatIO.output
 *               | Buffer of string BatIO.output *)


(** {3 Exporters} *)
module type Exporter = sig

  val default_filename : string -> string
  (** [default_filename file] should return the output file corresponding to input file [file].
      See {!Prelude.change_ext} to deal with most common case.  *)

  val export : Config.instance -> Document.t -> 'a output -> unit
  (** The export function. *)
end

type exporter = (module Exporter)

module Exporters : sig
  val add : exporter plugin -> unit

  val find : string -> exporter

  val run :
    exporter -> Config.instance -> Document.t -> 'a output -> unit
end

(** {3 General purpose plugin} *)
module General : sig
  val add : unit plugin -> unit
end
(** General purpose plugins (such as Transformers) *)

(** {3 Global item configuration} *)
module Global : sig
  val verbose : int Config.item
  (** Verbosity level *)
end
(** The global configuration. In this module are stored
    the global parameter of mlorg. *)

(** {2 Configuration management} *)

val global_config : (string * string) list -> Config.instance
(** Creates a configuration instance for all registered plugins from a list of
    key-value terms. *)

val eprint_config_descr : unit -> unit
(** Prints on stderr the description of all the configuration items *)

val options_man : unit -> Cmdliner.Manpage.block list
(** Returns the options documentation in man format *)

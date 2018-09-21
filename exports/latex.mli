(** LaTeX exporter *)

val export : Config.instance -> Document.t -> unit Batteries.IO.output -> unit
(** [export config doc out] exports the document [document] to [out] with config [config] *)

(** The exporter class for LaTeX *)
class latexExporter :
  Config.instance
  -> unit Batteries.IO.output
  -> object
       inherit [unit, Toc.t] Document.bottomUpWithArg

       method bot : unit

       method combine : unit list -> unit

       method header : Document.t -> unit

       method footer : Document.t -> unit

       (** This method doesn't write the header/footer *)
       method _document : Toc.t -> Document.t -> unit
     end

(** Configuration items declared by this exporter *)
module Config : sig
  val classname : string Config.item

  val header : string Config.item

  val footer : string Config.item

  val extraheader : string Config.item

  val sections : string list Config.item
end

val escape_inside : string -> string
(** Escape a string inside brackets *)

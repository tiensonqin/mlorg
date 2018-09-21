(** Beamer exporter *)

val export : Config.instance -> Document.t -> unit Batteries.IO.output -> unit
(** [export config doc out] exports the document [document] in beamer to [out] with config [config] *)

(** The exporter class for Beamer *)
class beamerExporter :
  Config.instance
  -> unit Batteries.IO.output
  -> object
       (** This method doesn't write the header/footer *)
       inherit Latex.latexExporter
     end

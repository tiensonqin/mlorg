(** Org exporter *)

val export : 'a -> Document.t -> unit Batteries.IO.output -> unit
(** [export config doc out] exports the document [document] to [out] with config [config] *)

val inline_to_string : Inline.t list -> string
(** Export inline contents to string *)

(** The exporter class for org *)
class orgExporter :
  unit Batteries.IO.output
  -> object
       inherit [unit] Document.bottomUp

       method bot : unit

       method combine : unit list -> unit

       method timestamp : Timestamp.t -> unit

       (** This method doesn't write the header/footer *)
       method range : Timestamp.range -> unit
     end

(** Configuration items declared by this exporter *)

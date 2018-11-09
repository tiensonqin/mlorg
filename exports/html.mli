(** Html generation *)
open Config  (** This module deals with html generation for mlorg.
    Generation is done in a pretty straightforward way, returning a {!Xml.t}
 *)

(** The configuration item of this module. See the documentation for the meaning
    of them. *)
module Config : sig
  val encoding : string Config.item

  val full : bool Config.item

  val style : string Config.item

  val number_lines : bool Config.item

  val image_extensions : string list Config.item
end

(** The object used to export to Html. *)
class htmlExporter :
  instance
  -> object
       inherit [Xml.t list] Document.bottomUp

       method bot : Xml.t list

       method range : Timestamp.range -> bool -> Xml.t

       method timestamp : Timestamp.t -> string -> Xml.t

       method handle_image_link :
         Inline.url -> string -> Inline.t list -> Xml.t list

       method combine : Xml.t list list -> Xml.t list
     end

val export : instance -> Document.t -> 'a Batteries.IO.output -> unit
(** [export config document out] exports the document [document] to HTML
    and writes the output to [out] *)

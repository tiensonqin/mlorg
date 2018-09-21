(** Entity definition *)

(** This short files defines the entities of org.
    Note that latin1 part of entities have been suppressed. *)
type t =
  { name: string
  ; latex: string
  ; latex_mathp: bool
  ; html: string
  ; ascii: string
  ; unicode: string }

type entity = t

(** The list of entities *)
module Entities : ExtList.ExtList

val find : string -> entity
(** [find name] finds the entity named [name] or raises [Not_found] *)

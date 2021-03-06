(** Defining XML trees and outputs for XML trees. *)
open Batteries

(** The type of xml data, constructed with {!block}, {!raw}, {!list}, {!empty}
    and {!data} *)
type t

val data : string -> t
(** Construct a leaf of the tree *)

val raw : string -> t
(** Used to insert raw xml as a string in the tree *)

val block : ?attr:(string * string) list -> string -> t list -> t
(** Construct a block with given attribute name and children. The indent
    parameter specify if it is allowed to indent the contents of this block (default true) *)

val list : t list -> t
(** Compacts a list of elements into a single one. *)

val empty : t
(** The empty element *)

val output :
     ?offset:int
  -> unit IO.output
  -> string list
  -> string list
  -> string list
  -> string list
  -> t list
  -> unit
(** Output a list of xml element to a channel.
    Takes arguments to prettyprint :
    - the list of markup that defines inline content : will not be
    followed by EOL.
    - the list of markup that may contain inline markup
    - the list of exceptions to markups that can't be
    written like <foo/> when there's no child.
    - the list of markup for which spaces are significant *)

val output_xhtml : ?offset:int -> 'a IO.output -> t list -> unit
(** Outputs the tree as xhtml. *)

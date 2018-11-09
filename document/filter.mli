open Re

(** Filters *)

(** This file defines basic filter on headings *)

(** The type of a filter *)
type t = Document.heading -> bool

(** {3 String matcher} *)

(** The type of a string matcher: a function matching strings *)
type string_matcher = string -> bool

val s : string -> string_matcher
(** [s "foo"] matches only ["foo"] *)

val r : Str.regexp -> string_matcher
(** [r regexp] matches only what [regexp matches] *)

val rs : string -> string_matcher
(** [rs "regexp"] matches only what [Str.regexp "regexp"] matches *)

val rsi : string -> string_matcher
(** [rsi "regexp"] matches only what [Str.regexp_case_fold "regexp"] matches *)

(** {3 Basic filters} *)

val has_property : string_matcher -> t
(** Selects only headings with the given property *)

val prop_val : string -> string_matcher -> t
(** [prop_val name v] selects headings which have the property [val] matching [v] *)

val has_tag : string_matcher -> t
(** Selects only headings with the given tag *)

val name : string_matcher -> t
(** Selects headings named string *)

val marker : string_matcher -> t
(** Selects headings with matching markers *)

val scheduled : Timestamp.t -> t
(** Selects heading scheduled on the given day *)

val deadline : Timestamp.t -> t
(** Selects heading due on the given day *)

val happens : Timestamp.t -> t
(** Selects happening on the given day (ie. with a timestamp equal to the given day) *)

val leaf : t
(** Selects leaf headings (ie. headings with no children) *)

(** {3 Combinators} *)

val under : t -> t
(** [under f] selects headings that are children of a node matched by [f] *)

val ( &&& ) : t -> t -> t
(** [f &&& g] selects heading matching [f] and [g] *)

val ( ||| ) : t -> t -> t
(** [f ||| g] selects heading matching [f] or [g] *)

(** {3 Running filters} *)

val run : t -> Document.t -> Document.heading list
(** Run the filter on a document *)

val run_headings : t -> Document.heading list -> Document.heading list
(** Run on a list of headings *)

val run_headings_sub : t -> Document.heading list -> Document.heading list
(** Run on a list of headings and their children *)

val modify :
  t -> Document.t -> (Document.heading -> Document.heading) -> Document.t

(** [modify filter doc f] runs [f] on each heading matched by [filter]
    in [doc], and returns the document thus modified.

    Note that [f] is given a heading where the children have /already/
    been filtered.
*)

val count : t -> Document.t -> int
(** Count the results (on a documet) *)

val count_headings : t -> Document.heading list -> int
(** Count the results (on headings) *)

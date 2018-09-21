(** Inline contents: type definition and output. *)

(** {1 Description} *)

(** This file deal with parsing inline contents. It is called /objects= in =org-element=.
This is the list of possible elements:
- emphasis (of several kind : italic, underline, bold, ...)
- entities : html or latex entities embded in org files, such as \alpha, &aacute, ...
- export-snippet [\@type\{value\}] : this construction is to insert
  raw data for a specific exporter:for instance [\@html\{\<p\>Paragraph\</p\>\}]
- footnote reference : [[fn:foo]] for instance
- inline babel call : make a call to a babel source block. More details [[http://orgmode.org/manual/Evaluating-code-blocks.html][here]].
- inline source block : [src_lang[arg]\{contents\}]
- latex fragment : [$foo$] or [\foo{}]
- line break : =\\\\= at the end of a line
- link : =[[descr][target]]=
- macro : text substitution [{{{name(arg1, arg2)}}}]. Documentation [[http://orgmode.org/manual/Macro-replacement.html][here]].
- radio target : special links  [<<<target>>>]
- targets : anchors [<<anchor>>]
- subscript [_{foo}]
- superscript [^{foo}]
- verbatim : [=plaincode=]
- statistics-cookie : statistics about the percentage of done children
- superscript/subscript : [^\{foo\}] and [_\{foo\}]
- timestamp : scheduled/deadline or plain/inactive timestamp

The file is organized as follows :
- definition of types corresponding to the category below
- definition of what a parser is (see below for more information)
- definition of parsers for each syntaxic categories
- glue that does the job
*)

(** {1 Type definitions} *)

(**
This section follows the following convention :
- a type for each category
- all types are mutually recursive, if not needed.

Indeed for some categories (link, emphasis, ...) we need to embded inline content.
*)

(** {2 Emphasis} *)

(** org supports three types of emphasis :
- bold (with stars : [*foo*])
- italic (with slashes : [/foo/])
- underline (with underscores : [_foo_])
*)
type emphasis = [`Bold | `Italic | `Underline] * t list

(** {2 Entities} *)

(** Entity are defined in the module {!Entity}. *)

and entity = Entity.t

(** {2 Export snippett} *)

(** An export snippet is given by a couple [(language, text)].*)

and export_snippet = string * string

(** {2 Footnote reference} *)

and footnote_reference = {
  name : string;
  definition : t list option;
}
(** A footnote reference contains:
    - a name (if not specified, generated)
    - a definition (optional) *)

(** {2 Inline call} *)
and inline_call = {
  program : string; (** The name of the block to call *)
  arguments : (string * string) list; (** The arguments to the block *)
  inside_headers : string option; (** The inside header arguments *)
  end_headers : string option; (** The end header arguments *)
}
(** See org's documentation for more information *)

(** {2 Inline source block} *)

and inline_source_block = {
  language: string; (** The language of the code block *)
  options: Hd_arguments.t; (** The options *)
  code: string; (** The code *)
}

(** {2 Latex fragments} *)

and latex_fragment =
  | Math of string (** A formula: $x+1$ *)
  | Command of string * string (** A command: [\command{argument}] *)

(** {2 Links} *)
(** Links are composed of two parts : an url and a label.
    An url may be pointed to a file, to a search or to an actual url *)

and url =
  | File of string (** The link refers to a local file *)
  | Search of string (** The link refers to a heading in the document *)
  | Complex of complex (** The link refers to an URI *)
and complex = {
  protocol: string; (** The protocol of the URI *)
  link: string (** The data *)
}

and link = {
  url: url; (** URL which the link refers to. *)
  label: t list; (** The label, containing inline contents *)
}

(** {2 Cookies} *)

(** Cookies are a way to indicate the progress of a task.
    They can be of two form : percentage or absolute value *)
and stats_cookie =
  | Percent of int
  | Absolute of int * int (** current, max *)


(** {2 Timestamps} *)

(** A clock item-- either stopped or
    started *)
and clock_item =
  | Started of Timestamp.t
  | Stopped of Timestamp.range

and timestamp =
  | Scheduled of Timestamp.t
  | Deadline of Timestamp.t
  | Date of Timestamp.t
  | Closed of Timestamp.t
  | Clock of clock_item
  | Range of Timestamp.range

(** {2 The type of inline contents} *)

(** The final type for {!Inline.t} is as follows: *)
and t =
  | Emphasis of emphasis
  | Entity of entity
  | Export_Snippet of export_snippet
  | Footnote_Reference of footnote_reference
  | Inline_Call of inline_call
  | Inline_Source_Block of inline_source_block
  | Latex_Fragment of latex_fragment
  | Break_Line
  | Link of link
  | Macro of string * string list
  | Radio_Target of string
  | Target of string
  | Subscript of t list
  | Superscript of t list
  | Verbatim of string
  | Cookie of stats_cookie
  | Timestamp of timestamp
  | List of t list
  | Plain of string

(** {1 Mappers and folders} *)
(** In this section we define mapper and folders overline. See Document for the
    full documentation of these traversal. *)

class ['a] mapper :
object
  method inline : 'a -> t -> t
  method inlines : 'a -> t list -> t list
end

class ['a] folder :
object
  method inline : 'a -> t -> 'a
  method inlines : 'a -> t list -> 'a
end

class virtual ['a] bottomUp :
object
  method virtual bot : 'a
  (** The default value for leaf *)

  method virtual combine : 'a list -> 'a
  (** Combining a list of result *)

  method inline : t -> 'a
  (** Traverse a single element *)

  method inlines : t list -> 'a
  (** Traverse a list of elements and combine their results *)

end
(** Implements a bottom up traversal of the tree
    where contents is created from the leaf and propagated upward.
    This is very useful for exporters *)

class virtual ['a, 'b] bottomUpWithArg :
object
  method virtual bot : 'a
  (** The default value for leaf *)

  method virtual combine : 'a list -> 'a
  (** Combining a list of result *)

  method inline : 'b -> t -> 'a
  (** Traverse a single element *)

  method inlines : 'b -> t list -> 'a
  (** Traverse a list of elements and combine their results *)
end
(** As bottomUp but this can take an argument that is propaged downwards.
    This is very useful for exporters *)

(** {1 Useful tools about inline} *)

val ascii : t -> string
val asciis : t list -> string
(** Convert inline contents to plain ascii *)

val string_of_url : url -> string
(** Convert an url to a string *)

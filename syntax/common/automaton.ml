open Prelude
open Batteries

module Make (C : sig
  type context
end) =
struct
  open C

  type input =
    { line: string
    ; context: context
    ; parse: context -> string Enum.t -> context * Block.blocks }

  type 'state return =
    | Next of 'state
    | Partial of 'state
    | Done of Block.blocks * bool

  module type Automaton = sig
    type state

    val parse_line : state -> input -> context * state return

    val interrupt :
         context
      -> state
      -> (context -> string Enum.t -> context * Block.blocks)
      -> context * Block.blocks

    val priority : int

    val is_start : input -> (context * state) option
  end

  type t = (module Automaton)

  let priority l =
    let module M = (val l : Automaton) in
    M.priority

  let sort l = List.sort (fun x y -> compare (priority y) (priority x)) l
end

open Batteries
module type SerializableType = sig
  type t
  val show : t -> string
  val read : string -> t option
  val description : string
end


type 'a serializable = (module SerializableType with type t = 'a)
(** An alias to hide the module *)
(** Some instances of SerializableType for most common types: *)  
    
let int = 
  let module Int = struct
    type t = int
    let description = "integer"
    let show = string_of_int
    let read s = 
      try Some (int_of_string s)
      with _ -> None
  end in
  (module Int : SerializableType with type t = int)
    
let boolean =
  let module Boolean = struct
    type t = bool
    let description = "boolean (true, yes, no, false)"
    let show b = if b then "yes" else "no"
    let read s = if s = "yes" || s = "true" then Some true
      else if s = "no" || s = "false" then Some false
      else None
  end in
  (module Boolean : SerializableType with type t = bool)
    
let string = 
  let module String = struct
    type t = string
    let description = "any string"
    let show x = x
    let read x = Some x
  end 
  in (module String : SerializableType with type t = string)


(* We use here Delimiters to parse the couples. *)    
let table = [
  '(', (')', false);
  '[', (']', false);
  '"', ('"', false);
  ',', (',', false);
]
module D = Delimiters.Make (struct let table = table end)
  
let couple (type v) (type u) (s1:u serializable) (s2:v serializable) = 
    let module S1 = (val s1 : SerializableType with type t = u) in
    let module S2 = (val s2 : SerializableType with type t = v) in
    let module Couple = struct
      type t = u * v
      let description = Printf.sprintf "Couple of (%s, %s)" 
        S1.description S2.description
      let show (a, b) = Printf.sprintf "(%s, %s)" (S1.show a) (S2.show b)
      let read s = 
        if String.length s = 0 
        || s.[0] <> '(' || s.[String.length s - 1] <> ')' then
          None
        else       
          let sub = BatSubstring.substring s 1 (String.length s - 2) in
          match D.split sub ',' with
            | [a; b] -> (match S1.read a, S2.read b with
                | Some a, Some b -> Some (a, b)
                | _ -> None)
            | _ -> None
    end
    in
    (module Couple : SerializableType with type t = u * v)


let list (type u) (s: u serializable) =
  let module S = (val s : SerializableType with type t = u) in
  let module List = struct
    type t = u list
    let description = Printf.sprintf "List of %s" S.description
    let show l = Printf.sprintf "[%s]" (String.concat ", " (List.map S.show l))
    let read s = 
      if String.length s = 0 
      || s.[0] <> '[' || s.[String.length s - 1] <> ']' then
        None
      else       
        let sub = BatSubstring.substring s 1 (String.length s - 2) in
        match D.split sub ',' with
          | l -> Some (List.filter_map S.read l)
  end
  in
  (module List : SerializableType with type t = u list)

let split c string = D.split (BatSubstring.all string) c
let parse_keyvalue s = 
  try Scanf.sscanf s "%[^=]=%[^\\00]" (fun key v -> (key, v))  
  with _ -> s, "yes"
let parse_comma s = 
  split ',' s |> List.map parse_keyvalue
      
  
module type Item = sig
  type t
  module T : SerializableType with type t = t
  val name : string
  val description : string
  val default : t
  val tmp : t ref
  val index: int
end
type 'a item = (module Item with type t = 'a)
type t = (module Item) list ref
type interface = (module Item) array

type instance = 
    {get : 'a. 'a item -> 'a}

let create () = ref []
let add (type u) config name (typ : u serializable) description default = 
  let module I = struct
    type t = u
    module T = (val typ : SerializableType with type t = u)
    let name = name and description = description
    and default = default and tmp = ref default
    let index = List.length !config
  end in
  config := (module I : Item) :: !config;
  (module I : Item with type t = u)
    
let concat configs = 
  let counter = ref (-1) in
  ref (
    List.map (fun (name, config) ->
      List.map (fun i ->
        let module I = (val i : Item) in
        incr counter;
        let module I'= struct
          include I
          let index = !counter
          let name = name ^ "." ^ I.name
        end
        in (module I' : Item))
        !config)
      configs |> List.concat
  )
let validate t = Array.of_list (List.rev !t)
(* this takes a config and a list of string * string 
   an existing instance, and create a composed instance *)
let append config list instance = 
  let array = Array.init
    (Array.length config)
    (fun k ->
      let module I = (val config.(k) : Item) in
      let v, b = try Option.get (I.T.read (List.assoc I.name list)), true
        with _ -> I.default, false in
      fun () -> I.tmp := v; b)
  in
  {get = fun (type u) item ->
    let module I = (val item : Item with type t = u) in
    if array.(I.index) () then
      !I.tmp
    else
      instance.get item}
    
let make config list = append config list
  {get = (fun (type u) i ->
    let module I = (val i : Item with type t = u) in
    I.default)
  }
let from_comma config s = parse_comma s |> make config
      

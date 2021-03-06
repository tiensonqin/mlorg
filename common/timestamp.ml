open Batteries

type date = {year: int; month: int; day: int}

type time = {hour: int; min: int}

(* TODO: delay *)
type t = {date: date; time: time option; repetition: date option; active: bool}

type range = {start: t; stop: t}

let year t = t.date.year

let month t = t.date.month

let day t = t.date.day

let hour t = Option.map_default (fun x -> x.hour) 0 t.time

let min t = Option.map_default (fun x -> x.min) 0 t.time

let hour_opt t = Option.map_default (fun x -> Some x.hour) None t.time

let min_opt t = Option.map_default (fun x -> Some x.min) None t.time

let null_date = {year= 0; month= 0; day= 0}

(* let null_time = {min= 0; hour= 0} *)

let null = {date= null_date; time= None; repetition= None; active= true}

let to_tm t =
  let open Unix in
  let tm =
    { tm_sec= 0
    ; tm_min= min t
    ; tm_hour= hour t
    ; tm_mday= day t
    ; tm_mon= month t - 1
    ; tm_year= year t - 1900
    ; tm_wday= 0
    ; tm_yday= 0
    ; tm_isdst= false }
  in
  snd (mktime tm)

let from_tm ?(active = true) tm =
  let open Unix in
  let tm = snd (mktime tm) in
  { date= {year= tm.tm_year + 1900; month= tm.tm_mon + 1; day= tm.tm_mday}
  ; time= Some {hour= tm.tm_hour; min= tm.tm_min}
  ; active
  ; repetition= None }

let normalize t =
  let normalized = from_tm (to_tm t) in
  let t' = {normalized with repetition= t.repetition} in
  if t.time = None then {t' with time= None} else t'

let weekday t = (to_tm t).Unix.tm_wday

let parse_time s =
  try Scanf.sscanf s "%d:%d" (fun hour min -> Some {hour; min}) with _ -> None

let parse_date s =
  try Scanf.sscanf s "%d-%d-%d" (fun year month day -> Some {year; month; day})
  with _ -> None

let parse_repetition_marker s =
  try
    Scanf.sscanf s "+%d%c" (fun n c ->
        match c with
        | 'w' -> Some {null_date with day= 7 * n}
        | 'd' -> Some {null_date with day= n}
        | 'm' -> Some {null_date with month= n}
        | 'y' -> Some {null_date with year= n}
        | _ -> None )
  with _ -> None

let parse_timestamp_part timestamp s =
  let isdigit c = match c with '0' .. '9' -> true | _ -> false in
  if s.[0] = '+' then {timestamp with repetition= parse_repetition_marker s}
  else if isdigit s.[0] then {timestamp with time= parse_time s}
  else timestamp

module D = Delimiters.Make (struct
  let table = [('[', (']', false)); ('<', ('>', false))]
end)

let parse_substring s =
  let open BatSubstring in
  if length s > 0 && (get s 0 = '[' || get s 0 = '<') then
    match D.enclosing_delimiter s (get s 0) with
    | None -> None
    | Some (parts, rest) -> (
      match Prelude.words parts with
      | [] -> None
      | date_s :: rem_parts -> (
        match parse_date date_s with
        | None -> None
        | Some date ->
            let active = get s 0 = '<' in
            let timestamp =
              List.fold_left parse_timestamp_part {null with date; active}
                rem_parts
            in
            Some (timestamp, rest) ) )
  else None

let parse s =
  Option.map
    (fun (a, b) -> (a, BatSubstring.to_string b))
    (parse_substring (BatSubstring.all s))

let parse_range_substring s =
  let open BatSubstring in
  match parse_substring s with
  | None -> None
  | Some (start, rest) -> (
      if length rest < 2 || get rest 0 <> '-' || get rest 1 <> '-' then None
      else
        match parse_substring (triml 2 rest) with
        | Some (stop, rest) -> Some ({start; stop}, rest)
        | None -> None )

let parse_range s =
  Option.map
    (fun (a, b) -> (a, BatSubstring.to_string b))
    (parse_range_substring (BatSubstring.all s))

let date_to_string d = Printf.sprintf "%d-%02d-%02d" d.year d.month d.day

let time_to_string t = Printf.sprintf "%02d:%02d" t.hour t.min

let repetition_to_string d =
  if d.year <> 0 then Printf.sprintf "+%dy" d.year
  else if d.month <> 0 then Printf.sprintf "+%dy" d.month
  else if d.day <> 0 then Printf.sprintf "+%dd" d.day
  else "+0d"

let to_string ?(wday = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Wed"; "Sat"|]) t =
  Printf.sprintf "%c%s%c"
    (if t.active then '<' else '[')
    ( [ Some (date_to_string t.date)
      ; Some wday.(weekday t)
      ; Option.map time_to_string t.time
      ; Option.map repetition_to_string t.repetition ]
    |> List.filter_map identity |> String.concat " " )
    (if t.active then '>' else ']')

let range_to_string {start; stop} =
  Printf.sprintf "%s--%s" (to_string start) (to_string stop)

let seconds_of_t t = to_tm t |> Unix.mktime |> fst

let duration {start; stop} = truncate (seconds_of_t stop -. seconds_of_t start)

let from_now t =
  duration {start= t; stop= from_tm (Unix.localtime (Unix.time ()))}

let string_of_seconds n =
  let mins, _secs = (n / 60, n mod 60) in
  let hours, mins = (mins / 60, mins mod 60) in
  Printf.sprintf "%02d:%02d" hours mins

let add_days t d = normalize {t with date= {t.date with day= t.date.day + d}}

let today () = Unix.time () |> Unix.localtime |> from_tm

let sub d d' =
  {year= d'.year - d.year; month= d'.month - d.month; day= d'.day - d.day}

let covers arg source =
  match source.repetition with
  | None -> source.date = arg.date
  | Some repetition ->
      if arg < source then false
      else
        let sub = sub arg.date source.date in
        (repetition.year = 0 || sub.year mod repetition.year = 0)
        && (repetition.month = 0 || sub.month mod repetition.month = 0)
        && (repetition.day = 0 || sub.day mod repetition.day = 0)

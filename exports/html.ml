open Timestamp
open Xml
open Prelude
open Entity
open Batteries
open Inline
open Block
open Document
open Plugin
open Config

module H = struct
  let name = "html"

  let config = Config.create ()

  let opt_attr name = function Some v -> [(name, v)] | None -> []

  let encoding =
    Config.add config "encoding" string "The document's encoding" "utf-8"

  let full =
    Config.add config "wrap" boolean
      "Shall the output be a full html document ?" true

  let style =
    Config.add config "style" string "The stylesheet to use" "style.css"

  let number_lines =
    Config.add config "number-lines" boolean
      "Shall lines be numbered inside code blocks" false

  let use_math2png =
    Config.add config "use-math2png" boolean
      "Convert latex formulas to PNG using Math2png extension" false

  let image_extensions =
    Config.add config "image-extensions" (list string)
      "The list of extensions to be considered as images"
      [".png"; ".jpg"; ".jpeg"; ".gif"; ".bmp"]

  let mathjax_url =
    Config.add config "mathjax-url" string "The URL of the MathJax script"
      "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"

  let js_files =
    Config.add config "javascript-files" (list string)
      "Javascript files to include in the page (URLs)" []

  let inline_js =
    Config.add config "javascript" string
      "Inline javascript code to add in the <head> section" ""

  let use_pygments =
    Config.add config "use-pygments" boolean
      "Shall we use pygments to color code ?" true

  type interface = exporter

  let concatmap f l = List.concat (List.map f l)

  let assoc l s = try List.assoc s l with _ -> ""

  let mathjax_used = ref false

  class htmlExporter config =
    object (self)
      inherit [Xml.t list] Document.bottomUp as super

      method bot = []

      method combine = List.concat

      method handle_image_link url href label =
        match url with
        | Complex {protocol; link} ->
            (* slight hack here to handle math2png annotations *)
            let opts, href =
              try
                Scanf.sscanf protocol "depth-%d" (fun n ->
                    ( [("style", Printf.sprintf "vertical-align: -%dpx" n)]
                    , link ) )
              with _ -> ([], href)
            in
            [ Xml.block "img"
                ~attr:(opts @ [("src", href); ("title", Inline.asciis label)])
                [] ]
        | Search s | File s ->
            [ Xml.block "img"
                ~attr:[("src", href); ("title", Inline.asciis label)]
                [] ]

      method inline =
        function
        | Plain s -> [Xml.data s]
        | Superscript l -> [Xml.block "sup" (self#inlines l)]
        | Subscript l -> [Xml.block "sub" (self#inlines l)]
        | Emphasis (kind, data) ->
            let l = [(`Bold, "b"); (`Italic, "i"); (`Underline, "u")] in
            [Xml.block (List.assoc kind l) (self#inlines data)]
        | Entity e -> [Xml.raw e.html]
        | Latex_Fragment (Inline.Math s) ->
            mathjax_used := true ;
            [Xml.data ("\\(" ^ s ^ "\\)")]
        | Link {url; label} ->
            let href = Inline.string_of_url url in
            (* If it is an image *)
            if
              List.exists (String.ends_with href)
                (Config.get config image_extensions)
            then self#handle_image_link url href label
            else
              let href =
                match url with Search x -> "#" ^ Toc.link x | _ -> href
              in
              [Xml.block "a" ~attr:[("href", href)] (self#inlines label)]
        | Verbatim s -> [Xml.block "code" [Xml.data s]]
        | Inline_Source_Block x -> [Xml.block "code" [Xml.data x.code]]
        | Export_Snippet ("html", s) -> [Xml.raw s]
        | Break_Line -> [Xml.block "br" []]
        | Target s -> [Xml.block "a" ~attr:[("id", s)] []]
        | Timestamp (Scheduled t) -> [self#timestamp t "Scheduled"]
        | Timestamp (Deadline t) -> [self#timestamp t "Deadline"]
        | Timestamp (Date t) -> [self#timestamp t "Date"]
        | Timestamp (Range t) -> [self#range t false]
        | Timestamp (Closed t) -> [self#timestamp t "Closed"]
        | Timestamp (Clock (Stopped t)) -> [self#range t true]
        | Timestamp (Clock (Started t)) -> [self#timestamp t "Started"]
        | x -> super#inline x

      method range {start; stop} stopped =
        Xml.block "div"
          ~attr:
            [("class", "timestamp-range"); ("stopped", string_of_bool stopped)]
          [self#timestamp start "Start"; self#timestamp stop "Stop"]

      method timestamp ({active; date; time; repetition} as t) kind =
        let prefix =
          match kind with
          | "Scheduled" ->
              Xml.raw
                "<i class=\"fa fa-calendar\" style=\"margin-right:6px;\"></i>"
          | "Deadline" ->
              Xml.raw
                "<i class=\"fa fa-calendar-times-o\" \
                 style=\"margin-right:6px;\"></i>"
          | "Date" -> Xml.empty
          | "Closed" -> Xml.empty
          | "Started" ->
              Xml.raw
                "<i class=\"fa fa-clock-o\" style=\"margin-right:6px;\"></i>"
          | "Start" -> Xml.data "From: "
          | "Stop" -> Xml.data "To: "
        in
        Xml.block "span"
          ~attr:
            [ ( "class"
              , "timestamp " ^ if kind = "Closed" then "line-through" else ""
              )
            ; ("active", if active then "true" else "false") ]
          [prefix; Xml.data (to_string t)]

      method list_item x =
        let contents =
          match x.contents with
          | Paragraph i :: rest -> self#inlines i @ self#blocks rest
          | _ -> self#blocks x.contents
        in
        let checked, checked_html =
          match x.checkbox with
          | Some x ->
              ( x
              , if x then
                  Xml.raw
                    "<i class=\"fa fa-check-square-o\" \
                     style=\"margin-right:6px;\"></i>"
                else
                  Xml.raw
                    "<i class=\"fa fa-square-o\" \
                     style=\"margin-right:6px;\"></i>" )
          | _ -> (false, Xml.empty)
        in
        match x.number with
        | None ->
            [ Xml.block
                ~attr:[("checked", string_of_bool checked)]
                "li"
                [Xml.block "p" (checked_html :: contents)] ]
        | Some number ->
            [ Xml.block
                ~attr:
                  [ ("style", "list-style-type: none")
                  ; ("checked", string_of_bool checked) ]
                "li"
                [ Xml.block "p"
                    (Xml.data (number ^ " ") :: checked_html :: contents) ] ]

      method block =
        function
        | Paragraph l -> [Xml.block "p" (self#inlines l)]
        | Horizontal_Rule -> [Xml.block "hr" []]
        | List (l, _) -> [Xml.block "ul" (concatmap self#list_item l)]
        | Example (_, l) -> [Xml.block "pre" [Xml.data (String.concat "\n" l)]]
        | Src {language; lines} ->
            if Config.get config use_pygments then (
              try
                [ Xml.raw
                    (Pygments.color config language "html" (List.map fst lines))
                ]
              with Command.Failed (command, message) ->
                Log.warning "While running pygments (%s): %s" command message ;
                [ Xml.block "pre"
                    [Xml.data (String.concat "\n" (List.map fst lines))] ] )
            else
              [ Xml.block "pre"
                  [Xml.data (String.concat "\n" (List.map fst lines))] ]
        | Custom (name, _, l) ->
            [Xml.block "div" ~attr:[("class", name)] (self#blocks l)]
        | Math s ->
            mathjax_used := true ;
            [ Xml.block "div" ~attr:[("class", "mathblock")]
                [Xml.data ("$$" ^ s ^ "$$")] ]
        | Quote l -> [Xml.block "blockquote" (self#blocks l)]
        | Table {rows} ->
            let lmap name f arr =
              Array.to_list (Array.map (Xml.block name % f) arr)
            in
            [ Xml.block ~attr:[("border", "1")] "table"
                (lmap "colgroup" (lmap "tr" (lmap "td" self#inlines)) rows) ]
        | x -> super#block x

      method heading d =
        let attr = [("id", Toc.link d.anchor)] in
        let marker =
          match d.marker with
          | Some v -> (
            match v with
            | "TODO" | "todo" ->
                Xml.raw "<span class=\"task-status todo\">TODO</span>"
            | "DONE" | "done" ->
                Xml.raw "<span class=\"task-status done\">DONE</span>"
            | v ->
                Xml.raw
                  (Printf.sprintf "<span class=\"task-status %s\">%s</span>"
                     (String.lowercase_ascii v) (String.uppercase_ascii v)) )
          | None -> Xml.empty
        in
        let priority =
          match d.priority with
          | Some v ->
              Xml.raw (Printf.sprintf "<span class=\"priority\">%c</span>" v)
          | None -> Xml.empty
        in
        let tags =
          match d.tags with
          | [] -> Xml.empty
          | tags ->
              Xml.block "span"
                ~attr:[("class", "heading-tags")]
                (List.map
                   (fun tag ->
                     Xml.raw
                       (Printf.sprintf "<span class=\"heading-tag\">%s</span>"
                          tag) )
                   tags)
        in
        let children =
          self#blocks d.content @ concatmap self#heading d.children
        in
        Xml.block ~attr
          (Printf.sprintf "h%d" d.level)
          (self#inlines d.name @ [marker; priority; tags])
        :: children

      method document d =
        [ Xml.block "div" ~attr:[("id", "content")]
            (self#blocks d.beginning @ concatmap self#heading d.headings) ]
    end

  let doctype =
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \
     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"

  let with_custom_exporter o config doc out =
    let doc = Toc.transform config doc in
    let doc =
      if Config.get config use_math2png then Math2png.transform config doc
      else doc
    in
    Xml.output_xhtml out (o#document doc)

  module E = struct
    let export config doc =
      with_custom_exporter (new htmlExporter config) config doc

    let default_filename = change_ext "html"
  end

  let data = (module E : Exporter)
end

module Config = struct
  include H
end

include H
include H.E

let _ = Plugin.Exporters.add (module H : Plugin with type interface = exporter)

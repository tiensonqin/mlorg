open Prelude
open Entity
open Batteries
open Inline
open Block
open Document
open Plugin
open Config
module E = struct
  let name = "latex"
  let config = Config.create ()
  let classname = 
    add config "classname" string "The LaTeX class name to use" "article"
  let header = add config "header" string
    "The LaTeX header. You can use the following variables in it:
- classname: the class name chosen for this document
- packages: the list of packages to be loaded (formatted)
- extraheader: user's extra header
- title, author: document's metadata"
"\\documentclass{$classname}
$packages
$extraheader
\\title{$title}
\\author{$author}
\\begin{document}
\\maketitle
"
  let footer = add config "footer" string "The LaTeX footer"
    "\\end{document}"
  let extraheader = add config "extraheader" string "Extra LaTeX header" ""
    
  let sections = add config "sections" (list string)
    "The name of the sections"
    ["section"; "subsection"; "subsubsection"; "paragraph"; "subparagraph"]

  let assoc l s = try List.assoc s l with _ -> ""

  let escape_inside s = s
  let tex_escape = escape ["}"; "{"; "$"; "\\"; "["; "]"]
  let write_header { get } out doc =
    let vars = ["classname", escape_inside (get classname);
                "packages", "";
                "extraheader", get extraheader;
                "title", escape_inside doc.title;
                "author", escape_inside doc.author;
               ]
    in
    IO.nwrite out (substitute (assoc vars) (get header))

  let export { get } doc out = 
    let toc = Toc.gather doc in
    let o = object(self)
      inherit [unit] Document.folder as super

      method header () = write_header { get } out doc
      method footer () = 
        IO.nwrite out (get footer)
      method inline () = function
        | Plain s -> IO.nwrite out (tex_escape s)
        | Emphasis (kind, data) ->
          let l = [`Bold, "textbf"; `Italic, "emph"; `Underline, "underline"] in
          Printf.fprintf out "\\%s{" (List.assoc kind l);
          self#inlines () data;
          Printf.fprintf out "}"
        | Break_Line -> Printf.fprintf out "\\\\\n"
        | Entity e -> 
          if not e.latex_mathp then 
            Printf.fprintf out "%s" e.latex
          else
            Printf.fprintf out "$%s$" e.latex
        | Latex_Fragment (Inline.Math s) ->
          Printf.fprintf out "$%s$" (escape ["$"] s)
        | Latex_Fragment (Command (name, "")) ->
          Printf.fprintf out "\\%s" name
        | Latex_Fragment (Command (name, option)) ->
          Printf.fprintf out "\\%s{%s}" name (escape_inside option)
        | Verbatim s -> 
          Printf.fprintf out "\\texttt{%s}" (escape ["}"] s)
        | Target s ->
          Printf.fprintf out "\\label{%s}" (Toc.link s)
        | Link {url; label} ->
            (match url, label with
              | Search title, _ ->
                  if Toc.mem title toc then
                    Printf.fprintf out "\\ref{sec:%s}" (Toc.link title)
                  else
                    Printf.fprintf out "\\ref{%s}" title
              | _, label ->
                  Printf.fprintf out "\\hyperref[%s]{"
                    (Inline.string_of_url url);
                  self#inlines () label;
                  Printf.fprintf out "}")
        | x -> super#inline () x
      method list_item () i = (match i.number with
        | Some c -> Printf.fprintf out "  \\item[%s] " c
        | _ -> Printf.fprintf out "\\item ");
        self#blocks () i.contents

      method block () = function
        | Paragraph l -> self#inlines () l; Printf.fprintf out "\n\n"
        | List (i, _) ->
          Printf.fprintf out "\\begin{itemize}\n";
          List.fold_left self#list_item () i;
          Printf.fprintf out "\\end{itemize}\n";

        | Latex_Environment (name, opts, lines) ->
            Printf.fprintf out "\\begin{%s}%s\n" name opts;
            List.iter (Printf.fprintf out "%s\n") lines;
            Printf.fprintf out "\\end{%s}\n" name
        | Math b ->
            Printf.fprintf out "$$%s$$\n" b
        | Custom ("tableofcontents", _, _) ->
            Printf.fprintf out "\\tableofcontents\n"
        | Custom (name, opts, l) ->
          Printf.fprintf out "\\begin{%s}{%s}\n" (escape_inside name)
            (escape_inside opts);
          self#blocks () l;
          Printf.fprintf out "\\end{%s}\n" (escape_inside name)
        | With_Keywords (l, Custom ("figure", opts, lines)) ->
            Printf.fprintf out "\\begin{figure}%s\n\\centering" opts;
          self#blocks () lines;
            (try Printf.fprintf out "\\caption{%s}\n" (List.assoc "CAPTION" l)
            with _ -> ());
          Printf.fprintf out "\\end{figure}\n"
            
        | x -> super#block () x
      method heading () d = 
        let command = List.nth (get sections) (d.level - 1) in
        Printf.fprintf out "\\%s{" command;
        self#inlines () d.name;
        Printf.fprintf out "}\\label{sec:%s}\n" (Toc.link (Inline.asciis d.name));
        self#blocks () d.content;
        List.iter (self#heading ()) d.children
      method document () d = 
        self#header ();
        super#document () d;
        self#footer ()
    end
    in o#document () doc

  module D = struct let export = export and default_filename = change_ext "ext" end
  type interface = exporter
  let data = (module D : Exporter)
end

let _ = Exporters.add (module E : Plugin with type interface = exporter)        

(* mlorg's js main *)
open Batteries
open Mlorg

let _ =
  let open Js_of_ocaml in
  Js.export "Orgmode"
    (object%js
      (* method json input =
       *   let str = Js.to_string input in
       *   let lines = String.nsplit str ~by:"\n" in
       *   let (_context, blocks) = Org_parser.parse @@ List.enum lines in
       *   blocks
       *   (\* Js_of_ocaml.Js.string @@ Edn_cconv.to_string blocks *\) *)


      method parse input =
        let str = Js.to_string input in
        let _ = Printexc.record_backtrace true in
        let config = Plugin.global_config [] in
        let export = Plugin.Exporters.find "html" in
        let module E = (val export : Plugin.Exporter) in
        let buffer = Buffer.create 1024 in
        let output = Buffer.output_buffer buffer in
        let (doc, config) = (Document.from_string ~config "<string>" str) in
        Plugin.Exporters.run export config doc output;
        Js_of_ocaml.Js.string (Buffer.contents buffer)
    end)

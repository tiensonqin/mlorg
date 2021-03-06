(** Entry point of the mlorg library *)

module Document = Document
module Block = Block
module Delimiters = Delimiters
module Inline = Inline
module Config = Config
module Plugin = Plugin
module Filter = Filter
module Timestamp = Timestamp

module Syntaxes = struct
  module Org = struct
    module Inline = Org_inline
    module Parser = Org_parser
  end
end

module Backends = struct
  module Html = Html
end

module Xml = Xml
module Utils = Utils

(** Alias for [Syntaxes.Org.Inline.parse] *)
let org_parse_inline = Syntaxes.Org.Inline.parse

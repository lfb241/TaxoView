module Tree exposing (..)

import Metadata exposing(..)

--- id, label, rank, metadata, children
type TreeNode =
  TreeNode String String String (Maybe (List Metadata)) (Maybe (List TreeNode))


-- TODO: Handle decoding
-- decodeTreeString : String -> Result Json.Decode.Error (List(TreeNode))

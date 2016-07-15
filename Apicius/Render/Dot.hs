{-# LANGUAGE ParallelListComp #-}

module Apicius.Render.Dot (showDotGraph) where

import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T

import Apicius.Language
import Apicius.ReverseTree
import Apicius.Util

showDotGraph :: Recipe -> Text
showDotGraph r@(Recipe name _) =
  dotGraph name . buildReverseGraph . getChunks $ r

dotGraph :: Text -> ReverseGraph -> Text
dotGraph rname gr =
  ("digraph \"" <> rname <> "\" {\n") <> T.unlines (go "n" 0 gr) <> "\n}"
  where go :: Text -> Int -> ReverseGraph -> [Text]
        go parent n (ReverseGraph t rs) =
          let name = parent <> "_" <> text n
              children = [ (i, name <> "_" <> text i, r)
                         | i <- [0..]
                         | r <- rs
                         ]
          in [ "  " <> name <> stepMeta t ] ++
             [ "  " <> cname <> " -> " <> name <> ";"
             | (_, cname, _) <- children
             ] ++
             concat [ go name i r
                    | (i, _, r) <- children
                    ]

stepMeta :: Either IngredientList Text -> Text
stepMeta (Right t) = " [label=\"" <> t <> "\",color=red]"
stepMeta (Left (IngredientList is)) =
  " [label=\"" <> T.intercalate "; " [ ingName i | i <- is ] <> "\"]"

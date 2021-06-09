module Apicius.Language
  ( Recipe (..),
    Step (..),
    Input (..),
    IngredientList (..),
    Ingredient (..),
    Action (..),
    parseFile,
    showAst,
  )
where

import Apicius.AST
import Apicius.Parser
import Apicius.Util (text)
import Data.Text (Text)

showAst :: Recipe -> Text
showAst = text

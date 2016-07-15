module Apicius.Language ( Recipe(..)
                        , Step(..)
                        , Input(..)
                        , IngredientList(..)
                        , Ingredient(..)
                        , Action(..)
                        , parseFile
                        , showAst
                        ) where

import Data.Text (Text)

import Apicius.AST
import Apicius.Parser
import Apicius.Util (text)

showAst :: Recipe -> Text
showAst = text

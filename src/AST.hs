module AST where

import Data.Monoid ((<>))
import Data.Text (Text)
import Util

data Recipe = Recipe
  { rName   :: Text
  , rRecipe :: [Step]
  } deriving (Eq, Show)

instance TShow Recipe where
  text (Recipe name steps) =
    "Recipe { rName = " <>
    text name <>
    ", rRecipe = " <>
    text steps <>
    " }"

data Step = Step
  { sInputs  :: Input
  , sActions :: [Action]
  } deriving (Eq, Show)

instance TShow Step where
  text (Step inp acts) =
    "Step { sInputs = " <>
    text inp <>
    ", sActions = " <>
    text acts <>
    " }"

data Input
  = InpIngredients IngredientList
  | InpJoin Text
    deriving (Eq, Show)

instance TShow Input where
  text (InpJoin ts) = text ts
  text (InpIngredients is) = text is

data IngredientList = IngredientList
  { fromIngredientList :: [Ingredient]
  } deriving (Eq, Show)

instance TShow IngredientList where
  text (IngredientList is) =
    "IngredientList " <> text is

data Ingredient = Ingredient
  { iAmount :: Maybe Text
  , iType   :: Text
  } deriving (Eq, Show)

instance TShow Ingredient where
  text (Ingredient i t) =
    "Ingredient { iAmount = " <>
    text i <>
    ", iType = " <>
    text t <>
    " }"

data Action
  = Action Text (Maybe IngredientList)
  | Join Text
  | Done
    deriving (Eq, Show)

instance TShow Action where
  text (Action t i) = "Action " <> text t <> " " <> text i
  text (Join t) = "Join " <> text t
  text Done = "Done"


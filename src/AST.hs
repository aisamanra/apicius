module AST where

import           Data.Text (Text)

data Recipe = Recipe
  { rName   :: Text
  , rRecipe :: [Step]
  } deriving (Eq, Show)

data Step = Step
  { sInputs  :: Input
  , sActions :: [Action]
  } deriving (Eq, Show)

data Input
  = InpIngredients IngredientList
  | InpJoin Text
    deriving (Eq, Show)

data IngredientList = IngredientList
  { fromIngredientList :: [Ingredient]
  } deriving (Eq, Show)

data Ingredient = Ingredient
  { iAmount :: Maybe Text
  , iType   :: Text
  } deriving (Eq, Show)

data Action
  = Action Text (Maybe IngredientList)
  | Join Text
  | Done
    deriving (Eq, Show)

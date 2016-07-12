module Main where

import           Data.List (intercalate)
import qualified Data.Text as T
import           Data.Text (Text)

import AST
import Parser (parseFile)

main :: IO ()
main = do
  cs <- getContents
  case parseFile "[stdin]" cs of
    Left err -> putStrLn err
    Right recipes ->
      putStrLn (unlines (map showRecipeGraph recipes))

-- An 'ActionChunk' represents a set of actions in between two join
-- points. This is 'reversed' from what we'd expect: the 'name' is
-- actually the name of the join point at the end of a sequence of
-- actions, or the string DONE, while the 'prev' is the name of the
-- join point that came at the beginning, or the ingredients list
-- that started the rule. The actions also will appear in reverse
-- order.

-- Maybe an explanation is in order: this rule
--   ingredients -> a -> $x -> b -> c -> $y;
-- will produce two ActionChunks:
--   ActionChunk $y [c, b] (Right $x)
-- and
--   ActionChunk $x [a] (Left ingredients)
data ActionChunk = ActionChunk
  { acName  :: Text
  , acRules :: [Text]
  , acPrev  :: Either IngredientList Text
  } deriving (Eq, Show)

-- This is the function that actually splits apart the action into
-- ActionChunks. It's grosser than I'd hoped, but it's mostly a lot
-- of fiddly but straightforward traversing.
splitApart :: Either IngredientList Text -> [Action] -> [ActionChunk]
splitApart i = toChunk [] . reverse
  where toChunk cs (Join t:xs) =
          gather t xs [] cs
        toChunk cs (Action "DONE" _:xs) =
          gather "DONE" xs [] cs
        toChunk cs (Done:xs) =
          gather "DONE" xs [] cs
        toChunk _ (Action _ _:_) =
          error "expected chunk to end with a join or DONE"
        toChunk cs [] = cs
        gather n xs@(Join t:_) as cs =
          toChunk (ActionChunk n (reverse as) (Right t) : cs) xs
        gather n (Action t _:xs) as cs =
          gather n xs (t:as) cs
        gather _ (Done:_) _ _ =
          error "unsure how to handle this case"
        gather n [] as cs =
          ActionChunk n (reverse as) i : cs

-- Here we take a recipe and pull all the ActionChunks into a single
-- list.
getChunks :: Recipe -> [ActionChunk]
getChunks Recipe { rRecipe = st } =
  mconcat (map getActions st)
  where getActions (Step (InpJoin t) as) = splitApart (Right t) as
        getActions (Step (InpIngredients is) as) = splitApart (Left is) as

-- The ReverseGraph is a tree rooted at the DONE node. The 'children'
-- are actually the steps leading up to a given node. Only childless
-- nodes should have an IngredientList associated with them, but we
-- don't encode this invariant in the type.
data ReverseGraph = ReverseGraph
  { rStep  :: Either IngredientList Text
  , rPrevs :: [ReverseGraph]
  } deriving (Eq, Show)

-- Take a list of ActionChunks and stitch them back together so that
-- we can build a ReverseGraph of them. Again, fiddly but straightforward
-- traversing of the data structures.
buildReverseGraph :: [ActionChunk] -> ReverseGraph
buildReverseGraph as = ReverseGraph (Right "DONE")
                         (concat (map buildFrom (findChunks "DONE")))
  where findChunks n = [ chunk | chunk <- as, acName chunk == n ]
        buildFrom (ActionChunk _ rs p) = go rs p
        go [] (Right p) = concat $ map buildFrom (findChunks p)
        go [] (Left i)  = [ReverseGraph (Left i) []]
        go (r:rs) p = [ReverseGraph (Right r) (go rs p)]

-- Prettily convert a ReverseGraph to a readable tree. This will give
-- us a recipe tree in reverse order, starting with the DONE, and
-- gradually going back to the ingredients.
prettyGraph :: ReverseGraph -> String
prettyGraph = go 0
  where go n (ReverseGraph t rs) =
          indent n ++ stepName t ++ "\n" ++ concat (map (go (n+2)) rs)
        indent n = replicate n ' '
        stepName (Right t) = T.unpack t
        stepName (Left (IngredientList is)) =
          intercalate "; " [ ingName i | i <- is ]
        ingName (Ingredient (Just amt) name) =
          T.unpack amt ++ " " ++ T.unpack name
        ingName (Ingredient Nothing name) =
          T.unpack name

showRecipeGraph :: Recipe -> String
showRecipeGraph = prettyGraph . buildReverseGraph . getChunks

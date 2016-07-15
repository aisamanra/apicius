module Main where

import           Control.Error
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           System.Environment (getArgs)
import           System.IO ( IOMode(WriteMode)
                           , hClose
                           , openFile
                           , stdout
                           )

import Apicius.ReverseTree (showFragments, showReverseTree)
import Apicius.Render.Dot (showDotGraph)
import Apicius.Language (Recipe, parseFile, showAst)

usage :: String
usage =
  unlines ("\n\nCOMMANDS:" : [ "  " ++ r | (r, _) <- renderers ])

renderers :: [(String, Recipe -> Text)]
renderers =
  [ ("ast",          showAst)
  , ("fragments",    showFragments)
  , ("dot",          showDotGraph)
  , ("reverse-tree", showReverseTree)
  ]

main :: IO ()
main = runScript $ do
  as <- scriptIO getArgs
  (file, render, contents, output) <- case as of
    [x] -> do
      r <- lookup x renderers ?? ("Unable to find renderer " ++ x)
      cs <- scriptIO getContents
      return ("[stdin]", r, cs, stdout)
    [x,f] -> do
      r <- lookup x renderers ?? ("Unable to find renderer " ++ x)
      cs <- scriptIO $ readFile f
      return (f, r, cs, stdout)
    [x,f,o] -> do
      r <- lookup x renderers ?? ("Unable to find renderer " ++ x)
      cs <- scriptIO $ readFile f
      file <- scriptIO $ openFile o WriteMode
      return (f, r, cs, file)
    _ -> throwE usage
  ast <- hoistEither $ parseFile file contents
  mapM_ (scriptIO . T.hPutStrLn output . render) ast
  scriptIO $ hClose output

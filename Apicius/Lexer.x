{
{-# OPTIONS -w #-}
{-# LANGUAGE OverloadedStrings #-}

module Apicius.Lexer where

import           Control.Monad (liftM)
import           Data.Text (Text)
import qualified Data.Text as T

import           Prelude hiding (lex)
}

%wrapper "monadUserState"

$special = [ \{ \} \[ \] \( \) \; \, \+ \& \- \$ ]
$idchar = $printable # $special

tokens :-
  $white+ ;
  "#".* ;

  DONE  { lex' TkDone }

  \{  { lex' TkLCurl }
  \}  { lex' TkRCurl }
  \[  { lex' TkLBrac }
  \]  { lex' TkRBrac }
  \;  { lex' TkSemi  }
  \+  { lex' TkPlus }
  \&  { lex' TkAnd }

  \-\> { lex' TkArrow }

  $idchar + { lex (TkText . T.strip) }

  \$ $idchar + { lex (TkJoin . T.strip) }

{
data Token = Token AlexPosn TkType deriving (Eq, Show)

data TkType
  = TkLCurl
  | TkRCurl
  | TkLBrac
  | TkRBrac
  | TkArrow
  | TkAnd
  | TkSemi
  | TkDone
  | TkPlus
  | TkText Text
  | TkJoin Text
  | TkEOF
    deriving (Eq, Show)

data AlexUserState = AlexUserState
  { filePath :: FilePath
  } deriving (Eq, Show)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return (Token p TkEOF)


lex :: (Text -> TkType) -> AlexAction Token
lex f = \(p,_,_,s) i -> return (Token p (f (T.pack (take i s))))

lex' :: TkType -> AlexAction Token
lex' = lex . const

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' mote fp input = runAlex input (setFilePath fp >> mote)
}

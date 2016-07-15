{
{-# LANGUAGE OverloadedStrings #-}

module Apicius.Parser where

import Apicius.AST
import Apicius.Lexer

}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TkEOF }
%error { happyError }

%token
  '{' { Token _ TkLCurl }
  '}' { Token _ TkRCurl }
  '[' { Token _ TkLBrac }
  ']' { Token _ TkRBrac }
  ';' { Token _ TkSemi  }
  '+' { Token _ TkPlus }
  '&' { Token _ TkAnd }

  '->' { Token _ TkArrow }
  done { Token _ TkDone }

  text { Token _ (TkText $$) }
  join { Token _ (TkJoin $$) }

%%

file
  : recipe file { $1 : $2 }
  |             { [] }

recipe
  : text '{' steps '}' { Recipe $1 $3 }

steps
  : step ';' steps { $1 : $3 }
  |                { [] }

step
  : input '->' actions { Step $1 $3 }

input
  : ilist { InpIngredients $1 }
  | join  { InpJoin $1 }

ilist
  : ingredients { IngredientList $1 }

ingredients
  : ingredient '+' ingredients { $1 : $3 }
  | ingredient                 { [$1] }

ingredient
  : '[' text ']' text { Ingredient (Just $2) $4 }
  | text              { Ingredient Nothing   $1 }

actions
  : action '->' actions { $1 : $3 }
  | action              { [$1] }

action
  : text { Action $1 Nothing }
  | text '&' ilist { Action $1 (Just $3) }
  | join { Join $1 }
  | done { Done }

{

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token " ++ show t)

parseFile :: FilePath -> String -> Either String [Recipe]
parseFile = runAlex' parse
}

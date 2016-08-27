module Main where

import Data.Monoid
import Language.Haskell.Exts

path = "/Users/rob/Developer/Projects/language-haskell-ast/app/Main.hs"

main :: IO ()
main = do
  result <- parseFile path
  case result of
    ParseOk m -> print "parse succeeded"
    ParseFailed loc reason -> print $ "parse failed at " <> show loc <> " because " <> reason

data AST = AST { loc :: SrcLoc, name :: String, children :: AST }

class IsAST t where
  toAST :: t -> AST

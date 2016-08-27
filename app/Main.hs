{-# LANGUAGE DefaultSignatures, DeriveAnyClass, FlexibleContexts, FlexibleInstances, StandaloneDeriving #-}
module Main where

import Data.Monoid
import GHC.Generics
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
  default toAST :: (Generic t, IsAST' (Rep t)) => t -> AST
  toAST = toASTGeneric

class IsAST' t where
  toAST' :: t a -> AST

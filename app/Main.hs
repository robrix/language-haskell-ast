{-# LANGUAGE DefaultSignatures, DeriveAnyClass, FlexibleContexts, FlexibleInstances, StandaloneDeriving #-}
module Main where

import Data.Monoid
import GHC.Generics
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax

path = "/Users/rob/Developer/Projects/language-haskell-ast/app/Main.hs"

main :: IO ()
main = do
  result <- parseFile path
  case result of
    ParseOk m -> print "parse succeeded"
    ParseFailed loc reason -> print $ "parse failed at " <> show loc <> " because " <> reason

data AST = AST { astLoc :: SrcSpanInfo, astName :: String, astChildren :: [AST] }
  deriving (Eq, Show)

instance IsAST (Module SrcSpanInfo) where
  toAST m@(Module l _ _ _ _) = AST l "program" []


class IsAST t where
  toAST :: t -> AST
  default toAST :: (Generic t, IsAST' (Rep t)) => t -> AST
  toAST = toASTGeneric

class IsAST' t where
  toAST' :: t a -> AST

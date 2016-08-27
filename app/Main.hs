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
    ParseOk m -> putStrLn $ "parse succeeded: " <> show (toAST m)
    ParseFailed loc reason -> putStrLn $ "parse failed at " <> show loc <> " because " <> reason

data AST = AST { astLoc :: SrcSpan, astName :: String, astChildren :: [AST] }
  deriving (Eq, Show)

instance IsAST (Module SrcSpanInfo) where
  toAST (Module l header _ _ _) = AST (srcInfoSpan l) "program" []


class IsAST t where
  toAST :: t -> AST
  default toAST :: (Generic t, IsAST' (Rep t)) => t -> AST
  toAST = toASTGeneric

class IsAST' t where
  toAST' :: t a -> AST

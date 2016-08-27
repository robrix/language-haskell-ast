{-# LANGUAGE DefaultSignatures, DeriveAnyClass, FlexibleContexts, FlexibleInstances, StandaloneDeriving #-}
module Main where

import Data.Monoid
import GHC.Generics
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Data.Maybe

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
  toAST (Module l header _ _ _) = AST (srcInfoSpan l) "program" (toAST <$> maybeToList header)

instance IsAST (ModuleHead SrcSpanInfo) where
  toAST (ModuleHead l _ _ _) = AST (srcInfoSpan l) "module_head" []


data SrcRange = SrcRange { srcRangeStartLine :: !Int, srcRangeStartColumn :: !Int, srcRangeEndLine :: !Int, srcRangeEndColumn :: !Int }
  deriving (Eq, Show)

spanToRange :: SrcSpan -> SrcRange
spanToRange (SrcSpan _ sl sc el ec) = SrcRange sl sc el ec


class IsAST t where
  toAST :: t -> AST
  default toAST :: (Generic t, IsAST' (Rep t)) => t -> AST
  toAST = toASTGeneric

class IsAST' t where
  toAST' :: t a -> AST

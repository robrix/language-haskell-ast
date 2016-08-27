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
    ParseOk m -> putStrLn $ "parse succeeded: " <> show (toAST (spanToRange . srcInfoSpan <$> m))
    ParseFailed loc reason -> putStrLn $ "parse failed at " <> show loc <> " because " <> reason

data AST = AST { astRange :: SrcRange, astName :: String, astChildren :: [AST] }
  deriving (Eq, Show)

instance IsAST (Module SrcRange) where
  toAST (Module l header _ _ _) = AST l "program" (toAST <$> maybeToList header)

instance IsAST (ModuleHead SrcRange) where
  toAST (ModuleHead l name _ _) = AST l "module_head" [ toAST name ]

instance IsAST (ModuleName SrcRange) where
  toAST (ModuleName l s) = AST l "identifier" []

data SrcRange = SrcRange { srcRangeStartLine :: !Int, srcRangeStartColumn :: !Int, srcRangeEndLine :: !Int, srcRangeEndColumn :: !Int }
  deriving (Eq)

spanToRange :: SrcSpan -> SrcRange
spanToRange (SrcSpan _ sl sc el ec) = SrcRange sl sc el ec


instance Show SrcRange where
  showsPrec _ (SrcRange sl sc el ec) = showParen True $ shows sl . showString ":" . shows sc . showString "-" . shows el . showString ":" . shows ec

class IsAST t where
  toAST :: t -> AST
  default toAST :: (Generic t, IsAST' (Rep t)) => t -> AST
  toAST = toASTGeneric

class IsAST' t where
  toAST' :: t a -> AST

toASTGeneric :: (Generic t, IsAST' (Rep t)) => t -> AST String
toASTGeneric = toAST' . from

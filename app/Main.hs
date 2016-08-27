{-# LANGUAGE DefaultSignatures, DeriveAnyClass, FlexibleContexts, FlexibleInstances, StandaloneDeriving #-}
module Main where

import Data.Monoid
import GHC.Generics
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Data.List
import Data.Maybe

path = "/Users/rob/Developer/Projects/language-haskell-ast/app/Main.hs"

main :: IO ()
main = do
  result <- parseFile path
  case result of
    ParseOk m -> putStrLn $ "parse succeeded: " <> show (toAST (spanToRange . srcInfoSpan <$> m))
    ParseFailed loc reason -> putStrLn $ "parse failed at " <> show loc <> " because " <> reason

data AST a
  = Leaf { astRange :: SrcRange, astName :: a, astContents :: a }
  | Branch { astRange :: SrcRange, astName :: a, astChildren :: [AST a] }
  deriving (Eq, Show)

instance IsAST Module where
  toAST (Module l header pragmas _ _) = Branch l "program" $ (toAST <$> maybeToList header) <> (toAST <$> pragmas)

instance IsAST ModuleHead where
  toAST (ModuleHead l name warning exportSpecList) = Branch l "module_head" $ toAST name : (toAST <$> maybeToList warning) <> (toAST <$> maybeToList exportSpecList)

instance IsAST WarningText where
  toAST (DeprText l t) = Leaf l "deprecation" t
  toAST (WarnText l t) = Leaf l "warning" t

instance IsAST ExportSpecList where
  toAST (ExportSpecList l specs) = Branch l "export_specs" $ toAST <$> specs

instance IsAST ExportSpec where
  toAST (EVar _ name) = toAST name

instance IsAST QName where
  toAST q = Branch (ann q) "qualified" $ case q of
    Qual _ moduleName name -> [ toAST moduleName, toAST name ]
    UnQual _ name -> pure (toAST name)
    Special _ c -> pure (toAST c)

instance IsAST SpecialCon where
  toAST (UnitCon l) = Leaf l "unit_constructor" "()"
  toAST (ListCon l) = Leaf l "list_type_constructor" "[]"
  toAST (FunCon l) = Leaf l "function_type_constructor" "->"
  toAST (TupleCon l boxed n) = Leaf l "tuple_constructor" $ intercalate "," $ replicate n $ case boxed of
    Boxed -> "#"
    Unboxed -> ""
  toAST (Cons l) = Leaf l "list_data_constructor" ":"
  toAST (UnboxedSingleCon l) = Leaf l "unboxed_singleton_tuple_constructor" "(# #)"

instance IsAST Name where
  toAST (Ident l s) = Leaf l "identifier" s
  toAST (Symbol l s) = Leaf l "symbol" s

instance IsAST ModuleName where
  toAST (ModuleName l s) = Leaf l "identifier" s

instance IsAST ModulePragma where
  toAST (LanguagePragma l names) = Branch l "language_pragma" $ toAST <$> names
  toAST (OptionsPragma l _ s) = Leaf l "options_pragma" s
  toAST (AnnModulePragma l annotation) = Branch l "annotation_pragma" $ pure (toAST annotation)

instance IsAST Annotation where
  toAST (Ann l name e) = Branch l "expression_annotation" [ toAST name{-, toAST e -} ]
  toAST (TypeAnn l name e) = Branch l "type_annotation" [ toAST name{-, toAST e -} ]
  toAST (ModuleAnn l e) = Branch l "module_annotation" [ {- toAST e -} ]

data SrcRange = SrcRange { srcRangeStartLine :: !Int, srcRangeStartColumn :: !Int, srcRangeEndLine :: !Int, srcRangeEndColumn :: !Int }
  deriving (Eq)

spanToRange :: SrcSpan -> SrcRange
spanToRange (SrcSpan _ sl sc el ec) = SrcRange sl sc el ec


instance Show SrcRange where
  showsPrec _ (SrcRange sl sc el ec) = showParen True $ shows sl . showString ":" . shows sc . showString "-" . shows el . showString ":" . shows ec

class IsAST t where
  toAST :: t SrcRange -> AST String
  default toAST :: (Generic1 t, IsAST' (Rep1 t)) => t SrcRange -> AST String
  toAST = toASTGeneric

class IsAST' t where
  toAST' :: t a -> AST String

toASTGeneric :: (Generic1 t, IsAST' (Rep1 t)) => t SrcRange -> AST String
toASTGeneric = toAST' . from1

instance IsAST' f => IsAST' (M1 i c f) where
  toAST' = toAST' . unM1

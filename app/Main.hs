{-# LANGUAGE DefaultSignatures, DeriveAnyClass, FlexibleContexts, FlexibleInstances, StandaloneDeriving, TypeOperators #-}
module Main where

import Data.List
import Data.Maybe
import Data.Monoid
import GHC.Generics
import Language.Haskell.Exts hiding (Pretty)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Text.PrettyPrint.HughesPJClass hiding ((<>))

path = "/Users/rob/Developer/Projects/language-haskell-ast/app/Main.hs"

main :: IO ()
main = do
  result <- parseFile path
  case result of
    ParseOk m -> putStrLn $ "parse succeeded:\n" <> render (pPrint (toAST (spanToRange . srcInfoSpan <$> m)))
    ParseFailed loc reason -> putStrLn $ "parse failed at " <> show loc <> " because " <> reason

data AST a
  = Leaf { astRange :: SrcRange, astName :: a, astContents :: a }
  | Branch { astRange :: SrcRange, astName :: a, astChildren :: [AST a] }
  deriving (Eq, Show)

instance IsAST Module where
  toAST (Module l header pragmas imports _) = Branch l "program" $ (toAST <$> maybeToList header) <> (toAST <$> pragmas) <> (toAST <$> imports)

instance IsAST ModuleHead where
  toAST (ModuleHead l name warning exportSpecList) = Branch l "module_head" $ toAST name : (toAST <$> maybeToList warning) <> (toAST <$> maybeToList exportSpecList)

instance IsAST WarningText where
  toAST (DeprText l t) = Leaf l "deprecation" t
  toAST (WarnText l t) = Leaf l "warning" t

instance IsAST ExportSpecList where
  toAST (ExportSpecList l specs) = Branch l "export_specs" $ toAST <$> specs

instance IsAST ExportSpec where
  toAST (EVar _ name) = toAST name
  toAST (EAbs _ _ name) = toAST name
  toAST (EThingWith l _ name names) = Branch l "export_spec" $ toAST name : (toAST <$> names)
  toAST (EModuleContents _ name) = toAST name

instance IsAST QName where
  toAST q = Branch (ann q) "qualified" $ case q of
    Qual _ moduleName name -> [ toAST moduleName, toAST name ]
    UnQual _ name -> pure (toAST name)
    Special _ c -> pure (toAST c)

instance IsAST CName where
  toAST (VarName _ name) = toAST name
  toAST (ConName _ name) = toAST name

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

instance IsAST ImportDecl where
  toAST (ImportDecl l m _ _ _ _ as importSpecList) = Branch l "import_declaration" $ toAST m : (toAST <$> maybeToList as) <> (toAST <$> maybeToList importSpecList)

instance IsAST ImportSpecList where
  toAST (ImportSpecList l _ specs) = Branch l "import_specs" $ toAST <$> specs

instance IsAST ImportSpec where
  toAST (IVar _ name) = toAST name
  toAST (IAbs _ _ name) = toAST name
  toAST (IThingAll _ name) = toAST name
  toAST (IThingWith l name names) = Branch l "import_spec" $ toAST name : (toAST <$> names)

data SrcRange = SrcRange { srcRangeStartLine :: !Int, srcRangeStartColumn :: !Int, srcRangeEndLine :: !Int, srcRangeEndColumn :: !Int }
  deriving (Eq)

spanToRange :: SrcSpan -> SrcRange
spanToRange (SrcSpan _ sl sc el ec) = SrcRange sl sc el ec


instance Show SrcRange where
  showsPrec _ (SrcRange sl sc el ec) = showParen True $ shows sl . showString ":" . shows sc . showString "-" . shows el . showString ":" . shows ec

instance Pretty SrcRange where
  pPrint (SrcRange sl sc el ec) = text $ "[" <> show sl <> ":" <> show sc <> "-" <> show el <> ":" <> show ec <> "]"

instance Pretty (AST String) where
  pPrint (Leaf l name contents) = parens $ text name <+> pPrint l <+> text (show contents)
  pPrint (Branch l name children) = parens $ text name <+> pPrint l <+> sep (pPrint <$> children)

class IsAST t where
  toAST :: t SrcRange -> AST String
  default toAST :: (Generic (t SrcRange), IsAST' (Rep (t SrcRange))) => t SrcRange -> AST String
  toAST = toASTGeneric

class IsLocated t where
  location :: t SrcRange -> SrcRange

class IsLeaf t where
  leaf :: t a -> String

class IsAST' t where
  toAST' :: t SrcRange -> AST String

class IsAST'' t where
  toAST'' :: t SrcRange -> [AST String]

toASTGeneric :: (Generic (t SrcRange), IsAST' (Rep (t SrcRange))) => t SrcRange -> AST String
toASTGeneric = toAST' . from

instance (IsLocated f, IsAST' f, Datatype c) => IsAST' (M1 D c f) where
  toAST' m = toAST' (unM1 m)

instance (IsLocated f, IsAST'' f, Constructor c) => IsAST' (M1 C c f) where
  toAST' m = Branch (location m) (conName m) (toAST'' (unM1 m))

instance IsAST'' (M1 S c (K1 R v)) where
  toAST'' m = []

instance (IsAST'' f, IsAST'' g) => IsAST'' (f :*: g) where
  toAST'' (f :*: g) = toAST'' f <> toAST'' g

instance IsLocated f => IsLocated (f :*: g) where
  location (l :*: _) = location l

instance (IsLocated f, IsLocated g) => IsLocated (f :+: g) where
  location (L1 l) = location l
  location (R1 r) = location r

instance IsLocated f => IsLocated (M1 i c f) where
  location = location . unM1

instance IsLocated (K1 R SrcRange) where
  location = unK1

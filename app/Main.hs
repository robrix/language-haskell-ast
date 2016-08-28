{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, TypeOperators #-}
module Main where

import Data.List
import Data.Maybe
import Data.Monoid
import GHC.Generics
import Language.Haskell.Exts hiding (Pretty)
import Text.PrettyPrint.HughesPJClass hiding ((<>))

path :: String
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

instance IsAST WarningText
instance IsAST ExportSpecList
instance IsAST Namespace

instance IsAST ExportSpec where
  toAST (EVar _ name) = toAST name
  toAST (EAbs _ _ name) = toAST name
  toAST (EThingWith l _ name names) = Branch l "export_spec" $ toAST name : (toAST <$> names)
  toAST (EModuleContents _ name) = toAST name

instance IsAST QName
instance IsAST CName

instance IsAST SpecialCon where
  toAST (UnitCon l) = Leaf l "unit_constructor" "()"
  toAST (ListCon l) = Leaf l "list_type_constructor" "[]"
  toAST (FunCon l) = Leaf l "function_type_constructor" "->"
  toAST (TupleCon l boxed n) = Leaf l "tuple_constructor" $ intercalate "," $ replicate n $ case boxed of
    Boxed -> "#"
    Unboxed -> ""
  toAST (Cons l) = Leaf l "list_data_constructor" ":"
  toAST (UnboxedSingleCon l) = Leaf l "unboxed_singleton_tuple_constructor" "(# #)"

instance IsAST Name
instance IsAST ModuleName
instance IsAST ModulePragma

instance IsAST Annotation where
  toAST (Ann l name e) = Branch l "expression_annotation" [ toAST name{-, toAST e -} ]
  toAST (TypeAnn l name e) = Branch l "type_annotation" [ toAST name{-, toAST e -} ]
  toAST (ModuleAnn l e) = Branch l "module_annotation" [ {- toAST e -} ]

instance IsAST ImportDecl where
  toAST (ImportDecl l m _ _ _ _ as importSpecList) = Branch l "import_declaration" $ toAST m : (toAST <$> maybeToList as) <> (toAST <$> maybeToList importSpecList)

instance IsAST ImportSpecList
instance IsAST ImportSpec

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
  default toAST :: (Generic (t SrcRange), IsAST (Rep (t SrcRange))) => t SrcRange -> AST String
  toAST = toASTGeneric

class IsLocated t where
  location :: t SrcRange -> SrcRange

class IsAST' t where
  toAST' :: t SrcRange -> [AST String]

toASTGeneric :: (Generic t, IsAST (Rep t)) => t -> AST String
toASTGeneric = toAST . from

instance (IsLocated f, IsAST f, Datatype c) => IsAST (M1 D c f) where
  toAST = toAST . unM1

instance Constructor c => IsAST (C1 c (S1 s (Rec0 SrcRange) :*: (S1 t (Rec0 String)))) where
  toAST m = Leaf (location m) (conName m) $ unK1 . unM1 . r . unM1 $ m
    where r (_ :*: r) = r

instance Constructor c => IsAST (C1 c (S1 s (Rec0 SrcRange))) where
  toAST m = Branch (location m) (conName m) []

instance (IsLocated l, Constructor c, IsAST v) => IsAST (C1 c (l :*: (S1 t (Rec0 (v SrcRange))))) where
  toAST m = Branch (location m) (conName m) $ pure . toAST . unK1 . unM1 . r . unM1 $ m
    where r (_ :*: r) = r

instance (IsLocated l, Constructor c, IsAST v) => IsAST (C1 c (l :*: (S1 t (Rec0 [v SrcRange])))) where
  toAST m = Branch (location m) (conName m) $ fmap toAST . unK1 . unM1 . r . unM1 $ m
    where r (_ :*: r) = r

instance (IsLocated l, IsAST' g, IsAST' h, Constructor c) => IsAST (C1 c (l :*: (g :*: h))) where
  toAST m = case toAST' (r (unM1 m)) of
    [ a ] | astRange a == location m -> a
    as -> Branch (location m) (conName m) as
    where r (_ :*: r) = r

instance (IsAST v, Selector s) => IsAST' (S1 s (Rec0 (v SrcRange))) where
  toAST' = pure . toAST . unK1 . unM1

instance (IsAST v, Selector s) => IsAST' (S1 s (Rec0 [v SrcRange])) where
  toAST' = fmap toAST . unK1 . unM1

instance (IsAST v, Selector s) => IsAST' (S1 s (Rec0 (Maybe (v SrcRange)))) where
  toAST' = fmap toAST . maybeToList . unK1 . unM1

instance Selector s => IsAST' (S1 s (Rec0 String)) where
  toAST' _ = []

instance Selector s => IsAST' (S1 s (Rec0 Bool)) where
  toAST' _ = []

instance Selector s => IsAST' (S1 s (Rec0 (Maybe Tool))) where
  toAST' _ = []

instance Selector s => IsAST' (S1 s (Rec0 (Maybe String))) where
  toAST' _ = []

instance (IsAST' f, IsAST' g) => IsAST' (f :*: g) where
  toAST' (f :*: g) = toAST' f <> toAST' g

instance (IsAST f, IsAST g) => IsAST (f :+: g) where
  toAST (L1 l) = toAST l
  toAST (R1 r) = toAST r

instance IsLocated f => IsLocated (f :*: g) where
  location (l :*: _) = location l

instance (IsLocated f, IsLocated g) => IsLocated (f :+: g) where
  location (L1 l) = location l
  location (R1 r) = location r

instance IsLocated f => IsLocated (M1 i c f) where
  location = location . unM1

instance IsLocated (K1 R SrcRange) where
  location = unK1

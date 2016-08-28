{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, RecordWildCards, TypeOperators #-}
module Main where

import Control.Monad
import Data.Foldable
import Data.Monoid hiding (Alt)
import GHC.Generics
import Language.Haskell.Exts hiding (Pretty)
import Options.Applicative
import Text.PrettyPrint.HughesPJClass hiding ((<>))

arguments :: Parser (Bool, FilePath)
arguments = helper <*> ((,) <$> switch (long "verbose" <> short 'v' <> help "show verbose output, including source ranges") <*> strArgument (metavar "FILE"))

main :: IO ()
main = do
  (verbose, path) <- execParser (info arguments (fullDesc <> progDesc "Haskell sources to AST s-exprs"))
  result <- parseFile path
  case result of
    ParseOk m -> putStrLn $ render (pPrintPrec (if verbose then PrettyLevel 1 else prettyNormal) 0 (toAST (spanToRange . srcInfoSpan <$> m)))
    ParseFailed loc reason -> putStrLn $ "parse failed at " <> show loc <> " because " <> reason


-- Datatypes

data AST a
  = Leaf { astRange :: SrcRange, astName :: a, astContents :: a }
  | Branch { astRange :: SrcRange, astName :: a, astChildren :: [AST a] }
  deriving (Eq, Show)

data SrcRange = SrcRange { srcRangeStartLine :: !Int, srcRangeStartColumn :: !Int, srcRangeEndLine :: !Int, srcRangeEndColumn :: !Int }
  deriving (Eq)

spanToRange :: SrcSpan -> SrcRange
spanToRange (SrcSpan _ sl sc el ec) = SrcRange sl sc el ec

l :: (l :*: r) a -> l a
l (l :*: _) = l

r :: (l :*: r) a -> r a
r (_ :*: r) = r

inlr :: (l a -> b) -> (r a -> b) -> (l :+: r) a -> b
inlr f g s = case s of { L1 l -> f l ; R1 r -> g r }


-- Typeclasses

class IsAST t where
  toAST :: t SrcRange -> AST String
  default toAST :: (Generic (t SrcRange), IsAST (Rep (t SrcRange))) => t SrcRange -> AST String
  toAST = toASTGeneric

class IsLocated t where
  location :: t SrcRange -> SrcRange

class IsAST' t where
  toAST' :: t SrcRange -> [AST String]
  toAST' _ = []


-- Instances

instance IsAST Module
instance IsAST ModuleHead
instance IsAST WarningText
instance IsAST ExportSpecList
instance IsAST Namespace
instance IsAST ExportSpec
instance IsAST EWildcard
instance IsAST QName
instance IsAST CName
instance IsAST SpecialCon
instance IsAST Name
instance IsAST ModuleName
instance IsAST ModulePragma
instance IsAST TyVarBind
instance IsAST Kind
instance IsAST Unpackedness
instance IsAST BangType
instance IsAST IPName
instance IsAST DeclHead
instance IsAST XName
instance IsAST CallConv
instance IsAST BooleanFormula
instance IsAST InjectivityInfo
instance IsAST ResultSig
instance IsAST Op
instance IsAST DataOrNew
instance IsAST Safety
instance IsAST Overlap
instance IsAST FunDep
instance IsAST Activation
instance IsAST Assoc
instance IsAST QOp
instance IsAST Literal
instance IsAST Sign
instance IsAST Role
instance IsAST ConDecl
instance IsAST Decl
instance IsAST PatternSynDirection
instance IsAST RPat
instance IsAST RPatOp
instance IsAST FieldDecl
instance IsAST PatField
instance IsAST PXAttr
instance IsAST Type
instance IsAST Splice
instance IsAST Promoted
instance IsAST Context
instance IsAST Asst
instance IsAST Rule
instance IsAST RuleVar
instance IsAST Pat
instance IsAST Deriving
instance IsAST InstDecl
instance IsAST InstRule
instance IsAST InstHead
instance IsAST QualConDecl
instance IsAST TypeEqn
instance IsAST GadtDecl
instance IsAST Rhs
instance IsAST GuardedRhs
instance IsAST Stmt
instance IsAST ClassDecl
instance IsAST Binds
instance IsAST IPBind
instance IsAST Alt
instance IsAST QualStmt
instance IsAST Bracket
instance IsAST FieldUpdate
instance IsAST XAttr
instance IsAST Exp
instance IsAST Match
instance IsAST Annotation
instance IsAST ImportDecl
instance IsAST ImportSpecList
instance IsAST ImportSpec


instance Show SrcRange where
  showsPrec _ (SrcRange sl sc el ec) = showParen True $ shows sl . showString ":" . shows sc . showString "-" . shows el . showString ":" . shows ec

instance Pretty SrcRange where
  pPrint (SrcRange sl sc el ec) = text $ "[" <> show sl <> ":" <> show sc <> "-" <> show el <> ":" <> show ec <> "]"

instance Pretty (AST String) where
  pPrintPrec level n ast = parens $ text (astName ast) <+> (if level > prettyNormal then pPrintPrec level n (astRange ast) else mempty) <+> case ast of
    Leaf{..} -> text (show astContents)
    Branch{..} -> sep (pPrintPrec level n <$> astChildren)

toASTGeneric :: (Generic t, IsAST (Rep t)) => t -> AST String
toASTGeneric = toAST . from

instance (IsLocated f, IsAST f, Datatype c) => IsAST (M1 D c f) where
  toAST = toAST . unM1

instance Constructor c => IsAST (C1 c (S1 s (Rec0 SrcRange) :*: (S1 t (Rec0 String)))) where
  toAST m = Leaf (location m) (conName m) $ unK1 . unM1 . r . unM1 $ m

instance Constructor c => IsAST (C1 c (S1 s (Rec0 SrcRange) :*: (S1 t (Rec0 Int)))) where
  toAST m = Branch (location m) (conName m) []

instance Constructor c => IsAST (C1 c (S1 s (Rec0 SrcRange) :*: (S1 t (Rec0 Bool)))) where
  toAST m = Branch (location m) (conName m) []

instance Constructor c => IsAST (C1 c (S1 s (Rec0 SrcRange))) where
  toAST m = Branch (location m) (conName m) []

instance Constructor c => IsAST (C1 c U1) where
  toAST m = Branch (location m) (conName m) []

instance (IsLocated l, Constructor c, IsAST v) => IsAST (C1 c (l :*: (S1 t (Rec0 (v SrcRange))))) where
  toAST m = Branch (location m) (conName m) $ pure . toAST . unK1 . unM1 . r . unM1 $ m

instance (IsLocated l, Constructor c, IsAST v, Functor t, Foldable t, Functor u, Foldable u) => IsAST (C1 c (l :*: (S1 s (Rec0 (t (u (v SrcRange), a)))))) where
  toAST m = Branch (location m) (conName m) $ join . toList . fmap (toList . fmap toAST . fst) . unK1 . unM1 . r . unM1 $ m

instance (IsLocated l, Constructor c, IsAST v, Functor t, Foldable t) => IsAST (C1 c (l :*: (S1 s (Rec0 (t (v SrcRange)))))) where
  toAST m = Branch (location m) (conName m) $ toList . fmap toAST . unK1 . unM1 . r . unM1 $ m

instance (IsLocated l, IsAST' l, IsAST' g, IsAST' h, Constructor c) => IsAST (C1 c (l :*: (g :*: h))) where
  toAST m = case toAST' (unM1 m) of
    [ a ] | astRange a == location m -> a
    as -> Branch (location m) (conName m) as

instance (IsAST v, Selector s) => IsAST' (S1 s (Rec0 (v SrcRange))) where
  toAST' = pure . toAST . unK1 . unM1

instance (IsAST v, Selector s, Functor t, Foldable t) => IsAST' (S1 s (Rec0 (t (v SrcRange)))) where
  toAST' = toList . fmap toAST . unK1 . unM1

instance (IsAST v, Selector s, Functor t, Foldable t, Functor u, Foldable u) => IsAST' (S1 s (Rec0 (t (u (v SrcRange), a)))) where
  toAST' = join . toList . fmap (toList . fmap toAST . fst) . unK1 . unM1

instance (IsAST v, Selector s, Functor t, Foldable t, Functor u, Foldable u) => IsAST' (S1 s (Rec0 (t (u (v SrcRange))))) where
  toAST' = join . toList . fmap (toList . fmap toAST) . unK1 . unM1

instance Selector s => IsAST' (S1 s (Rec0 SrcRange))
instance Selector s => IsAST' (S1 s (Rec0 String))
instance Selector s => IsAST' (S1 s (Rec0 Char))
instance Selector s => IsAST' (S1 s (Rec0 Rational))
instance Selector s => IsAST' (S1 s (Rec0 Bool))
instance Selector s => IsAST' (S1 s (Rec0 Int))
instance Selector s => IsAST' (S1 s (Rec0 (a, b)))
instance Selector s => IsAST' (S1 s (Rec0 Integer))
instance Selector s => IsAST' (S1 s (Rec0 Boxed))
instance Selector s => IsAST' (S1 s (Rec0 (Maybe Int)))
instance Selector s => IsAST' (S1 s (Rec0 (Maybe Tool)))
instance Selector s => IsAST' (S1 s (Rec0 (Maybe String)))

instance (IsAST' f, IsAST' g) => IsAST' (f :*: g) where
  toAST' (f :*: g) = toAST' f <> toAST' g

instance (IsAST f, IsAST g) => IsAST (f :+: g) where
  toAST = inlr toAST toAST

instance IsLocated f => IsLocated (f :*: g) where
  location = location . l

instance (IsLocated f, IsLocated g) => IsLocated (f :+: g) where
  location = inlr location location

instance IsLocated f => IsLocated (M1 i c f) where
  location = location . unM1

instance IsLocated (K1 R SrcRange) where
  location = unK1

instance IsLocated U1 where
  location _ = SrcRange 0 0 0 0

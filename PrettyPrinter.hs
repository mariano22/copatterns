module PrettyPrinter
( printTerm,            -- Pretty printer for Term.
  printCopattern,       -- Pretty printer for Copattern.
  printPattern,         -- Pretty printer for Pattern.
  printRule,            -- Pretty printer for Rule.
  printType,            -- Pretty printer for Type.
  printSymbolInfo,      -- Given a Program and Symbol prints it's definition/declaration if is on the Program.
  printFullProgramInfo, -- Pretty print type definitions, signature and rules in a Program.
  PPrintable(..)
) where
import Common
import Utils
import qualified Data.Map as M
import Data.List
import Data.Char
import Data.Maybe
import Text.PrettyPrint
import Control.Applicative

-- Class of types pretty printables.
class PPrintable a where
  pprint :: a -> Doc

instance PPrintable Term where
  pprint = printTerm

instance PPrintable Copattern where
  pprint = printCopattern

instance PPrintable Pattern where
  pprint = printPattern

instance PPrintable Rule where
  pprint = printRule

instance PPrintable Type where
  pprint = printType

-- Adds parens to a Doc if condition holds.
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- Pretty printer for Term.
printTerm :: Term -> Doc
printTerm (Var s) = text s
printTerm Unit = text "()"
printTerm (Tuple t1 t2) = parens (printTerm t1 <> comma <+> printTerm t2)
printTerm tt@(Constructor c t) = maybe (text c <+> printAsAtom t) int (termToNat tt)
printTerm (App t1 t2) = printTerm t1 <+> printAsAtom t2
printTerm (Destructor d t) = text d <+> printAsAtom t

{- If a term is a value of type Nat = Data(X)< Zero () | Succ X > returns as
   decimal integer representation. -}
termToNat :: Term -> Maybe Int
termToNat (Constructor "Zero" Unit) = return 0
termToNat (Constructor "Succ" t) = liftA (+1) (termToNat t)
termToNat _ = Nothing

{- Print a Term as an atomic term. Adds parens if it's an (term, constructor or
  destructor) application -}
printAsAtom :: Term -> Doc
printAsAtom t = parensIf (not (isAtomTerm t)) (printTerm t)

-- Returns True iff Term is not an application (it's atomic term).
isAtomTerm :: Term -> Bool
isAtomTerm (Var _) = True
isAtomTerm  Unit = True
isAtomTerm (Tuple _ _) = True
isAtomTerm tt@(Constructor _ _) = isJust (termToNat tt)
isAtomTerm (App _ _) = False
isAtomTerm (Destructor _ _) = False

-- Pretty printer for Copattern.
printCopattern :: Copattern -> Doc
printCopattern = printTerm . copatternToTerm
-- Pretty printer for Pattern.
printPattern :: Pattern -> Doc
printPattern = printTerm . patternToTerm

-- Convert Copattern to analogous Term.
copatternToTerm :: Copattern -> Term
copatternToTerm (CHole s) = Var s
copatternToTerm (CApp q p) = App (copatternToTerm q) (patternToTerm p)
copatternToTerm (CDestructor d q) = Destructor d (copatternToTerm q)

-- Convert Pattern to analogous Term.
patternToTerm :: Pattern -> Term
patternToTerm (PVar s) = Var s
patternToTerm PUnit = Unit
patternToTerm (PTuple p1 p2) = Tuple (patternToTerm p1) (patternToTerm p2)
patternToTerm (PConstructor c p) = Constructor c (patternToTerm p)

-- Type variables for using to print ligated as type symbols.
type_vars :: [String]
type_vars = [ c : n | n <- "" : map show [(1::Integer)..], c <- ['X','Y','Z'] ++['A'..'W'] ]

-- Pretty printer for Type.
printType :: Type -> Doc
printType t = printType' 0 t
  where ligadure_vars = filter (\v -> not $ elem v (fvType t)) type_vars
        printType' deep = pp
          where pp (TyVar s) = text s
                pp (TyBound k) = text (ligadure_vars !! (deep - k - 1))
                pp  TyUnit = text "()"
                pp (TyTuple t1 t2) = parens (pp t1 <> comma <+> pp t2)
                pp (TyFun t1 t2) = parensIf (isFunTy t1) (pp t1) <+> text "->" <+> pp t2
                pp (TyData sig) = text "Data" <> parens (text (ligadure_vars !! deep)) <> text "<" <> sigPP sig <> text ">"
                pp (TyRecord sig) = text "Record" <> parens (text (ligadure_vars !! deep)) <> text "{" <> sigPP sig <> text "}"
                sigPP :: (M.Map Symbol Type) -> Doc
                sigPP = hsep . intersperse (text "|") . map (\(s,t) -> text s <+> colon <+> printType' (deep+1) t) . M.toList

-- Pretty printer for Rule.
printRule :: Rule -> Doc
printRule r = let q = getCopattern r
                  t = getTerm r
              in printCopattern q <+> text "=" <+> printTerm t

{- Given a program and a Symbol prints:
      - A type definition if it's symbol type defined in synonym definition.
      - The signature/rules if it's a defined variable symbol. -}
printSymbolInfo :: Program -> Symbol -> Maybe Doc
printSymbolInfo (typeSig, symbolDefs) symbol =
  if isUpper (head symbol)
    then printTypeSymbolDef typeSig symbol
    else do signatureDoc <- printSymbolSignature symbolDefs symbol
            rulesDoc     <- printSymbolDefs symbolDefs symbol
            return (signatureDoc $+$ rulesDoc)

-- Print type definition of a symbol type defined in synonym definition.
printTypeSymbolDef :: TypeSignature -> Symbol -> Maybe Doc
printTypeSymbolDef (TypeSig m) s = do ty <- M.lookup s m
                                      return $ text s <+> text "=" <+> (printType . getDef) ty

-- Print signature of a defined variable symbol.
printSymbolSignature :: SymbolDefs -> Symbol -> Maybe Doc
printSymbolSignature m s = do (ty, _) <- M.lookup s m
                              return $ text s <+> colon <+> (printType . getDef) ty

-- Print the definition rules of a defined variable symbol.
printSymbolDefs :: SymbolDefs -> Symbol -> Maybe Doc
printSymbolDefs m s = vcat . map (printRule . getDef) . snd <$> M.lookup s m

{- Print all the type definitions and signatures and rule definitions of all
   defined symbols in a Program.
-}
printFullProgramInfo :: Program -> Doc
printFullProgramInfo p = vcat $ map (fromJust . printSymbolInfo p) (sort allSymbols)
  where allSymbols :: [Symbol]
        allSymbols = let (TypeSig m1, m2) = p in M.keys m1 ++ M.keys m2

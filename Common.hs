module Common where

import qualified Data.Map as M

-- Type synonyms for identifiers.
type Symbol = String

-- The type of the types.
data Type = TyVar Symbol
          | TyBound Int
          | TyUnit
          | TyTuple Type Type
          | TyData (M.Map Symbol Type)
          | TyFun Type Type
          | TyRecord (M.Map Symbol Type)
          deriving (Show,Eq)

-- Type of the terms.
data Term = Var Symbol
          | Unit
          | Tuple Term Term
          | Constructor Symbol Term
          | App Term Term
          | Destructor Symbol Term
          deriving (Show)

-- A term with a type declaration.
data TypedTerm = TypedTerm Term Type

-- A type definition. Example: UnitFun = () -> ()
data TypeDef = TypeDef { getTypeVar :: Symbol,
                         getType :: Type } deriving (Show)

-- A signature definition. Example: myFunction : UnitFun
data SignatureDef = SignatureDef { getSymbol :: Symbol,
                                   getSymbolType :: Type } deriving (Show)

{- A term definition. Example: .head( fiboSeq ) = 0
   The happy parser parses any 'Term = Term' as rule definition.
   Then we check l.h.s. is a valid copattern and transform to a (Copattern, Term).
-}
data TermDef = TermDef { lhs :: Term,
                         rhs :: Term } deriving (Show)

-- A datatype to keep values defined in certain line of code.
data Def a = Def { getDef :: a, getDefLineNo :: Int } deriving (Show,Eq)

instance Functor Def where
  fmap f d = d{ getDef = (f . getDef) d }

{- The happy parser output.
   A program code file it's parsed as a list of type synonyms definitions,
   a list of signature definitions and a list of rule definitions.
-}
data ProgramDefs = ProgramDefs { typeDefs :: [Def TypeDef],
                                 signatureDefs :: [Def SignatureDef],
                                 termDefs :: [Def TermDef] } deriving (Show)

-- Contexts that maps symbols to type definitions.
{- VarSignature maps a defined symbol to its type.
   If we have "myFunction : UnitFun" it will map "myFunction" -> Def UnitFun -}
newtype VarSignature = VarSig { getVarSig :: M.Map Symbol (Def Type) } deriving (Show)

{- TypeSignature maps a type variable symbole to its synonym definition.
   If we have "A = () -> ()" it will map "A" -> Def () -> () -}
newtype TypeSignature = TypeSig { getTypeSig :: M.Map Symbol (Def Type) } deriving (Show)

{- AST of the Copattern which are the l.h.s. of our definitions.
   Respective defined identifier symbol was added to Copattern AST to make
   implementation easier. -}
data Copattern = CHole Symbol
               | CApp Copattern Pattern
               | CDestructor Symbol Copattern
               deriving (Show, Eq)

-- AST of the patterns.
data Pattern  = PVar Symbol
              | PUnit
              | PTuple Pattern Pattern
              | PConstructor Symbol Pattern
              deriving (Show, Eq)

-- Type of the Substitution of symbols by terms generated by copattern matching.
type Substitution = [(Symbol,Term)]

-- A term definition will be translated to a Rule where l.h.s. is a Copattern.
data Rule = Rule { getCopattern :: Copattern,
                   getTerm      :: Term  } deriving (Show)

-- A maps from identifier to declaration and definition.
type SymbolDefs = M.Map Symbol (Def Type, [Def Rule])

-- A program is the set of identifier declaration/definition with the type definitions
type Program = (TypeSignature, SymbolDefs)

-- A program with no definitions at all.
emptyProgram :: Program
emptyProgram = (TypeSig M.empty, M.empty)

{- The type of the context needed to do the type checking: the respective types of
   defined symbols and variables and the type synonym definitions. -}
type TypeContext = (VarSignature, TypeSignature)

{- Los contextos de evaluación son un subconjunto sintáctico de los términos que
   son candidatos a matchear con un copatrón (ya que tienen "forma de copatrón").
   El parámetro x nos permite definir contextos de evaluación con términos
   como argumentos ó contextos con sólo valores. -}
data EContext x = EHole Symbol
                | EApp (EContext x) x
                | EDestructor Symbol (EContext x)
                deriving Show

instance Functor EContext where
  fmap _ (EHole f) =  EHole f
  fmap f (EApp e x) = EApp (fmap f e) (f x)
  fmap f (EDestructor d e) =  EDestructor d (fmap f e)

{- Representar un EContext x como [ContextE x] nos da una mirada "desde adentro"
   de la estructura de los entornos de evaluación.
   También usamos este constructor de tipos para obtener una representación de
   Copattern "desde adentro" con ContextE Pattern. -}
data ContextE x = EEHole Symbol | EEApp x | EEDestructor Symbol deriving Show

-- El tipo de los valores.
data Value = PVUnit
           | PVConstructor Symbol Value
           | PVPair Value Value
           | NegV (EContext Term)
           deriving Show

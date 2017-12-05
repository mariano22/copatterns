module Common where
  import Data.List

  -- Comandos interactivos o de archivos
  data Stmt i = Let String i           --  Declarar un nuevo identificador x, let x = t
              | Eval i                 --  Evaluar el término
    deriving (Show)
  
  -- Tipos de los nombres
  data Name
     =  Global  String
     |  Quote   Int
    deriving (Show, Eq)

  -- Entornos
  type NameEnv a = [(Symbol,a)]

  type Def = (Symbol, (Type, [Rule]))
  type Program = [Def]
  type Rule = (Copattern,Term)
  type Symbol = String
  type TypeVar = String
  -- Tipos de los tipos
  data Type = TyVar Symbol
            | TyUnit
            | TyTuple Type Type
            | TyData TypeVar LabeledTypes
            | TyFun Type Type
            | TyRecord TypeVar LabeledTypes
            deriving (Show, Eq)

  type Label = String
  type LabeledTypes = [ (Label, Type) ]

  data Pattern  = PVar String
                | PUnit
                | PTuple Pattern Pattern
                | PConstructor Label Pattern
             deriving (Show,Eq)
  data Copattern  = Hole
                  | CApp Copattern Pattern
                  | CDestructor Label Copattern
             deriving (Show,Eq)

  data Term = Var Symbol
            | Unit
            | Tuple Term Term
            | Constructor Label Term
            | App Term Term
            | Destructor Label Term
             deriving (Show,Eq)











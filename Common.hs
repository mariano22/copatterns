module Common where
  import Data.List
  import qualified Data.Map as M
  import Control.Applicative
  import Data.Traversable
  import Control.Monad.Trans.Maybe
  import Data.Maybe

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

  data Def = RuleDef Symbol VType [Rule]
           | TypeDef Symbol VType


  type Program = NameEnv (Type,[Rule])
  type Rule = (Copattern,Term)
  type Symbol = String
  type TypeVar = String
  -- Tipos de los tipos
  data VType = VTyVar Symbol
             | VTyUnit
             | VTyTuple VType VType
             | VTyData TypeVar LabeledVTypes
             | VTyFun VType VType
             | VTyRecord TypeVar LabeledVTypes
             deriving (Show, Eq)

  data Type  = Bound Int
             | TyUnit
             | TyTuple Type Type
             | TyData LabeledTypes
             | TyFun Type Type
             | TyRecord LabeledTypes
             deriving (Show, Eq)

  type Label = String
  type LabeledVTypes = M.Map Label VType
  type LabeledTypes = M.Map Label Type

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


  typeFold :: Program -> NameEnv Type -> [Def] -> Maybe ((NameEnv Type),Program)
  typeFold oldProgram oldTypes [] = return (oldTypes,oldProgram)
  typeFold oldProgram oldTypes ((TypeDef tName tDef):rest) = do tDef' <- typeSubstitution2 oldTypes tDef
                                                                typeFold oldProgram ((tName,tDef'):oldTypes) rest
  typeFold oldProgram oldTypes ((RuleDef fName fVtype fDef):rest) = do ftype <- typeSubstitution2 oldTypes fVtype
                                                                       if isJust (lookup fName oldProgram)
                                                                        then fail $ "Redefinicion de: " ++ fName
                                                                        else typeFold ((fName,(ftype,fDef)):oldProgram) oldTypes rest

  typeSubstitution2 :: NameEnv Type -> VType -> Maybe Type
  typeSubstitution2 env = f []
                   where f benv t@(VTyVar y) =  fmap Bound (elemIndex y benv) <|> (lookup y env)
                         f benv VTyUnit = return TyUnit
                         f benv (VTyTuple t1 t2) = do t1' <- (f benv t1) 
                                                      t2' <- (f benv t2)
                                                      return (TyTuple t1' t2')
                         f benv (VTyData y dDef) = do rDef' <- mapM (f (y:benv)) dDef
                                                      return (TyData rDef')
                         f benv (VTyFun t1 t2) = do t1' <- (f benv t1) 
                                                    t2' <- (f benv t2)
                                                    return (TyFun t1' t2')
                         f benv (VTyRecord y rDef) = do rDef' <- mapM (f (y:benv)) rDef
                                                        return (TyRecord rDef')
  
  
  
  





module Common where
  import Data.List
  import qualified Data.Map as M
  import Control.Applicative
  import Data.Traversable
  import Control.Monad.Trans.Maybe
  import Control.Monad
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



  data PDef tRule = RuleDef Symbol VType [tRule]
                  | TypeDef Symbol VType

  type Def = PDef Rule
  type Def' = PDef (Term,Term)


  type Program = NameEnv (Type,[Rule])
  type Rule = (Copattern,Term)
  type Symbol = String
  type TypeVar = String
  -- Tipos de los tipos
  data VType = VTyVar Symbol
             | VTyUnit
             | VTyTuple VType VType
             | VTyData Symbol (M.Map Label VType)
             | VTyFun VType VType
             | VTyRecord Symbol (M.Map Label VType)
             deriving (Show, Eq)

  data Type = TyVar Int
             | TyUnit
             | TyTuple Type Type
             | TyData (M.Map Label Type)
             | TyFun Type Type
             | TyRecord (M.Map Label Type)
             deriving (Show, Eq)


  type Label = String

  data Pattern  = PVar String
                | PUnit
                | PTuple Pattern Pattern
                | PConstructor Label Pattern
             deriving (Show,Eq)
  data Copattern = Hole 
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

  pretty (Constructor "Cspace" Unit) = " "
  pretty (Constructor ['C',x] Unit) = [x]
  pretty (Constructor "Empty" Unit) = ""
  pretty (Constructor "ConsString" (Tuple x xs)) = pretty x ++ pretty xs
  pretty x = error $ show x
  

  makeCopattern f = mC
              where mC (Var x) = if x==f then return Hole else fail $
                                 "Error at " ++ f ++ " definition, " ++ 
                                 x ++ " found in a hole position. "     ++ 
                                 f ++ " was expected."
                    mC (Destructor d t) = fmap (CDestructor d) (mC t)
                    mC (App t1 t2) = do q <- mC t1
                                        p <- mP t2
                                        return (CApp q p)
                    mP (Var x) = if x/=f then return (PVar x) else fail $
                                 "Error at " ++ f ++ " definition, can not use " 
                                 ++ f ++ " for  pattern variable position."
                    mP Unit = return PUnit
                    mP (Tuple t1 t2) = do p1 <- mP t1
                                          p2 <- mP t2
                                          return (PTuple p1 p2)
                    mP (Constructor c t) = fmap (PConstructor c) (mP t)

  procDef :: Def' -> Maybe Def
  procDef (RuleDef f t r) = fmap (RuleDef f t) nr
                            where nr = mapM g r
                                  g (q,t) = do q' <- makeCopattern f q
                                               return (q',t)                               
  procDef (TypeDef s t) = return (TypeDef s t)

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
                   where f benv t@(VTyVar y) =  fmap TyVar (elemIndex y benv) <|> (lookup y env)
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
  
  natToTerm 0 = Constructor "Zero" Unit
  natToTerm n = Constructor "Succ" $ natToTerm (n-1)
  
  





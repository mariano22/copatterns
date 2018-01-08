-- Comentarios Haddock
module Typing (
       isLinear,
       isProgramTyped,
       computeAll,
       rules,
       computeAllIO
       )
       where

import Data.List
import Data.Maybe
import Control.Applicative
import Common
import qualified Data.Map as M
import Control.Monad.Except

-- Comentarios
isLinear :: Copattern -> Bool
isLinear =  allUnique . fvc
           where fvc (CApp c p) = fvc c ++ fvp p
                 fvc (CDestructor _ c) = fvc c
                 fvc _ = []
                 fvp (PVar x) = [x]
                 fvp (PTuple p1 p2) = fvp p1 ++ fvp p2
                 fvp (PConstructor _ p) = fvp p
                 fvp _ = []
                 allUnique xs = (length . nub) xs == length xs

signature :: Program -> NameEnv Type
signature = map ( \t -> (fst t,fst (snd t)) )
rules :: Program -> NameEnv [Rule]
rules = map ( \t -> (fst t,snd (snd t)) )

isProgramTyped :: Program -> WithError ()
isProgramTyped defs = if (not .isJust . lookup "main") defs then throwError "Undefined symbol main"
                      else do mapM (\(_,(ty,rs)) -> 
                               mapM (\(q,u)       -> isRuleTyped sig ty q u) rs ) defs
                              return ()
                      where sig = signature defs




isRuleTyped :: NameEnv Type -> Type -> Copattern -> Term -> WithError ()
isRuleTyped sig ty q u = do (context, inferedType) <- typeCopattern ty q
                            typeCheckTerm (context++sig) u inferedType

type WithError a = Either String a

typeCopattern :: Type -> Copattern -> WithError (NameEnv Type,Type)
typeCopattern typeA Hole = return ([],typeA)
typeCopattern typeA (CApp q p) = do (context1,typeFun) <- typeCopattern typeA q
                                    (typeB,typeC) <- case typeFun of
                                                          TyFun typeB typeC -> return (typeB,typeC)
                                                          _                 -> throwError "Error no especificado (TODO)"
                                    context2 <- typePattern typeB p
                                    return (context1 ++ context2, typeC)
typeCopattern typeA (CDestructor d q) = do (context, typeRecord) <- typeCopattern typeA q
                                           typeField <- case typeRecord of
                                                             t@(TyRecord rec) -> maybe (throwError "Error no especificado (TODO)")
                                                                                 (return . typeSubstitution t 0) (M.lookup d rec) 
                                                             _ -> throwError "Error no especificado (TODO)"
                                           return (context,typeField)

typePattern :: Type -> Pattern -> WithError (NameEnv Type)
typePattern typeA (PVar x) = return [ (x,typeA) ]
typePattern TyUnit PUnit = return []
typePattern (TyTuple type1 type2) (PTuple p1 p2) = do context1 <- typePattern type1 p1
                                                      context2 <- typePattern type2 p2
                                                      return (context1++context2)
typePattern t@(TyData typecons) (PConstructor c p) = do typeField <-  maybe (throwError "Error no especificado (TODO)")
                                                                      (return . typeSubstitution t 0) (M.lookup c typecons)
                                                        typePattern typeField p
typePattern _ _ = throwError "Error no especificado (TODO)"                               
                                        


typeSubstitution :: Type -> Int -> Type -> Type
typeSubstitution t x  = f
                 where  f (TyVar y) = if x==y then t else TyVar y
                        f TyUnit = TyUnit
                        f (TyTuple t1 t2) = TyTuple (f t1) (f t2)
                        f (TyData dDef) = TyData $ M.map g dDef 
                        f (TyFun t1 t2) = TyFun (f t1) (f t2)
                        f (TyRecord rDef) = TyRecord $ M.map g rDef 
                        g = typeSubstitution t (x+1)
                                    

typeTerm :: NameEnv Type -> Term -> WithError Type
typeTerm c = tyT
  where tyT (Var x) = maybe (throwError "Error no especificado (TODO)") return (lookup x c)
        tyT (App t1 t2) = do tyFun <- tyT t1
                             (tyA1,tyA2) <- case tyFun of
                                                 TyFun tyA1 tyA2 -> return (tyA1,tyA2)
                                                 _               -> throwError "Error no especificado (TODO)"
                             typeCheckTerm c t2 tyA1 
                             return tyA2
        tyT (Destructor d t) = do tyRecord <- tyT t
                                  case tyRecord of
                                       tt@(TyRecord rDef) -> maybe (throwError "Error no especificado (TODO)")
                                                             (return . typeSubstitution tt 0) (M.lookup d rDef) 
                                       _                    -> throwError "Error no especificado (TODO)"


typeCheckTerm :: NameEnv Type -> Term -> Type -> WithError ()
typeCheckTerm c = tyC
  where tyC Unit TyUnit = return ()
        tyC (Tuple t1 t2) (TyTuple ty1 ty2) = (tyC t1 ty1) <|> (tyC t2 ty2)
        tyC (Constructor cL t) tt@(TyData dDef) = do tcL <- maybe (throwError "Error no especificado (TODO)") return  (M.lookup cL dDef)
                                                     tyC t $ typeSubstitution tt 0 tcL
        tyC t ty = do tt <- typeTerm c t
                      if tt==ty then return () else throwError "Error no especificado (TODO)"




evalSymbol :: Term -> Maybe Symbol
evalSymbol (Var x) = Just x
evalSymbol (App e _) = evalSymbol e
evalSymbol (Destructor _ e) = evalSymbol e
evalSymbol _ = Nothing

type Substitution = [(Symbol,Term)]

matchPattern :: Term -> Pattern -> Maybe Substitution
matchPattern Unit PUnit = return []
matchPattern t (PVar x) = return [(x,t)]
matchPattern (Constructor c t) (PConstructor c' p) = if c==c' then matchPattern t p else Nothing
matchPattern (Tuple t1 t2) (PTuple p1 p2) = do sub1 <- matchPattern t1 p1
                                               sub2 <- matchPattern t2 p2
                                               return (sub1++sub2)
matchPattern _ _ = Nothing

matchCopattern :: Term -> Copattern -> Maybe Substitution
matchCopattern (Var _) Hole = return []
matchCopattern (App e t) (CApp q p) = do sub <- matchCopattern e q
                                         sub' <- matchPattern t p
                                         return (sub++sub')
matchCopattern (Destructor d e) (CDestructor d' q) = if d==d' then matchCopattern e q else Nothing
matchCopattern _ _ = Nothing

termSubstitution :: Substitution -> Term -> Term
termSubstitution s = sub
  where sub t@(Var x) = fromMaybe t (lookup x s)
        sub Unit = Unit
        sub (Constructor c t) = Constructor c (sub t)
        sub (App t1 t2) = App (sub t1) (sub t2)
        sub (Tuple t1 t2) = Tuple (sub t1) (sub t2)
        sub (Destructor d t) = Destructor d (sub t)

findTerm :: NameEnv [Rule] -> Term -> Maybe Term
findTerm rules t = do f <- evalSymbol t
                      fRules <- lookup f rules
                      (u,sub)  <- findRule fRules
                      return (termSubstitution sub u)
                   where findRule [] = Nothing
                         findRule ((q,u):fR) = fmap (\sub -> (u,sub)) (matchCopattern t q) <|> findRule fR

reduceStep :: NameEnv [Rule] -> Term -> Maybe Term
reduceStep rules t = findTerm rules t <|> case t of
                     (Tuple t1 t2) -> fmap (\t1->Tuple t1 t2) (g t1) <|> fmap (Tuple t1) (g t2)
                     (Constructor c t) -> fmap (Constructor c) (g t)
                     (App t1 t2)       -> fmap (\t1->App t1 t2) (g t1) <|> fmap (App t1) (g t2)
                     (Destructor d t)  -> fmap (Destructor d) (g t) 
                     _ -> Nothing
                     where g = reduceStep rules

computeAll :: NameEnv [Rule] -> Term -> Term
computeAll r t = maybe t (computeAll r) (reduceStep r t)

computeAllIO :: NameEnv [Rule] -> Term -> IO Term
computeAllIO r = f 0
  where f nro t = do let x = reduceStep r t
                     putStrLn $ "Eval Nro " ++ show nro ++ ": " ++  show x
                     maybe (return t) (f (nro+1)) x




















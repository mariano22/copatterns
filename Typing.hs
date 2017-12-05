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


isProgramTyped :: Program -> Bool
isProgramTyped defs = (isJust . lookup "main") defs && 
                      all (\(f,(ty,rs)) -> all (\(q,u) -> isRuleTyped sig f ty q u ) rs ) defs
                      where sig = signature defs

isRuleTyped :: NameEnv Type -> Symbol -> Type -> Copattern -> Term -> Bool
isRuleTyped sig f ty q u = maybe False ( \(context, inferedType) -> typeCheckTerm (context++sig) u inferedType ) (typeCopattern ty q)

typeCopattern :: Type -> Copattern -> Maybe (NameEnv Type,Type)
typeCopattern typeA Hole = return ([],typeA)
typeCopattern typeA (CApp q p) = do (context1,typeFun) <- typeCopattern typeA q
                                    (typeB,typeC) <- case typeFun of
                                                          TyFun typeB typeC -> return (typeB,typeC)
                                                          _                 -> Nothing
                                    context2 <- typePattern typeB p
                                    return (context1 ++ context2, typeC)
typeCopattern typeA (CDestructor d q) = do (context, typeRecord) <- typeCopattern typeA q
                                           typeField <- case typeRecord of
                                                             t@(TyRecord x rec) ->  lookup d rec >>= (return . typeSubstitution x t)
                                                             _ -> Nothing
                                           return (context,typeField)

typePattern :: Type -> Pattern -> Maybe (NameEnv Type)
typePattern typeA (PVar x) = return [ (x,typeA) ]
typePattern TyUnit PUnit = return []
typePattern (TyTuple type1 type2) (PTuple p1 p2) = do context1 <- typePattern type1 p1
                                                      context2 <- typePattern type2 p2
                                                      return (context1++context2)
typePattern t@(TyData x typecons) (PConstructor c p) = do typeField <- ( lookup c typecons >>= (return . typeSubstitution x t) )
                                                          typePattern typeField p
typePattern _ _ = Nothing                               
                                        


typeSubstitution :: TypeVar -> Type -> Type -> Type
typeSubstitution x t = f
                 where f (TyVar y) = if x==y then t else TyVar y
                       f TyUnit = TyUnit
                       f (TyTuple t1 t2) = TyTuple (f t1) (f t2)
                       f (TyData y dDef) = TyData y ( if x==y then dDef else map (\(l,ty)->(l,f ty)) dDef )
                       f (TyFun t1 t2) = TyFun (f t1) (f t2)
                       f (TyRecord y rDef) = TyRecord y ( if x==y then rDef else map (\(l,ty)->(l,f ty)) rDef )
                                    

typeTerm :: NameEnv Type -> Term -> Maybe Type
typeTerm c = tyT
  where tyT (Var x) = lookup x c
        tyT (App t1 t2) = do tyFun <- tyT t1
                             (tyA1,tyA2) <- case tyFun of
                                                 TyFun tyA1 tyA2 -> return (tyA1,tyA2)
                                                 _               -> Nothing
                             if typeCheckTerm c t2 tyA1 then return tyA2 else Nothing
        tyT (Destructor d t) = do tyRecord <- tyT t
                                  case tyRecord of
                                       tt@(TyRecord x rDef) -> lookup d rDef >>= (return . typeSubstitution x tt)
                                       _                    -> Nothing

cmpTy c1 c2 = f 
    where f (TyVar x) (TyVar y) = fromJust(elemIndex x c1) == fromJust(elemIndex y c2)
          f TyUnit TyUnit = True
          f (TyTuple xt1 xt2) (TyTuple yt1 yt2) = f xt1 yt1 && f xt2 yt2
          f (TyFun xt1 xt2) (TyFun yt1 yt2) = f xt1 yt1 && f xt2 yt2
          f (TyData x xT) (TyData y yT) = length xT == length yT && all (\((lx,tx),(ly,ty)) -> lx==ly && cmpTy (x:c1) (y:c2) tx ty) (zip xT yT)
          f (TyRecord x xT) (TyRecord y yT) = length xT == length yT && all (\((lx,tx),(ly,ty)) -> lx==ly && cmpTy (x:c1) (y:c2) tx ty) (zip xT yT)

typeCheckTerm :: NameEnv Type -> Term -> Type -> Bool
typeCheckTerm c = tyC
  where tyC Unit TyUnit = True
        tyC (Tuple t1 t2) (TyTuple ty1 ty2) = (tyC t1 ty1) && (tyC t2 ty2)
        tyC (Constructor cL t) tt@(TyData x dDef) = maybe False ((tyC t) . (typeSubstitution x tt)) (lookup cL dDef)
        tyC t ty = maybe False (cmpTy [] [] ty) (typeTerm c t)
-- TODO



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




















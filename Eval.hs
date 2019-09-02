module Eval
( eval,        -- Evaluates a Term given a Program
  quote        -- Given a Value transform it to a Term to be PrettyPrinted
) where
import Common
import Utils
import Typing

import qualified Data.Map as M
import Control.Monad.Except
import Data.Maybe
import Data.List
import Control.Applicative

isNegTy :: Type -> Bool
isNegTy (TyVar _) = error "Esto no deberia suceder."
isNegTy (TyBound _) = error "Esto no deberia suceder."
isNegTy TyUnit = False
isNegTy (TyTuple _ _) = False
isNegTy (TyData _) = False
isNegTy (TyFun _ _) = True
isNegTy (TyRecord _) = True

termToEContext :: Term -> EContext Term
termToEContext (Var f) = EHole f
termToEContext (App e t) = EApp (termToEContext e) t
termToEContext (Destructor d e) = EDestructor d (termToEContext e)
termToEContext _ = error "Esto no debería suceder!"

evalSymbol :: EContext x -> Symbol
evalSymbol (EHole f) = f
evalSymbol (EApp e _) = evalSymbol e
evalSymbol (EDestructor _ e) = evalSymbol e

data EContext x = EHole Symbol
                | EApp (EContext x) x
                | EDestructor Symbol (EContext x)

data ContextE x = EEHole Symbol | EEApp x | EEDestructor Symbol

eContextToTerm :: EContext Term -> Term
eContextToTerm (EHole f) = Var f
eContextToTerm (EApp e t) = Tuple (eContextToTerm e) t
eContextToTerm (EDestructor d e) = Destructor d (eContextToTerm e)

eContextList :: EContext Value -> [ ContextE Value ]
eContextList = reverse . eCL
  where eCL (EHole f) = [EEHole f]
        eCL (EApp e v) = EEApp v : eCL e
        eCL (EDestructor d e) = EEDestructor d : eCL e

eContextTree :: Term -> [ ContextE Value ] -> Term
eContextTree ev = eCT . reverse
  where eCT [] = ev
        eCT [EEHole f] = Var f
        eCT (EEApp v : xs) = App (eCT xs) (valueToTerm v)
        eCT (EEDestructor d : xs) = Destructor d (eCT xs)

copatternList :: Copattern -> [ ContextE Pattern ]
copatternList = reverse . cL
  where cL (CHole f) = [EEHole f]
        cL (CApp q p) =  EEApp p : cL q
        cL (CDestructor d q) = EEDestructor d : cL q

matchCopattern :: [ ContextE Pattern ] -> [ ContextE Value ] -> Maybe ( Substitution, [ ContextE Value ] )
matchCopattern (EEHole f:as) (EEHole f':bs) = if f==f' then matchCopattern as bs else Nothing
matchCopattern (EEApp p:as) (EEApp v:bs) = do sub1 <- matchPattern p v
                                              (sub2,rest) <- matchCopattern as bs
                                              return (sub1++sub2,rest)
matchCopattern (EEDestructor d:as) (EEDestructor d':bs) =
  if d==d' then matchCopattern as bs else Nothing
matchCopattern [] cv = return ([],cv)


instance Functor EContext where
  fmap _ (EHole f) =  EHole f
  fmap f (EApp e x) = EApp (fmap f e) (f x)
  fmap f (EDestructor d e) =  EDestructor d (fmap f e)

data Value = PVUnit
           | PVConstructor Symbol Value
           | PVPair Value Value
           | NegV (EContext Term)

matchPattern :: Pattern -> Value -> Maybe Substitution
matchPattern (PVar x) v = return [ (x, valueToTerm v) ]
matchPattern PUnit PVUnit = return []
matchPattern (PConstructor c' p) (PVConstructor c v) = if c==c' then matchPattern p v else Nothing
matchPattern (PTuple p1 p2) (PVPair t1 t2) = do sub1 <- matchPattern p1 t1
                                                sub2 <- matchPattern p2 t2
                                                return (sub1++sub2)

valueToTerm (PVUnit) = Unit
valueToTerm (PVConstructor c v) = Constructor c (valueToTerm v)
valueToTerm (PVPair v1 v2) = Tuple (valueToTerm v1) (valueToTerm v2)
valueToTerm (NegV ec) = eContextToTerm ec

quote :: Value -> Term
quote = valueToTerm

eval :: Program -> Term -> Value
eval p@(tySig, syDefs) = eval'
  where inferTy :: Term -> Maybe Type
        inferTy t = case runExcept (typeTerm p t) of
                      Left _ -> Nothing
                      Right ty -> Just (expandT tySig ty)
        evalEContextArgs :: EContext Term -> EContext Value
        evalEContextArgs = fmap eval'
        reduceEVContext :: EContext Value -> Value
        reduceEVContext ev = eval' (contractTerm syDefs ev)
        eval' :: Term -> Value
        eval' Unit = PVUnit
        eval' (Tuple t1 t2) = PVPair (eval' t1) (eval' t2)
        eval' (Constructor c t) = PVConstructor c (eval' t)
        eval' t = if isNegativeType then NegV tEContext
                  else (reduceEVContext . evalEContextArgs) tEContext
                  where isNegativeType = maybe False isNegTy (inferTy t)
                        tEContext = termToEContext t


contractByRule :: EContext Value -> Rule -> Maybe Term
contractByRule ev r =
  let q = (getCopattern r)
      t = (getTerm r)
      qList = copatternList q
      eList = eContextList ev
  in do (sub, evList) <- matchCopattern qList eList
        return $ eContextTree (termSubstitution sub t) evList

contractTerm   :: SymbolDefs -> EContext Value -> Term
contractTerm defs ev =
  let rules = (map getDef . snd) $ defs M.! (evalSymbol ev)
  in case (foldl1' (<|>) . map (contractByRule ev)) rules of
    Nothing -> error "Esto no deberia suceder si hay buen covering."
    Just t  -> t

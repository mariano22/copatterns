module Eval
( eval,        -- Evaluates a Term given a Program
  quote        -- Given a Value transform it to a Term to be PrettyPrinted
) where
import Common
import Computations
import Utils
import Typing

import qualified Data.Map as M
import Control.Monad.Except
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad.Writer.Lazy
import Control.Monad.Trans.Maybe

{- Para determinar si un tipo es negativo.
   Cómo lo usaremos sobre tipos inferidos y expandidos, no debería ser TyVar ni
   TyBound. -}
isNegTy :: Type -> Bool
isNegTy (TyVar _) = error "Esto no deberia suceder porque se usa sobre tipos expandidos."
isNegTy (TyBound _) = error "Esto no deberia suceder porque se usa sobre un tipo inferido."
isNegTy TyUnit = False
isNegTy (TyTuple _ _) = False
isNegTy (TyData _) = False
isNegTy (TyFun _ _) = True
isNegTy (TyRecord _) = True

{- Asumiendo que un Término tiene forma de contexto de evaluación lo transforma
   al tipo de los contexto de evaluación. -}
termToEContext :: Term -> EContext Term
termToEContext (Var f) = EHole f
termToEContext (App e t) = EApp (termToEContext e) t
termToEContext (Destructor d e) = EDestructor d (termToEContext e)
termToEContext _ = error "Esto no debería suceder porque se asume que el término tiene forma de contexto de evaluación."

{- Dado un contexto listo para evluar (con sólo valores) devuelve su representación
   como [ ContextE Value ] (mirada del entorno "desde adentro"). -}
eContextList :: EContext Value -> [ ContextE Value ]
eContextList = reverse . eCL
  where eCL (EHole f) = [EEHole f]
        eCL (EApp e v) = EEApp v : eCL e
        eCL (EDestructor d e) = EEDestructor d : eCL e

{- Dado un contexto listo para evluar (con sólo valores) como [ ContextE Value ]
   devuelve el símbolo a evaluar. -}
evalSymbol :: [  ContextE Value ] -> Symbol
evalSymbol ((EEHole f):xs) = f

-- Función exportada que traduce valores a término (útiles para imprimir con PrettyPrinter).
quote :: Value -> Term
quote (PVUnit) = Unit
quote (PVConstructor c v) = Constructor c (quote v)
quote (PVPair v1 v2) = Tuple (quote v1) (quote v2)
quote (NegV ec) = eContextToTerm ec
  where eContextToTerm :: EContext Term -> Term
        eContextToTerm (EHole f) = Var f
        eContextToTerm (EApp e t) = Tuple (eContextToTerm e) t
        eContextToTerm (EDestructor d e) = Destructor d (eContextToTerm e)

-- Dado un copatrón nos devuelve su representación "desde adentro".
copatternList :: Copattern -> [ ContextE Pattern ]
copatternList = reverse . cL
  where cL (CHole f) = [EEHole f]
        cL (CApp q p) =  EEApp p : cL q
        cL (CDestructor d q) = EEDestructor d : cL q

{- Tipo de las computaciones que van generando una Substitution (mapeo de
   variables por términos) y pueden fallar.
   La usaremos en el proceso de matchear términos con copatrones/patrones. -}
type MatchingRes a = MaybeT (Writer Substitution) a

yieldSubstitution :: Symbol -> Term -> MatchingRes ()
yieldSubstitution s t  = lift $ tell [(s,t)]
failMatching :: MatchingRes a
failMatching = mzero
runMatching :: MatchingRes a -> Maybe (a, Substitution)
runMatching m = let (mayA, sub) = runWriter (runMaybeT m) in fmap (\a->(a,sub)) mayA

{- Indica si un copatrón matchea con un contexto retornando el resto de contexto
   no matcheado y generando una substitución.
   Utilizamos la representación "desde adentro" de copatrones y contextos ya que
   el matcheo se realiza de adentro hacia afuera (desde el símbolo). -}
matchCopattern :: [ ContextE Pattern ] -> [ ContextE Value ] -> MatchingRes [ ContextE Value ]
matchCopattern (EEHole f:as) (EEHole f':bs) =
  if f==f' then matchCopattern as bs else failMatching
matchCopattern (EEApp p:as) (EEApp v:bs) = do matchPattern p v
                                              matchCopattern as bs
matchCopattern (EEDestructor d:as) (EEDestructor d':bs) =
  if d==d' then matchCopattern as bs else failMatching
matchCopattern [] cv = return cv

-- Chequea que un patrón matchee con un valor generando una substitución.
matchPattern :: Pattern -> Value -> MatchingRes ()
matchPattern (PVar x) v = yieldSubstitution x (quote v)
matchPattern PUnit PVUnit = return ()
matchPattern (PConstructor c' p) (PVConstructor c v) =
  if c==c' then matchPattern p v else failMatching
matchPattern (PTuple p1 p2) (PVPair t1 t2) = do matchPattern p1 t1
                                                matchPattern p2 t2


-- Función de evaluación de Términos a Valores dado un Programa.
eval :: Program -> Term -> Value
eval p@(tySig, syDefs) = eval'
  where -- Dado un término trata de inferir su tipo y retorna el tipo expandido.
        inferTy :: Term -> Maybe Type
        inferTy t = case runExcept (typeTerm p t) of
                      Left _ -> Nothing
                      Right ty -> Just (expandT tySig ty)
        -- Dado un contexto de evaluación evalúa los términos en posiciones de argumentos.
        evalEContextArgs :: EContext Term -> EContext Value
        evalEContextArgs = fmap eval'
        -- Dado un contexto con sólo valores lo evalúa (matchea y aplica regla).
        evalEContextWithValues :: EContext Value -> Value
        evalEContextWithValues =  evalEContextWithValues' . eContextList
        -- Analogo a anterior pero sobre la representación "desde adentro" del contexto.
        evalEContextWithValues'   :: [ ContextE Value ] -> Value
        evalEContextWithValues' evList =
          let eSymbol = evalSymbol evList
              candidatesRules = (map getDef . snd) (syDefs M.! eSymbol)
          in case (foldl1' (<|>) . map (contractByRule evList)) candidatesRules of
            Nothing -> error "Esto no deberia suceder si hay buen covering."
            Just (t, evList') -> if null evList' then eval' t
                                 else let innerEvList = eContextList . evalEContextArgs . termToEContext $ t
                                      in evalEContextWithValues' (innerEvList ++ evList')
        -- Función principal, tranforma Términos en Valores.
        eval' :: Term -> Value
        eval' Unit = PVUnit
        eval' (Tuple t1 t2) = PVPair (eval' t1) (eval' t2)
        eval' (Constructor c t) = PVConstructor c (eval' t)
        eval' t = if isNegativeType then NegV tEContext
                  else (evalEContextWithValues . evalEContextArgs) tEContext
                  where isNegativeType = maybe False isNegTy (inferTy t)
                        tEContext = termToEContext t

{- Función auxiliar que dado un contexto con sólo valores (representación "desde
   adentro") y una regla prueba matchear y sí lo hace retorna el término generado
   por la substitución correspondiente y el pedazo de entorno con valores no matcheado -}
contractByRule :: [ ContextE Value ] -> Rule -> Maybe ( Term, [ContextE Value] )
contractByRule evList r =
  let q = (getCopattern r)
      t = (getTerm r)
      qList = copatternList q
  in do (evList', sub) <- runMatching (matchCopattern qList evList)
        return (termSubstitution sub t, evList')

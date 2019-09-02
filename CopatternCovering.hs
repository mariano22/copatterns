{- In this module we develop a polynomial algorith for copattern covering.-}
module CopatternCovering
( checkCoverage,  {- Checks all definitions in a program have a good covering
                     or fail with an error telling which symbol is not defined properly. -}
  coverage        {- Given type of the defined symbol determines if a provided
                     list of Copatterns is a good covering. -}
) where
import Common
import Utils
import TError
import PrettyPrinter

import Text.PrettyPrint
import Data.List
import Data.Function
import Data.Maybe
import Control.Monad.Except
import qualified Data.Map as M

{-  We are going to work with [ CoppaternE ] instad of Copattern.
    Coppatern that are pattern application with correspond to CEPattern, the type
    is the type of the applied Pattern.
    Coppatern that are destructor application with correspond to CEDestructor,
    the (M.Map Symbol Type) is the type signature of the TyRecord type that is
    defined.
-}
data CoppaternE = CEDestructor Symbol (M.Map Symbol Type) | CEPattern Pattern Type deriving (Show,Eq)
-- The type of a Copattern.
data CopatternT = CTDestructor | CTPattern deriving (Show,Eq)
-- The type of the pattern.
data PatternT = PTVar | PTUnit | PTTuple | PTConstructor deriving (Show,Eq)
-- Each copattern is translated to a list of CoppaternE.
type Rama = [ CoppaternE ]
-- A set of copattern we are analysing.
type Subarbol = [ Rama ]

{- Translates Copattern -> [CoppaternE]. The type is first with a dummy type and
   then typed properly (in 'typeCopatternLine') given the defined symbol type. -}
copatternToLine :: Copattern -> Rama
copatternToLine (CHole f)         =   []
copatternToLine (CApp q p)        =   copatternToLine q ++ [ CEPattern p (TyVar "dummy")    ]
copatternToLine (CDestructor d q) =   copatternToLine q ++ [ CEDestructor d M.empty         ]

typeCopatternLine :: Type -> Rama -> Rama
typeCopatternLine _ [] = []
typeCopatternLine ty@(TyRecord sig) ((CEDestructor d _):cs) =
  CEDestructor d sig:(typeCopatternLine (indexSignature ty d) cs)
typeCopatternLine (TyFun t1 t2) ((CEPattern p _):cs) =
  (CEPattern p t1):(typeCopatternLine t2 cs)

-- Typing function for our Copattern/Pattern types.
typeOfCopattern :: CoppaternE -> CopatternT
typeOfCopattern (CEDestructor _ _)  =   CTDestructor
typeOfCopattern (CEPattern _ _)     =   CTPattern

typeOfPattern ::  Pattern -> PatternT
typeOfPattern (PVar _)            =   PTVar
typeOfPattern (PUnit)             =   PTUnit
typeOfPattern (PTuple _ _)        =   PTTuple
typeOfPattern (PConstructor _ _)  =   PTConstructor

-- Pretty print function to debugging.
printCopatternE :: CoppaternE -> IO ()
printCopatternE (CEDestructor d _) = putStr(" "++d++" ")
printCopatternE (CEPattern p _) = putStr (" "++ render (printPattern p)++" ")
printRama :: Rama -> IO ()
printRama xs = do putStr "[ "
                  mapM printCopatternE xs
                  putStrLn " ]"
printSubarbol :: Subarbol -> IO ()
printSubarbol s = do putStrLn "Subarbol"
                     mapM_ printRama  s

-- Auxiliar functions to extract that present in CopatternE

-- Get a Pattern from a CopatternE that is a Pattern application.
fromPattern :: CoppaternE -> Pattern
fromPattern (CEPattern p _) = p

-- Get the type of the Pattern from a CopatternE that is a Pattern application.
getTypePattern :: CoppaternE -> Type
getTypePattern (CEPattern _ ty) = ty

-- Get the destructor symbol from a CopatternE that is a Destructor application.
destructorSymbol :: CoppaternE -> Symbol
destructorSymbol (CEDestructor f _) = f

-- Get the destructor type signature from a CopatternE that is a Destructor application.
destructorSignature :: CoppaternE -> M.Map Symbol Type
destructorSignature (CEDestructor _ s)  =   s

{- Get the constructor symbol from a CopatternE that is a Pattern application.
   And the pattern is a constructor pattern. -}
constructorSymbol :: CoppaternE -> Symbol
constructorSymbol (CEPattern (PConstructor c _) _) = c

-- Check if a CopatternE is a application of a free variable (i.e. "f x").
isPVar :: CoppaternE -> Bool
isPVar (CEPattern (PVar _) _) = True
isPVar _ = False

{- Given a CoppaternE that is a Pattern application and the pattern is a constructor,
   returns the CEPattern of the inner pattern. -}
takeOffOnceConstructor :: CoppaternE -> CoppaternE
takeOffOnceConstructor (CEPattern (PConstructor c p) ty) = CEPattern p (indexSignature ty c)

{- Given a CoppaternE that is a Pattern application of a free variable of type
  pair replace it for a double application of free variables. ( f (x,y) <--> f x y ) -}
splitVariablePair :: CoppaternE -> [CoppaternE]
splitVariablePair (CEPattern (PVar x) (TyTuple t1 t2)) = [(CEPattern (PVar x) t1), (CEPattern (PVar x) t2)]
{- Given a CoppaternE that is a Pattern application of a tuple pattern
  replace it for a double application of the patterns. ( f (p1,p1) <--> f p1 p2 ) -}
splitPairPattern :: CoppaternE -> [CoppaternE]
splitPairPattern (CEPattern (PTuple p1 p2) (TyTuple t1 t2)) = [(CEPattern p1 t1), (CEPattern p2 t2)]

-- Returns the defined symbol of a given copattern.
evalSymbol :: Copattern -> Symbol
evalSymbol (CHole f)              =   f
evalSymbol (CApp q _)             =   evalSymbol q
evalSymbol (CDestructor _ q)      =   evalSymbol q

-- Extra auxiliary functions

{- Given a Data/Record type and a symbol defined returns the type of the symbol
   field with the correspondient substitution for the ligated type variable -}
indexSignature :: Type -> Symbol -> Type
indexSignature ty s = index' . fromJust $ getSignatureTy ty >>= M.lookup s
  where index' tyField = typeSubstitution ty 0 tyField

{- Given a program and a type symbol returns the type it represent expanded
  (without synonym type var present). -}
getSymbolExpandedType :: Program -> Symbol -> Type
getSymbolExpandedType (tySig ,syDefs) sy = expandT tySig ((getDef . fst) (syDefs M.! sy))

-- Check if a list of elements are all equals.
isSingleton :: Eq a => [a] -> Bool
isSingleton = (==) 1 . length . nub

-- Exported functions:
{- Check all definitions in a program have a good copattern covering. If not, fails
   with an error indicating which symbol is not properly defined. -}
checkCoverage :: Program -> Except TError ()
checkCoverage p@(_,s) = let qs :: [[Copattern]]
                            qs = map (map (getCopattern . getDef)) . map snd . M.elems $ s
                            qsS :: [ ([Copattern], Symbol) ]
                            qsS = map (\cs -> (cs, evalSymbol  (head cs))) qs
                            qsC :: [(Bool, Symbol)]
                            qsC = map (\(cs,s) -> (coverage (getSymbolExpandedType p s) cs, s)) qsS
                        in case find (\(c,_) -> not c) qsC of
                          Nothing -> return ()
                          Just (False, sy) -> throwError ("Not covering at "++sy)

{- Given type of the defined symbol determines if a provided
   list of Copatterns is a good covering. -}
coverage :: Type -> [Copattern] -> Bool
coverage ty qs =
  let isSameSymbol = isSingleton . map evalSymbol $ qs
      subTree :: Subarbol
      subTree = map (typeCopatternLine ty . copatternToLine) qs
  in if not isSameSymbol then error "No tienen el mismo simbolo!"
     else coverT subTree

{- The set of coppatern is trivially a good covering if some of it it's just the definition
  (defined symbol only) without destructor or pattern application. -}
isTriviallyCovered :: Subarbol -> Bool
isTriviallyCovered = elem []

-- Split the set of Coppatern in which most inner coppatern is an application of free variable and which not.
splitVarSubTree :: Subarbol -> (Subarbol, Subarbol)
splitVarSubTree = partition (\(copatternE:restRama) -> isPVar copatternE)

subTreeGroupByRep :: (Rama -> (Symbol,Rama)) -> Subarbol -> ( [Symbol], [Subarbol] )
subTreeGroupByRep addRep subTree =
  let subTreeWithRep :: [(Symbol,Rama)]
      subTreeWithRep = map addRep subTree
      repPresent :: [Symbol]
      repPresent = (sort . nub . map fst) subTreeWithRep
      grouped :: [ [ (Symbol,Rama) ] ]
      grouped = groupBy ((==) `on` fst) subTreeWithRep
      groupedSubTrees :: [ Subarbol ]
      groupedSubTrees = map (map snd) grouped
  in (repPresent, groupedSubTrees)

-- The heart of our algorithm
-- Checks if a set of copattern are a good covering.
coverT :: Subarbol -> Bool
coverT subTree =
  if length subTree == 0 then error "Nunca deberiamos llamar con un subarbol vacio."
  else if isTriviallyCovered subTree then True
  else let heads :: [ CopatternT ]
           heads = map (typeOfCopattern . head) subTree
           headsSameType :: Bool
           headsSameType = isSingleton heads
       in if not headsSameType then error "Esto deberia estar filtrado por type checking."
          else case head heads of
            CTDestructor -> coverTDest subTree
            CTPattern    -> coverTPatt subTree

{- Checks the case the most internal applied copattern is a Pattern application. -}
coverTPatt ::  Subarbol -> Bool
coverTPatt subTree =
  let heads :: [ PatternT ]
      heads = filter (/= PTVar) . map (typeOfPattern. fromPattern . head) $ subTree
      headsSameType :: Bool
      headsSameType = isSingleton heads
  in if heads==[] then coverTAllVars subTree
     else if not headsSameType then error "Esto deberia estar filtrado por type checking."
     else case head heads of
            PTUnit -> coverTUnit subTree
            PTTuple -> coverTPair subTree
            PTConstructor -> coverTCons subTree

{- Checks the case the most internal applied copattern is a Pattern application
   (and all the applied things are free variables). -}
coverTAllVars :: Subarbol -> Bool
coverTAllVars subTree =
  let withoutHeads :: Subarbol
      withoutHeads = map tail subTree
  in coverT withoutHeads

{- Checks the case the most internal applied copattern is a Pattern application
   and some of it are unit patterns. -}
coverTUnit :: Subarbol -> Bool
coverTUnit subTree =
  let withoutHeads :: Subarbol
      withoutHeads = map tail subTree
  in coverT withoutHeads

{- Checks the case the most internal applied copattern is a Pattern application
   and some of it are pair patterns. -}
coverTPair :: Subarbol -> Bool
coverTPair subTree =
  let ( varSubTree, consSubTree ) = splitVarSubTree subTree
      splitVariablePairInRama :: Rama -> Rama
      splitVariablePairInRama (x:xs) = splitVariablePair x++xs
      splitPairPatternInRama :: Rama -> Rama
      splitPairPatternInRama (x:xs)  = splitPairPattern x++xs
      varSubTree' = map splitVariablePairInRama varSubTree
      consSubTree' = map splitPairPatternInRama consSubTree
  in coverT(varSubTree'++consSubTree')

{- Checks the case the most internal applied copattern is a Pattern application
   and some of it are constructor patterns. -}
coverTCons :: Subarbol -> Bool
coverTCons subTree =
  let ( varSubTree, consSubTree ) = splitVarSubTree subTree
      typePatterns  :: [ Type ]
      typePatterns = map (getTypePattern . head) consSubTree
      uniquePatternType :: Bool
      uniquePatternType = and(map isDataTy typePatterns) &&  isSingleton  (map (fromJust . getSignatureTy) typePatterns)
      allConstructorsFromType :: [Symbol]
      allConstructorsFromType = (M.keys . fromJust . getSignatureTy . head) typePatterns
      addRep :: Rama -> (Symbol,Rama)
      addRep (x:xs) = (constructorSymbol x,takeOffOnceConstructor x:xs)
      (constructorPresent, groupedSubTrees) = subTreeGroupByRep addRep consSubTree
  in if not uniquePatternType then error "This shouldn't happen :("
     else ( constructorPresent == allConstructorsFromType || ( (not . null) varSubTree  && coverT varSubTree ) )
          && and (map (coverT . (++varSubTree)) groupedSubTrees)

{- Checks the case the most internal applied copattern is a Destructor application -}
coverTDest :: Subarbol -> Bool
coverTDest subTree =
  let addRep :: Rama -> (Symbol,Rama)
      addRep (x:xs) = (destructorSymbol x,xs)
      (destructorPresent, groupedSubTrees) = subTreeGroupByRep addRep subTree
      signatures  :: [  M.Map Symbol Type ]
      signatures = map (destructorSignature . head) subTree
  in if not (isSingleton signatures) then error "This shouldn't happen :( "
     else destructorPresent == (M.keys (head signatures)) && and (map coverT groupedSubTrees)

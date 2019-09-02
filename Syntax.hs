module Syntax
( makeProgram -- Given a ProgramDefs (from happy parser) constructs a Program.
) where
import Common
import TError
import TopSort
import Utils
import Data.Function
import Data.List
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Except
import Data.Maybe

{- Helper function that raises an error if any 'symbolsToAnalyse' is not defined
   in the 'definedSymbols'. 'symbolsToAnalyse' is a pair of the symbols to analyse
   and the occurrence line of it.
   The error callback receive the line and the undefined symbol. -}
checkDefinedSymbols :: (Int -> Symbol -> TError) -> [ Symbol ] -> [(Symbol, Int)] -> Except TError ()
checkDefinedSymbols errorFactory definedSymbols symbolsToAnalyse = mapM_ checkDef symbolsToAnalyse
  where checkDef (s,l) = unless (elem s definedSymbols) (throwError $ errorFactory l s) :: Except TError ()

{- Given type definitions and a [(TypeSymbol, OccurrenceLine)] checks if there
   are non defined symbols type and throw a typeNotDefinedError error. -}
checkDefinedTypeSymbols :: [ Def TypeDef ] -> [(Symbol, Int)] -> Except TError ()
checkDefinedTypeSymbols tyDefs = checkDefinedSymbols typeNotDefinedError $ map (getTypeVar . getDef) tyDefs

{-
   =============
   =  TermDef  =
   =============

   Following functions helps to translate from [TermDef] to a Symbol to [Rule] mapping.
-}


-- Given a l.h.s. term transform to Copattern if it's well formed or returns error.
makeCopattern :: Term -> Except TError Copattern
makeCopattern (Var x) = return (CHole x)
makeCopattern (Destructor d t) = liftM (CDestructor d) (makeCopattern t)
makeCopattern (App t1 t2) = liftM2 CApp (makeCopattern t1) (makePattern t2)
makeCopattern _ = throwError parsingCopatternError

-- Given a term transform to Pattern if it's well formed or returns error.
makePattern :: Term -> Except TError Pattern
makePattern (Var x) = return (PVar x)
makePattern Unit = return PUnit
makePattern (Tuple t1 t2) = liftM2 PTuple (makePattern t1) (makePattern t2)
makePattern (Constructor c t) = liftM (PConstructor c) (makePattern t)
makePattern _ = throwError parsingCopatternError

-- Given a Copattern returns the defined symbol.
copatternSymbol :: Copattern -> Symbol
copatternSymbol (CHole f) = f
copatternSymbol (CApp q _) = copatternSymbol q
copatternSymbol (CDestructor _ q) = copatternSymbol q

-- Given a TermDef returns a Rule definition if l.h.s. is parseable as Copattern.
makeRule :: Def TermDef -> Except TError (Def Rule)
makeRule d = let lt = lhs ( getDef d )
                 rt = rhs ( getDef d )
                 l  = getDefLineNo d
             in do q <- withExceptT (addLineNo l) (makeCopattern lt)
                   if isLinear q
                      then return Def { getDefLineNo = l,
                                        getDef = Rule { getCopattern = q,
                                                        getTerm = rt } }
                      else throwError $ addLineNo l nonLinearCopatternError

{- Given a list of Rules definition group it by defined symbol, group them by
  consecutives groups of defined symbol and returns a map from symbol to list of
  respective rules.
  Check that a symbol is not defined in more than a group. -}
groupRules :: [Def Rule] -> Except TError (M.Map Symbol [Def Rule])
groupRules ds =
  case duplicateElem (map getDef symbolDefinitions) of
    Nothing     -> (return . M.fromList) [ (symbol, rules) | xs <- groupedRulesAndSymbol,
                   let symbol = (snd . head) xs, let rules = (fst . unzip) xs ]
    Just symbol -> let line = fromJust $ getDefinitionLine (reverse symbolDefinitions) symbol
                   in throwError $ symbolRedefinitionError line symbol
  where rulesAndSymbol :: [ (Def Rule , Symbol) ]
        rulesAndSymbol = zip ds . map (copatternSymbol . getCopattern . getDef) $ ds
        groupedRulesAndSymbol :: [ [ (Def Rule , Symbol) ] ]
        groupedRulesAndSymbol = groupBy ((==) `on` snd) rulesAndSymbol
        symbolDefinitions :: [ Def Symbol ]
        symbolDefinitions =  [ d{ getDef = s } | (d,s):_ <- groupedRulesAndSymbol ]

{-
   =============
   =  TypeDef  =
   =============

   Following functions helps to translate from [TypeDef] to TypeSignature doing
   correctness checks.
-}

{- Checks that all type symbols that occurred in any Type definition are defined
   in some synonym. -}
checkAllTypesDefined :: [ Def TypeDef ] -> Except TError ()
checkAllTypesDefined ds = checkDefinedTypeSymbols ds symbolAndLines
  where symbolAndLines = [ (sTy, getDefLineNo d) | d <- ds, sTy <- (fvType . getType . getDef) d ]

-- Checks there is no type symbol defined twice.
checkTypeRedefinition :: [ Def TypeDef ] -> Except TError ()
checkTypeRedefinition typeDefs =
  case  duplicateElem (map (getTypeVar . getDef) typeDefs) of
    Nothing -> return () :: Except TError ()
    Just symbol -> let  symbolDefinitions = map (fmap getTypeVar) typeDefs :: [Def Symbol]
                        line1 = fromJust $ getDefinitionLine symbolDefinitions symbol
                        line2 = fromJust $ getDefinitionLine (reverse symbolDefinitions) symbol
                    in throwError $ typeRedefinitionError line1 line2 symbol

{- Checks there is no cycle in the graph of type symbols synonym definitions.
   Assumes that each type symbol is defined exactly once. -}
checkTypeCycleDependencies :: [ Def TypeDef ] -> Except TError ()
checkTypeCycleDependencies ds =
  let graph :: (M.Map Symbol [Symbol])
      graph = M.fromList [ (getTypeVar d, (fvType . getType) d) | d' <- ds, let d = getDef d' ]
      symbolDefinition = (map  (fmap getTypeVar) ds)
      errorMsg :: [Symbol] -> Except TError ()
      errorMsg symbols = let lines = map (fromJust . (getDefinitionLine symbolDefinition)) symbols
                         in throwError . typeCycleDependenciesError $ zip symbols lines
  in maybe (return ()) errorMsg $ existsCycle graph

{- Given a list of Type synonym definitions returns TypeSignature (a maping from
   symbol to defined type).
   Does the above checkings (each type symbol is defined exactly once and there
   is no cycle in type synonym definitions). -}
makeTypeSignature :: [ Def TypeDef ] -> Except TError TypeSignature
makeTypeSignature ds =
  do checkAllTypesDefined ds
     checkTypeRedefinition ds
     checkTypeCycleDependencies ds
     return . TypeSig . M.fromList . map liftSymbol $ ds
  where liftSymbol :: Def TypeDef -> (Symbol, Def Type)
        liftSymbol d = ( (getTypeVar . getDef) d, fmap getType d )

{-
   ==================
   =  SignatureDef  =
   ==================

   Following functions helps to translate from [SignatureDef] to a identifier
   Symbol to Type mapping.
-}

-- Checks the used type symbols at any SignatureDef is defined at some point.
checkDefinedTypesSignature :: [ Def TypeDef ] -> [Def SignatureDef] -> Except TError ()
checkDefinedTypesSignature defs signature = checkDefinedTypeSymbols defs typeVars
  where typeVars :: [(Symbol,Int)]
        typeVars = [ (s,getDefLineNo d) | d <- signature, s <-(fvType . getSymbolType . getDef) d ]

{- Given a list of signature declarations (like "myOwnUnit : ()") returns a Symbol
   to Type definition mapping (VarSignature).
   Check there is no duplicate declaration. -}
makeGlobalSignature :: [Def SignatureDef] -> Except TError VarSignature
makeGlobalSignature = foldM addSymbolOrFail (VarSig M.empty)
                      where addSymbolOrFail :: VarSignature -> (Def SignatureDef) -> Except TError VarSignature
                            addSymbolOrFail (VarSig m) d =
                              let l  = getDefLineNo d
                                  s  = getSymbol . getDef $ d
                                  t  = getSymbolType . getDef $ d
                              in if M.member s m then throwError $ duplicateSignatureError l s
                                 else return . VarSig $ M.insert s Def{getDef = t, getDefLineNo=l} m

{-
  =============
  =  Program  =
  =============

  Following functions helps to translate from ProgramDefs to Program, applying
  all the checkings.
  Program is the final form o a well syntaxis constructed program.
  Typing and Semantic will work on it.
-}

{- Given a mapping from a Symbol to [ Rule ] (definitions) and a VarSignature
   (a mapping from Symbol to Type, the declarations). Checks each defined symbol
   has a declaration and each declared symbol has a definition. -}
makeSymbolDefinitions :: [ ( Symbol, [Def Rule] ) ] -> VarSignature -> Except TError SymbolDefs
makeSymbolDefinitions symbolToRules (VarSig symbolToTypes) =
  let symbolWithRules :: [(Symbol, Int)]
      symbolWithRules = zip (map fst symbolToRules) (map (getDefLineNo . head . snd) symbolToRules)
      symbolToTypes'  = M.toList symbolToTypes
      symbolWithType :: [(Symbol, Int)]
      symbolWithType  = zip (map fst symbolToTypes') (map (getDefLineNo . snd) symbolToTypes')
      addType (symbol, rules) = (symbol, (symbolToTypes M.! symbol, rules))
  in do checkDefinedSymbols symbolWithoutSignatureError (map fst symbolWithType) symbolWithRules
        checkDefinedSymbols symbolWithoutDefinitionError (map fst symbolWithRules) symbolWithType
        return . M.fromList . map addType $ symbolToRules

{- Given a ProgramDefs (obtained from happy parser) constructs a Program:
    - TypeSignature: a mapping from Symbol -> Type definitions
    - SymbolDefs: a mapping from Symbol -> (Type declared, [Rules] at definition).
   Applying more syntaxis checks (at type definitions, signatures declarations,
  and rule definitions).
-}
makeProgram :: ProgramDefs -> Except TError Program
makeProgram ProgramDefs{typeDefs = tyDefs, signatureDefs = sigDefs, termDefs = termDefs} =
  do typeSignature <- makeTypeSignature tyDefs
     checkDefinedTypesSignature tyDefs sigDefs
     symbolToRules <- mapM makeRule termDefs >>= groupRules
     symbolToTypes <- makeGlobalSignature sigDefs
     symbolDefs <- makeSymbolDefinitions (M.toList symbolToRules) symbolToTypes
     return (typeSignature,symbolDefs)

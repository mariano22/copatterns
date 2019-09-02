module TError where
import Common
import PrettyPrinter

import qualified Text.PrettyPrint as PP
import Data.List
import qualified Data.Map as M

-- The error message type.
type TError = String

-- Concatenate TError's with and endline in between.
addError :: TError -> TError -> TError
addError e1 e2 = e1++"\n"++e2
-- Error message specifying a line number.
lineNoError :: Int -> TError
lineNoError line = "Error at line " ++ show line ++ ": "
expresionErrorAt :: PPrintable p =>  p -> TError
expresionErrorAt ep = "Error while processing: " ++ PP.render (pprint ep)
-- Given a line number prepend to an error a message associating the line with the error.
addLineNo :: Int -> TError -> TError
addLineNo line e = addError (lineNoError line) e
-- An error occurred when parsing a l.h.s. term as copattern.
parsingCopatternError :: TError
parsingCopatternError = "Parse error in copattern."
-- The analysed copattern is non-linear.
nonLinearCopatternError :: TError
nonLinearCopatternError = "Non-linear copattern (free variable used more than once in l.h.s.)."
-- Identifier symbol redefined in certain line.
symbolRedefinitionError :: Int -> String -> TError
symbolRedefinitionError line symbol = addLineNo line $ "Redefined \"" ++ symbol ++ "\" symbol."
-- Type symbol occurred and it's not defined as type synonym.
typeNotDefinedError :: Int -> Symbol -> TError
typeNotDefinedError line tSymbol = addLineNo line $ "Non-defined \"" ++ tSymbol ++ "\" type."
-- Type symbol it's defined more than once (distinct definitions occurres in line1 and line2).
typeRedefinitionError :: Int -> Int -> String -> TError
typeRedefinitionError line1 line2 tSymbol = addLineNo line2 $
  "Redefined \"" ++ tSymbol ++ "\" type (defined at line " ++ show line1 ++ ")."
{- A cycle occurred in the graph of type symbols synonym definitions.
  Example:
    A = C -> C
    B = A -> A
    A = B -> B
  The argument is the [(symbol, definition line number)] of the cycle and the
  head and tail of the list are equals.
-}
typeCycleDependenciesError :: [(Symbol,Int)] -> TError
typeCycleDependenciesError d = let ppcycle = concat .  intersperse " -> " . fst . unzip $ d
                                   pp (s,l) = "Type " ++ s ++ " defined at " ++ show l ++ "."
                                   ppdefs = concat . intersperse "\n" . map pp . tail  $ d
                               in "Circular type dependency: " ++ ppcycle ++ ".\n" ++ ppdefs
-- A symbol is declared twice.
duplicateSignatureError :: Int -> String -> TError
duplicateSignatureError line symbol = addLineNo line $ "Duplicate signature for \"" ++ symbol ++ "\"."
-- A symbol is defined (there is at least a rule defining it). But there is no respective type declaration.
symbolWithoutSignatureError :: Int -> Symbol -> TError
symbolWithoutSignatureError line symbol = addLineNo line $ "There is no declaration for symbol \"" ++ symbol ++ "\"."
-- A symbol is declared (there is a respective signature). But there is no definition rule involving it.
symbolWithoutDefinitionError :: Int -> Symbol -> TError
symbolWithoutDefinitionError line symbol = addLineNo line $ "Symbol \"" ++ symbol ++ "\" not defined."

-- DE ACA EN ADELANTE FALTAN EJEMPLOS

-- An error detected while type checking: symbol used in r.h.s. is not defined or ligated by l.h.s.
varSymbolNotFoundError :: Symbol -> TError
varSymbolNotFoundError symbol = "Symbol \"" ++ symbol ++ "\" is not defined in Program or ligated at l.h.s."
-- An error that occurrs when not found certain symbol in the signature of a Record or Data type.
typeSignatureSymbolError :: Symbol -> Type -> TError
typeSignatureSymbolError s ty = "The symbol \"" ++ s ++ "\" doesn't exist in the signature of type "
  ++ PP.render (printType ty)
-- Following error correspond to be expecting a type and find other (while type checking).
_typeCheckErrorExpected :: String -> Type -> TError
_typeCheckErrorExpected renderExpected ty = "Couldn't match type: \"" ++
  renderExpected ++ "\".\nWith type: \"" ++ PP.render (printType ty) ++ "\"."
typeCheckErrorFun, typeCheckErrorTuple, typeCheckErrorData, typeCheckErrorRecord :: Type -> TError
typeCheckErrorFun = _typeCheckErrorExpected "t1 -> t2"
typeCheckErrorTuple = _typeCheckErrorExpected "(t1, t2)"
typeCheckErrorData = _typeCheckErrorExpected "Data"
typeCheckErrorRecord = _typeCheckErrorExpected "Record"
typeCheckErrorUnit = _typeCheckErrorExpected "()"
-- Can not infer the type of t = Constructor t'. This is because we support overloading of constructors.
typeInferConstructorError :: TError
typeInferConstructorError = "Can not infer the type of a constructor term (ambiguity by constrcutor overloading)"
termDestroyableError  :: Term -> TError
termDestroyableError t = "Can not apply a destructor to the term: " ++ (PP.render . printTerm) t
termNotApplicableError :: Term -> TError
termNotApplicableError t = "Can not apply the term as a function: " ++ (PP.render . printTerm) t
-- The type checked if satisfy is not the one inferred.
typeInferredMismatchError :: Type -> Type -> TError
typeInferredMismatchError tyInferred tyChecked = "The type: \"" ++
  PP.render (printType tyInferred) ++ "\" doesn't match with type: \""
  ++ PP.render (printType tyChecked) ++ "\""

import Common
import TError
import Parse
import Syntax
import Typing
import BruteEval
import Eval
import PrettyPrinter
import Interpreter
import CopatternCovering

import qualified Text.PrettyPrint as PP
import Control.Monad.Except
import Control.Monad.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Lazy
import Data.Maybe

-- The internal state that will be provided to Interpreter is the loaded Program.
newtype CPState = CPState { loadedProgram :: Program }

{- Creates the instance of ProgramState which will tell Interpreter how interpret
   interactive commands and code files. -}
instance ProgramState CPState where
  interpretFileContent = interpretFileContent'
  interpretInteractive = interpretWithEval (\program -> quote . eval program)

-- We create the interpreter with some extra commands.
interpreter = addUserCommand ":clear" "" "Unload the program." (const . const $ (CPState emptyProgram, "Unloaded program!")) $
              addUserCommand ":show" "<symbol>" "Prints the symbol information (or all program info if no symbol was given)." browseComm $
              addUserCommand ":type" "<term>" "Try to infer the type of a term." typeComm $
              addUserCommand ":bruteval" "<exp>" ("Given an expression (term :: Type)"++
                " interpret it but use the BruteEval eval") (interpretWithEval reduceAll) $
              initialState "CP>" (CPState emptyProgram)

-- Our main runs the created interpreter above.
main = runInterpreter interpreter

-- Auxiliary function to convert ParseResult to Except TError.
parseWithExcept :: (String -> ParseResult a) -> String -> Except TError a
parseWithExcept parser txt = case parser txt of
  Failed e -> throwError ("Error durante el parseo.\n"++e)
  Ok pDefs -> return pDefs

{- A UserAction that parse an expression (Term : Type), type chek it, and
  evaluate it (using a parametrized eval method). -}
interpretWithEval :: (Program -> Term -> Term) -> UserAction CPState
interpretWithEval evalMethod command (CPState prog) =
  case runExcept (do TypedTerm t ty <- parseWithExcept parseEvalExp command
                     typeCheckTerm prog t ty
                     return t) of
    Right t -> (CPState prog,  PP.render . printTerm . evalMethod prog $ t)
    Left errMsg   -> (CPState prog, errMsg)

{- A UserAction that parse set of defs (ProgramDefs), type check it, and (if no
   error occurred) replace the programState with the new loaded Program.
   The last loaded Program is overwritten. -}
interpretFileContent' :: UserAction CPState
interpretFileContent' code (CPState prog) =
  case runExcept (do programDef <- parseWithExcept parseDefs code
                     program <- makeProgram programDef
                     typeProgram program
                     checkCoverage program -- EXPERIMENAL BORRAR
                     return program) of
    Right newprog -> (CPState newprog, "File loaded!")
    Left errMsg   -> (CPState prog, errMsg)

{- A UserAction that shows the info of a provided symbol (type synonym variable
   or defined symbol). If the argument is empty show all the program info. -}
browseComm :: UserAction CPState
browseComm symbol (CPState prog) =
  (CPState prog, if null symbol
                  then PP.render (printFullProgramInfo prog)
                  else case printSymbolInfo prog symbol of
                          Just doc -> PP.render doc
                          Nothing  -> ("The symbol " ++ symbol ++ " is not defined in the loaded program.") )

-- A UserAction that parse a Term, infer its Type (if possible) and print it.
typeComm :: UserAction CPState
typeComm command (CPState prog) =
  case runExcept (do t <- parseWithExcept parseTerm command
                     typeTerm prog t) of
    Right ty -> (CPState prog,  PP.render . printType $ ty)
    Left errMsg   -> (CPState prog, errMsg)

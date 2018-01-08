{-# OPTIONS -XRecordWildCards #-}
module Main where

  import Control.Exception (catch,IOException)
  import Control.Monad.Except
--  import Control.Monad.Error 
  import Data.Char
  import Data.List
  import Data.Maybe
  import Prelude hiding (print, catch)
  import System.Console.Readline
  import System.Environment
  import System.IO hiding (print)
  import Text.PrettyPrint.HughesPJ (render,text)

  import Common
  import Parse
  import Typing

  w :: IO ()
  w = do defs' <- parseIO "asdasd" aux "main main = main main () ()"
         (putStrLn . show) defs'

  u :: IO ()
  u = do defs' <- parseIO "asdasd" aux2 "main main main "
         (putStrLn . show) defs'

  e :: IO ()
  e = do textProgram <- readFile "test2"
         defs' <- parseIO "test2" defs_parse textProgram
         let Just defs = mapM procDef (fromJust defs')
         let Just (tEnv,ast) = typeFold [] [] defs
         (putStrLn . show) ast
         (putStrLn . show) tEnv
         x (Just ast)

  x ast = do let typeResult = maybe "Not Parsed" (\ast-> if isProgramTyped ast then 
                          "Bien Tipado" else "Mal Tipado :(") ast
             putStrLn typeResult
             let programRules =  rules (fromMaybe [] ast)
             putStrLn $ "Reglas: " ++ show programRules
             t <- computeAllIO programRules (Var "main")
             (putStrLn . show) t
             (putStrLn . pretty) t

  parseIO :: String -> (String -> ParseResult a) -> String -> IO (Maybe a)
  parseIO f p x = case p x of
                       Failed e  -> do putStrLn (f++": "++e) 
                                       return Nothing
                       Ok r      -> return (Just r)
         
   


 
 

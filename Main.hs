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

  e = do textProgram <- readFile "test"
         ast <- parseIO "test" defs_parse textProgram
         (putStrLn . show) ast
         let typeResult = maybe "Not Parsed" (\ast-> if isProgramTyped ast then 
                          "Bien Tipado" else "Mal Tipado :(") ast
         putStrLn typeResult
         let programRules =  rules (fromMaybe [] ast)
         putStrLn $ "Reglas: " ++ show programRules
         t <- computeAllIO programRules (Var "main")
         (putStrLn . show) t

  parseIO :: String -> (String -> ParseResult a) -> String -> IO (Maybe a)
  parseIO f p x = case p x of
                       Failed e  -> do putStrLn (f++": "++e) 
                                       return Nothing
                       Ok r      -> return (Just r)
         





 
 

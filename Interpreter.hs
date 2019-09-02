module Interpreter
( ProgramState(..)   {- Clase de tipos de estado de un intérprete. Debe definir
                        funciones que modifiquen el estado y emitan un output
                        para cuando se lea del intérprete (interpretInteractive)
                        o de un archivo cargado (interpretInteractive) -}
, addUserCommand     {- Agrega una comando al intérprete con una acción pura
                        sobre el estado del programa. -}
, initialState       {- Para construir un intérprete dado el promt y el estado
                        inicial del programa. -}
, runInterpreter     {- Acción IO que ejecuta el intérprete. -}
, UserAction         {- Type of the commands added by user (pure computation of the state of the program). -}
) where

import Control.Exception (catch,IOException)
import System.Console.Readline
import System.Environment
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Char
import Data.Maybe
import System.IO

{- Dado un comando (String) y un estado del programa, avanza a un nuevo estado
   y emite un output (String). -}
type UserAction programState = String -> programState -> (programState, String)

{- Clase de tipos de los estados de un intérprete. Debe implementar dos acciones
   fundamentales: la que procesa la entrada del intérprete interactivo
   (interpretInteractive) y la que procesa la entrada que se lee de un archivo
   cargado (interpretFileContent) -}
class ProgramState programState where
  interpretInteractive :: UserAction programState
  interpretFileContent :: UserAction programState

{- El estado interno de nuestro intérprete, parametrizado por el estado del
   programa. Contiene además información útil para la ejecucción básica del
   intérprete como la lista de comandos, ó el último archivo cargado. -}
data InterpreterState programState = InterpreterState {
                                        programState :: programState,
                                        lastFile :: String,
                                        iprompt :: String,
                                        commands :: [Command programState]
                                      }

{- Tipo de las computaciones: realizan acciones IO computando (quizás) un nuevo
   estado. Si no lo hacen (Nothing) indica que el programa ha de terminar luego
   de la computación. -}
type Computation programState = InterpreterState programState -> IO (Maybe (InterpreterState programState))

{- Las especificaciones de un comando:
    - prefix: es el prefijo utilizado para invocarlo en el intérprete
    - argsFormat: una String que detalla el formato de sus argumentos.
    - help: una String que provee ayuda de lo que hace el comando. -}
data CmdSpec = CmdSpec { prefix :: String,
                         argsFormat :: String,
                         help :: String }

{- Un comando está dado por sus especificaciones y el comando en sí: una
   computación parametrizada por el argumento que podría recibir del usuario -}
data Command programState = Cmd { cmdSpec :: CmdSpec,
                                  command :: String -> Computation programState }

{- Los comandos agregados por el usuario sólo admiten computaciones puras (no
   accioens IO) sobre el estado del programa. -}
data UserCommand programState = UserCmd { uCmdSpec :: CmdSpec,
                                          uCommand :: UserAction programState }

-- Para construir un InterpreterState a partir del estado inicial de nuestro programa.
initialState :: ProgramState programState => String -> programState -> InterpreterState programState
initialState prompt pState = InterpreterState { iprompt = prompt, lastFile = "", programState = pState, commands = defaultCmd }

{- Si tenemos un cómputo sobre el estado del programa lo aplica al estado
   guardado en el estado de un intérprete dado. -}
liftState :: (programState -> programState) -> InterpreterState programState -> InterpreterState programState
liftState f st = st { programState = f (programState st) }

-- Transforma un comando de usuario en uno que se pueda agregar a la lista de comandos de InterpreterState.
liftCommand :: UserCommand programState -> Command programState
liftCommand uComm = Cmd { cmdSpec = uCmdSpec uComm, command = c }
                    where c arg st = let (pSt, out) = (uCommand uComm) arg (programState st)
                                     in do putStrLn out
                                           (return . Just . liftState (\_ -> pSt)) st

-- Agrega un comando de usuario a la lista de comandos del intérprete.
addCommand :: UserCommand programState -> InterpreterState programState -> InterpreterState programState
addCommand uComm st = st { commands = (liftCommand uComm):(commands st) }

-- Función exportada que construye un UserCommand y agrega al intérprete.
addUserCommand prefix argFormat help userAction =
 let cmd = UserCmd (CmdSpec prefix argFormat help) userAction in addCommand cmd

-- Dada una lista de retorna un String con descripción de los comandos para imprimir como ayuda.
helpTxt :: [Command a] -> String
helpTxt commands =
  "Lista de comandos:\n\n" ++ (unlines . map showComm) strComms
      where showComm (strComm,help) = strComm ++ replicate ((24 - length strComm) `max` 2) ' ' ++ help
            strComms = noComm:[(prefix c ++ " " ++ argsFormat c, help c) | c <- map cmdSpec commands]
            noComm   = ("<expr>", "interpreta una expresión del lenguaje")

{- Lee el archivo que se le pasa como argumento e interpreta su contenido, usando
   interpretFileContent que debe definir la instancia de programState, avanzando
   el estado del programa (retorna Nothing si ocurrió un error y el estado no debe
   actualizarse). -}
interpretFile :: ProgramState programState =>  String -> programState -> IO (Maybe programState)
interpretFile file pSt = let f' = reverse . dropWhile isSpace . reverse $ file
                             openError e = "No se pudo abrir el archivo " ++ f' ++ ": " ++ show (e :: IOException)
                         in do str <- catch (readFile f') (\e -> hPutStr stderr (openError e) >> return "")
                               if null str then return Nothing
                               else do putStrLn $ "Cargando archivo " ++ f'
                                       let (nSt, outTxt) = interpretFileContent str pSt
                                       unless (null outTxt) (putStrLn outTxt)
                                       return (Just nSt)

-- Interpreta una secuencia de archivos.
interpretFiles :: ProgramState programState => [String] -> programState -> IO programState
interpretFiles files pSt = foldM loadF pSt files
                           where loadF s f = fmap (fromMaybe s) (interpretFile f s)

{- Lista de los comandos que todo intérprete definido posee. Abajo están las
   definiciones de las funciones de cada comando. -}
defaultCmd :: ProgramState programState => [Command programState]
defaultCmd = [ Cmd (CmdSpec ":load" "<file>" "Cargar e interpretar un archivo") loadFileA,
               Cmd (CmdSpec ":quit" "" "Salir del intérprete") endExecutionA,
               Cmd (CmdSpec ":reload" "" "Volver a cargar el último archivo") reloadFileA,
               Cmd (CmdSpec ":help" "" "Mostrar esta lista de comandos") printHelpA ]

-- Las funciones de los comandos toman un String y actualizan el estado del intérprete produciendo en una acción IO.
endExecutionA, printHelpA, loadFileA, reloadFileA, evalA :: ProgramState programState => String -> Computation programState
-- :quit retorna Nothing en la Computation indicando que el estado del interprete es terminar.
endExecutionA  = const . const $ putStrLn "Bye!" >> return Nothing
-- Imprimimos la ayuda de comandos y no modificamos el estado del interprete.
printHelpA _ st = (putStrLn . helpTxt . commands) st >> (return . Just) st
{- Cargamos un archivo modificando el estado del intérprete (se modifica el
   "last reloaded file") si no hay errores (por ejemplo que el archivo no exista). -}
loadFileA file st = interpretFile file (programState st) >>= return . Just . (maybe st stSuccess)
                    where stSuccess pSt = st { programState = pSt, lastFile = file }
-- Vuelve a cargar el último archivo que ha sido cargado.
reloadFileA _ st  = if null (lastFile st) then
                      putStrLn "No hay un archivo cargado." >> (return . Just) st
                      else loadFileA (lastFile st) st
{- Evalúa una expresión (String) usando interpretInteractive (definida por programState).
   Imprime la output String y avanza al nuevo estado del intérprete (avanzando el
   estado del programa según el nuevo estado devuelto por interpretInteractive) -}
evalA expr st = let (newState, output) = interpretInteractive expr (programState st)
                    st' = st {programState = newState}
                in putStrLn output >> (return . Just) st'

{- Dado un intérprete lo ejecuta.
   Se cargan los archivos que se hayan pasado como argumentos de consola.
   Se podrán introducir comandos que serán  interpretados interactivamente por
   interpretInteractive de programState.
   Además podemos cargar archivos con :load que serán interpretados por
   interpretFileContent de programState.
   Se pueden usar los demás comandos definidos (':help' para ver la ayuda). -}
runInterpreter :: ProgramState programState => InterpreterState programState -> IO ()
runInterpreter initState = do args <- getArgs
                              pSt  <- interpretFiles args (programState initState)
                              let initialState' = initState { programState = pSt }
                              interpreterLoop initialState'

{- Dada un String ve si hay algún comando del intérprete (los comandos tienen la
   forma de :comando). Si hay algún comando lo ejecuta.
   Sino se ejecutará la acción de evaluar la entrada como una expresión. -}
parseCommand :: ProgramState programState => String -> [Command programState] -> Either String (Computation programState)
parseCommand str cmds =
  if ":" `isPrefixOf` str then
    let (cmd, arg')    =  break isSpace str
        arg           =  dropWhile isSpace arg'
        matchedCmds  =  filter (isPrefixOf cmd . prefix . cmdSpec) cmds
    in case matchedCmds of
        []  -> Left  $ "Comando desconocido `" ++ cmd ++ "'. Escriba :help para recibir ayuda."
        [c] -> Right $ command c arg
        _   -> Left  $ "Comando ambigüo, podría ser " ++ (concat . intersperse ", " . map (prefix . cmdSpec) ) matchedCmds
  else Right $ evalA str

{- Loop principal, lee un comando, ejecuta las acciones correspondientes y
   modifica el estado (así sucesivamente). -}
interpreterLoop :: ProgramState programState => InterpreterState programState -> IO ()
interpreterLoop st = do input <- readline (iprompt st)
                        maybe (return ()) procInput input
                     where procInput strComm = do addHistory strComm
                                                  either procError procCommand (parseCommand strComm (commands st))
                           procError error = do putStrLn error
                                                interpreterLoop st
                           procCommand comm = comm st >>= maybe (return ()) interpreterLoop

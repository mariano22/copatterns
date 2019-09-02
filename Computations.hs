{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Computations where

import Common
import PrettyPrinter
import TError

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Control.Monad.Writer.Lazy


{- Class of monads: we will use different kinds of monads to represent non pure
   computations (e.g. with state or error).
   Depending on the functionality we will need certain effects (for example when
   analysing definitions l.h.s. we would need to ligate variables but we wouldn't
   need that when analysing r.h.s. terms).

   We created type classes for each functionality.
   Then we have the monads we will use and we create instances of the type classes
   for them.
   Finally the functions will have constraint depending the functionality they need -}

{- Monads type class
   ================= -}

{- Computation that might fail with TError and can catch a throw error and add
   to it the line number. -}
class Monad f => WithTError f where
    throwTError :: TError -> f a
    addMoreTError :: TError -> f a -> f a

-- Add a line error number if an error was throw.
addLineNoIfError :: WithTError f => Int -> f a -> f a
addLineNoIfError line e = addMoreTError (lineNoError line) e

-- Add the term which caused if an error was throw.
addExpToError :: (WithTError f, PPrintable p) => p -> f a -> f a
addExpToError ex e = addMoreTError (expresionErrorAt ex) e

-- Reader only access to the type definitions.
class Monad f => WithTypeDefContex f where
    getTypeSignature :: f TypeSignature

-- Read only access to type signatures.
class Monad f => WithTypeContex f where
    getTypeSymbol :: Symbol -> f (Def Type)

-- Generate a maping [(Symbol,v)] while doing the computation.
class WithYieldSubstitution f where
    addToSubstitution :: Symbol -> v -> f v ()

{- Monads type
   =========== -}

-- Computation that might fail with access to a global contexts.
newtype ErrorAndTypeContext a = ErrorAndTypeContext { getErrorAndTypeContext :: ReaderT TypeContext (Except TError) a }
                                deriving (Functor,Applicative,Monad)

-- Crete the global context given a Program and run an ErrorAndTypeContext computation.
runErrorAndTypeContext :: Program -> ErrorAndTypeContext a -> Except TError a
runErrorAndTypeContext (typeSignature, symbolDefs) m =
  let getSymbolToType (symbol, (sType, _)) = (symbol, sType)
      varSignature = VarSig . M.fromList . map getSymbolToType . M.toList $ symbolDefs
  in (runReaderT . getErrorAndTypeContext) m (varSignature, typeSignature)

-- Computation that might fail, has access to a global contexts and writes a substitution [(Symbol, a)].
newtype LHSEnv a b = LHSEnv { getLHSEnv :: WriterT [(Symbol, a)] ErrorAndTypeContext b } deriving (Functor,Applicative,Monad)

{- Monads type instances
   ===================== -}

instance WithTError ErrorAndTypeContext where
  throwTError e = ErrorAndTypeContext . lift . throwError $ e
  addMoreTError e = let addLineExcept :: Except TError a -> Except TError a
                        addLineExcept = flip catchError (\e' -> throwError (addError e e'))
                        addLineReader :: ReaderT TypeContext (Except TError) a -> ReaderT TypeContext (Except TError) a
                        addLineReader = mapReaderT addLineExcept
                        sandwich f = ErrorAndTypeContext . f . getErrorAndTypeContext
                    in sandwich addLineReader :: ErrorAndTypeContext a -> ErrorAndTypeContext a

-- Returns the TypeSignature in the ReaderT.
instance WithTypeDefContex ErrorAndTypeContext where
  getTypeSignature = ErrorAndTypeContext $ snd <$> ask

{- Asks ReaderT for the VarSignature and use it to returns the Type definition.
   Fails if doesn't find the symbol with a varSymbolNotFoundError. -}
instance WithTypeContex ErrorAndTypeContext where
 getTypeSymbol s = ErrorAndTypeContext $
   do (VarSig m, _) <- ask
      maybe (lift . throwError $ varSymbolNotFoundError s) return (M.lookup s m)

-- Use lift/mapWriterT to access ErrorAndTypeContext behind.
instance WithTError (LHSEnv a) where
  throwTError e = LHSEnv . lift . throwTError $ e
  addMoreTError e = LHSEnv . mapWriterT (addMoreTError e) . getLHSEnv

-- Use lift to access ErrorAndTypeContext behind.
instance WithTypeDefContex (LHSEnv a) where
    getTypeSignature = LHSEnv $ lift getTypeSignature

-- Use lift to access ErrorAndTypeContext behind.
instance WithTypeContex (LHSEnv a) where
    getTypeSymbol s = LHSEnv . lift . getTypeSymbol $ s

-- Writes the (s,v) assignation to the WriterT [(Symbol, a)] substitution.
instance WithYieldSubstitution LHSEnv where
    addToSubstitution s v = LHSEnv $ tell [(s,v)]

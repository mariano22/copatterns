module Typing
( typeProgram,   -- Checks an entire Program is well typed.
  typeCheckTerm, -- Type check a term given a program.
  typeTerm,      -- Infer the type of a term given a program.
) where
import Common
import Computations
import TError
import Utils

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Control.Monad.Writer.Lazy

{- Auxiliary function to expand a Type using the Type context (type synonym
   definitions) and check it satisfy certain predicate (generally we will checks
   if have a certain form i.e. is a function type, is a record type, etc).
   If doesn't fail throwing an error constructed by a callback error constructor. -}
expectType :: (WithTError m, WithTypeDefContex m) =>
              (Type -> Bool) -> (Type -> TError) -> Type -> m Type
expectType tyPred tyError ty =
 do tySig   <- getTypeSignature
    let ty' = expandTOnce tySig ty
    if tyPred ty' then return ty' else throwTError (tyError ty)

-- Expand a type if it's a type variable and return it by checking is a certain kind of type.
expectTyFun, expectTyRecord, expectTyTuple, expectTyData, expectTyUnit :: (WithTError m, WithTypeDefContex m) => Type -> m Type
expectTyFun    = expectType isFunTy typeCheckErrorFun
expectTyRecord = expectType isRecordTy typeCheckErrorRecord
expectTyTuple  = expectType isTupleTy typeCheckErrorTuple
expectTyData   = expectType isDataTy typeCheckErrorData
expectTyUnit   = expectType isUnitTy typeCheckErrorUnit

{- Check a Type is a record/data with a symbol in the signature.
   Returns the type of the symbol field with the correspondient substitution for 
   the ligated type variable -}
indexSignature :: WithTError m => Type -> Symbol -> m Type
indexSignature ty s = case getSignatureTy ty >>= M.lookup s of
  Nothing      -> throwTError $ typeSignatureSymbolError s ty
  Just tyField -> return (typeSubstitution ty 0 tyField)

-- Checks an entire Program is well typed.
typeProgram :: Program -> Except TError ()
typeProgram p@(_, s) = let ruleDefs :: [Def Rule]
                           ruleDefs = concat . map snd . M.elems $ s
                       in mapM_ (runErrorAndTypeContext p . typeRule) ruleDefs

-- Type check a term given a program.
typeCheckTerm :: Program -> Term -> Type -> Except TError ()
typeCheckTerm p t ty = runErrorAndTypeContext p (typeCheckTerm' t ty)
-- Infer the type of a term given a program.
typeTerm :: Program -> Term -> Except TError Type
typeTerm p t = runErrorAndTypeContext p (typeTerm' t)

-- Checks a Rule (q = t) is well typed.
typeRule :: Def Rule -> ErrorAndTypeContext ()
typeRule Def { getDefLineNo = line, getDef = Rule{ getCopattern = copattern, getTerm = term  } } =
  do (termType, symbolsTypes) <- runWriterT . getLHSEnv . addLineNoIfError line . typeCopattern $ copattern
     let symbolsNewDefs :: [ (Symbol, Def Type) ]
         symbolsNewDefs = map (\(s,ty) -> (s, Def { getDef = ty, getDefLineNo = line })) symbolsTypes
         addSymbolsTypes :: TypeContext -> TypeContext
         addSymbolsTypes (VarSig vs, ts) = (VarSig $ M.union (M.fromList symbolsNewDefs) vs, ts)
     ErrorAndTypeContext . withReaderT addSymbolsTypes . getErrorAndTypeContext . addLineNoIfError line $ (typeCheckTerm' term termType)

-- Check a copattern is well typed while yielding a assignation free vars to type.
typeCopattern :: Copattern -> LHSEnv Type Type
typeCopattern wT@(CHole s) = addExpToError wT $ getDef <$> getTypeSymbol s
typeCopattern (CApp q p) =
  do typeFun <- typeCopattern q
     (TyFun typeB typeC) <- addExpToError q $ expectTyFun typeFun
     typeCheckPattern p typeB
     return typeC
typeCopattern wT@(CDestructor d q) =
  do ty <- typeCopattern q
     tyExpanded <- addExpToError q $ expectTyRecord ty
     addExpToError wT $ indexSignature tyExpanded d

-- Check a pattern is well typed while yielding a assignation free vars to type.
typeCheckPattern :: Pattern -> Type -> LHSEnv Type ()
typeCheckPattern wT@(PVar s) t  = addExpToError wT $ addToSubstitution s t
typeCheckPattern PUnit ty = addExpToError PUnit $ void (expectTyUnit ty)
typeCheckPattern wT@(PTuple p1 p2) ty =
  do (TyTuple ty1 ty2) <- addExpToError wT $ expectTyTuple ty
     typeCheckPattern p1 ty1
     typeCheckPattern p2 ty2
typeCheckPattern wT@(PConstructor c p) ty =
  do tyExpanded <- addExpToError wT $ (expectTyData ty)
     tty <- addExpToError wT $ indexSignature tyExpanded c
     typeCheckPattern p tty

headCheck' :: (Term -> TError) -> Term -> ErrorAndTypeContext ()
headCheck' tyError = hC
  where hC (Var v)          = return ()
        hC (App t _)        = headCheckFunction t
        hC (Destructor d t) = headCheckDestructor t
        hC t                = addExpToError t $ throwTError (tyError t)

headCheckDestructor, headCheckFunction :: Term -> ErrorAndTypeContext ()
headCheckDestructor = headCheck' termDestroyableError
headCheckFunction   = headCheck' termNotApplicableError

-- Infer the Type of a term.
typeTerm' :: Term -> ErrorAndTypeContext Type
typeTerm' wT@(Var v) = addExpToError wT $ getDef <$> (getTypeSymbol v)
typeTerm' Unit = return TyUnit
typeTerm' (Tuple t1 t2) = liftM2 TyTuple (typeTerm' t1) (typeTerm' t2)
typeTerm' wT@(Constructor _ _) = addExpToError wT $ throwTError typeInferConstructorError
typeTerm' (App t1 t2) =
 do headCheckFunction t1
    typeFun <- typeTerm' t1
    (TyFun typeA1 typeA2) <- addExpToError t1 $ expectTyFun typeFun
    typeCheckTerm' t2 typeA1
    return typeA2
typeTerm' (Destructor d t) =
 do headCheckDestructor t
    tyRecord <- typeTerm' t
    tyExpanded <- addExpToError t $ expectTyRecord tyRecord
    addExpToError t $ indexSignature tyExpanded d

-- Check a term satisfy a type.
typeCheckTerm' :: Term -> Type -> ErrorAndTypeContext ()
typeCheckTerm' wT@(Tuple t1 t2) ty =
  do (TyTuple ty1 ty2) <- addExpToError wT $ expectTyTuple ty
     typeCheckTerm' t1 ty1
     typeCheckTerm' t2 ty2
typeCheckTerm' wT@(Constructor c t) ty =
  do tyExpanded <- addExpToError wT $ expectTyData ty
     tty <- addExpToError wT $ indexSignature tyExpanded c
     typeCheckTerm' t tty
typeCheckTerm' t ty =
  do tyInf <- typeTerm' t
     tySig <- getTypeSignature
     unless (expandT tySig ty == expandT tySig tyInf)
            (addExpToError t $ throwTError (typeInferredMismatchError tyInf ty))

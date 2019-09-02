module Utils where
import Common
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe

{- Returns the free type variable given a type (respective to a alredy defined type). -}
fvType :: Type -> [Symbol]
fvType ty = case ty of
  TyVar s -> [s]
  TyBound _ -> []
  TyUnit -> []
  TyTuple t1 t2 -> fvType t1 ++ fvType t2
  TyFun t1 t2 -> fvType t1 ++ fvType t2
  TyData sig -> fvTypeSignature sig
  TyRecord sig -> fvTypeSignature sig
  where fvTypeSignature sig = concat . map (fvType . snd) . M.toList $ sig


{- Ligates a type variable symbol in a type.
   Records and Data are recursive types. This recursion is implemented by ligated
   type variables to make a self reference. -}
ligate :: Symbol -> Type -> Type
ligate s = ligate' 0
  where ligate' d (TyVar s') = if s==s' then TyBound d else TyVar s'
        ligate' d (TyBound i) = TyBound i
        ligate' d (TyUnit) = TyUnit
        ligate' d (TyTuple t1 t2) = TyTuple (ligate' d t1) (ligate' d t2)
        ligate' d (TyData sig) = TyData $ M.map (ligate' (d+1)) sig
        ligate' d (TyFun t1 t2) = TyFun (ligate' d t1) (ligate' d t2)
        ligate' d (TyRecord sig) = TyRecord $ M.map (ligate' (d+1)) sig

{- We implements naturals within the language as a Data Type with Zero and Succ
   constructors. We allow decimal notation (0, 1, 2 ..) as syntax sugar of it. -}
makeIntTerm :: Int -> Term
makeIntTerm n = iterate (Constructor "Succ") (Constructor "Zero" Unit) !! n

{- Given a list of defined symbols returns the line of a symbol if it's defined. -}
getDefinitionLine :: [ Def Symbol ] -> Symbol -> Maybe Int
getDefinitionLine ds s = getDefLineNo <$> find ((== s) . getDef) ds

-- Helper functions to add definition to a program set of definitions.
addTypeDef :: Def TypeDef -> ProgramDefs -> ProgramDefs
addTypeDef x p = p { typeDefs = x:typeDefs p }

addSignatureDef :: Def SignatureDef -> ProgramDefs -> ProgramDefs
addSignatureDef x p = p { signatureDefs = x:signatureDefs p }

addTermDef :: Def TermDef -> ProgramDefs -> ProgramDefs
addTermDef x p = p { termDefs = x:termDefs p }

{- Returns True iff a copattern is linear (free l.h.s. variables must appear
  exactly once). Example of non-linear: f x x = x + x
  To be well formed a copattern must be linear. -}
isLinear :: Copattern -> Bool
isLinear =  allUnique . fvCopattern
  where fvCopattern (CApp q p) = fvCopattern q ++ fvPattern p
        fvCopattern (CDestructor _ q) = fvCopattern q
        fvCopattern (CHole _) = []
        fvPattern (PVar x) = [x]
        fvPattern (PTuple p1 p2) = fvPattern p1 ++ fvPattern p2
        fvPattern (PConstructor _ p) = fvPattern p
        fvPattern  PUnit = []
        allUnique xs = (length . nub) xs == length xs

-- Given a list of Ord elements, returns a repeated element or Nothing.
duplicateElem :: Ord a => [a] -> Maybe a
duplicateElem ds = either (const Nothing) Just (foldl f (Left S.empty) ds)
  where f (Left s) x = if S.member x s then Right x else Left (S.insert x s)
        f x _ = x

{- Implementes the T[A/X] substitution of ligated type symbols by types necesary
   for the existence of recursive data types.
   Types symbols are ligated by TyData and TyRecord. We have eliminated the
   type ligated symbols and using Brujin indices instead (TyBound i) indicates
   it's a reference to the TyData/TyRecord that it's at distance i counting only
   TyData/TyRecord in the ancestors of the AST. -}
typeSubstitution :: Type -> Int -> Type -> Type
typeSubstitution t d = tS
  where tS (TyVar s)       = TyVar s
        tS (TyBound i)     = if d==i then t else (TyBound i)
        tS TyUnit          = TyUnit
        tS (TyTuple t1 t2) = TyTuple (tS t1) (tS t2)
        tS (TyFun t1 t2)   = TyFun (tS t1) (tS t2)
        tS (TyData sig)    = TyData $ M.map (typeSubstitution t (d+1)) sig
        tS (TyRecord sig)  = TyRecord $ M.map (typeSubstitution t (d+1)) sig

-- Apply a Substitution of symbols by Term.
termSubstitution :: Substitution -> Term -> Term
termSubstitution s = sub
  where sub t@(Var x) = fromMaybe t (lookup x s)
        sub Unit = Unit
        sub (Constructor c t) = Constructor c (sub t)
        sub (App t1 t2) = App (sub t1) (sub t2)
        sub (Tuple t1 t2) = Tuple (sub t1) (sub t2)
        sub (Destructor d t) = Destructor d (sub t)

{- Given the type synonyms definitions, expand a Type (recursively substituting
   type synonym symbols for their definitions) so there is no free type symbols.
   Useful for example, to compare Type for equality.
   If not a naive == comparision will be False for a Type and its synonym.
   Assume all the types are defined. -}
expandT :: TypeSignature -> Type -> Type
expandT (TypeSig tySig) = expandT'
  where expandT' (TyVar s) = expandT' (getDef (tySig M.! s)) -- We assume all used type symbols are defined so no problem here!
        expandT' (TyBound i) =  TyBound i
        expandT' TyUnit = TyUnit
        expandT' (TyTuple t1 t2) = TyTuple (expandT' t1) (expandT' t2)
        expandT' (TyFun t1 t2) = TyFun (expandT' t1) (expandT' t2)
        expandT' (TyRecord sig) = TyRecord $ M.map expandT' sig
        expandT' (TyData sig) = TyData $  M.map expandT' sig

{- If the given Type is a free type symbol replace it for its synonym.
   Assume all the types are defined. -}
expandTOnce :: TypeSignature -> Type -> Type
expandTOnce (TypeSig tySig) (TyVar s) = getDef (tySig M.! s)
expandTOnce _ x = x

-- Get type signature if is a record o data type.
getSignatureTy :: Type -> Maybe (M.Map Symbol Type)
getSignatureTy (TyRecord sig) = Just sig
getSignatureTy (TyData sig) = Just sig
getSignatureTy _ = Nothing

-- Check for certain kind of types.
isUnitTy, isTupleTy, isFunTy, isRecordTy, isDataTy  :: Type -> Bool
isFunTy (TyFun _ _)     = True
isFunTy _               = False
isRecordTy (TyRecord _) = True
isRecordTy _            = False
isTupleTy (TyTuple _ _) = True
isTupleTy _             = False
isDataTy (TyData _)     = True
isDataTy _              = False
isUnitTy TyUnit         = True
isUnitTy _              = False

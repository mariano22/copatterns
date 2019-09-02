{
module Parse
( parseDefs,     -- Parser for ProgramDefs (load program code from a file).
  parseEvalExp,  -- Parser for TypedTerm (to be evaluated interactively).
  parseTerm,     -- Parser for Term (to be type inferred).
  ParseResult(..)
) where
import Common
import Utils
import Data.Maybe
import Data.Char
import qualified Data.Map as M

}

%monad { P } { thenP } { returnP }
%name parseDefsP Defs
%name parseEvalExpP EvalExp
%name parseTermP Term
%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='                       { TEquals    }
    ':'                       { TColon     }
    '('                       { TParenOpen }
    ')'                       { TParenClose     }
    '{'                       { TBraceOpen }
    '}'                       { TBraceClose     }
    '>'                       { TGreater     }
    '<'                       { TLower     }
    '|'                       { TVerticalSeparator     }
    ','                       { TComma     }
    '->'                      { TArrow     }
    LOWERCASEIDENTIFIER       { TLowercaseIdentifier $$    }
    UPPERCASEIDENTIFIER       { TUppercaseIdentifier $$    }
    DESTRUCTORIDENTIFIER      { TDestructor $$    }
    RECORD                    { TRecord     }
    DATA                      { TData     }
    UNIT                      { TUnit }
    INT                       { TInt $$}
    ENDLINE                   { TEndLine }

%nonassoc '='
%nonassoc LOWERCASEIDENTIFIER UNIT INT '('
%right '->'

%%

Defs    ::                                                            { ProgramDefs }
        : lineno TypeDef Defs                                         { addTypeDef (Def { getDefLineNo = $1, getDef = $2 }) $3 }
        | lineno SignatureDef Defs                                    { addSignatureDef (Def { getDefLineNo = $1, getDef = $2 }) $3 }
        | lineno TermDef Defs                                         { addTermDef (Def { getDefLineNo = $1, getDef = $2 }) $3 }
        | ENDLINE Defs                                                { $2 }
        | {- empty -}                                                 { ProgramDefs [] [] [] }

TypeDef     ::                                                        { TypeDef }
            : UPPERCASEIDENTIFIER '=' Type                            { TypeDef { getTypeVar = $1, getType = $3 } }

SignatureDef ::                                                       { SignatureDef }
             : LOWERCASEIDENTIFIER ':' Type                           { SignatureDef { getSymbol = $1, getSymbolType = $3 } }

TermDef     ::                                                        { TermDef }
            : Term '=' Term                                           { TermDef { lhs = $1, rhs = $3 } }

Term ::                                                               { Term }
     : AtomicTerm                                                     { $1 }
     | Term AtomicTerm                                                { App $1 $2 }
     | UPPERCASEIDENTIFIER AtomicTerm                                 { Constructor $1 $2  }
     | DESTRUCTORIDENTIFIER AtomicTerm                                { Destructor $1 $2 }

AtomicTerm        ::                                                  { Term }
 		              : LOWERCASEIDENTIFIER                               { Var $1 }
 		              | UNIT                                              { Unit }
 		              | '(' Term ',' Term ')'                             { Tuple $2 $4 }
 		              | '(' Term ')'                                      { $2 }
                  | INT                                               { makeIntTerm $1 }

Type    ::                                                            { Type           }
        : UPPERCASEIDENTIFIER                                         { TyVar $1       }
        | UNIT                                                        { TyUnit         }
        | '(' Type ',' Type ')'                                       { TyTuple $2 $4  }
        | DATA '(' UPPERCASEIDENTIFIER ')' '<' DataTypeList '>'       { TyData $ M.map (ligate $3) $6 }
        | Type '->' Type                                              { TyFun $1 $3    }
        | RECORD '(' UPPERCASEIDENTIFIER ')' '{' RecordTypeList '}'   { TyRecord $ M.map (ligate $3) $6 }
        | '(' Type ')'                                                { $2              }

DataTypeList ::                                                       { M.Map Symbol Type }
             : UPPERCASEIDENTIFIER Type '|' DataTypeList          { M.insert $1 $2 $4 }
             | UPPERCASEIDENTIFIER Type                           { M.singleton $1 $2 }

RecordTypeList ::                                                     { M.Map Symbol Type }
               : DESTRUCTORIDENTIFIER ':' Type '|' RecordTypeList     { M.insert $1 $3 $5 }
               | DESTRUCTORIDENTIFIER ':' Type                        { M.singleton $1 $3 }

EvalExp ::                                                       { TypedTerm }
        : Term ':' Type                                          { TypedTerm $1 $3 }

lineno ::                                                             { LineNumber }
        : {- empty -}                                                 {% getLineNo }

{

data ParseResult a = Ok a | Failed String
                     deriving Show
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed $ "Línea " ++ show l ++ ": " ++ err ++ "\n" ++ s

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = failP "Error de parseo"

data Token =   TEquals
             | TColon
             | TParenOpen
             | TParenClose
             | TBraceOpen
             | TBraceClose
             | TGreater
             | TLower
             | TVerticalSeparator
             | TComma
             | TArrow
             | TLowercaseIdentifier String
             | TUppercaseIdentifier String
             | TDestructor String
             | TRecord
             | TData
             | TUnit
             | TInt Int
             | TEndLine
             | TEOF
             deriving Show

lexer :: (Token -> P a) -> P a
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':cs)  ->  \line -> cont TEndLine cs (line + 1)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> \line -> lexerComment 0 cont cs line
                    ('-':('}':cs)) -> failP "Comentario no abierto" s
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexerIdentifiers cont (c:cs)
                          | isDigit c -> lexerInt cont (c:cs)
                    ('.':cs) -> let (var, rest) = span (\x-> isAlpha x || x=='_' || isDigit x) cs
                                in cont (TDestructor ('.':var)) rest
                    ('-':('>':cs)) -> cont TArrow cs
                    ('(':(')':cs)) -> cont TUnit cs
                    ('=':cs) -> cont TEquals cs
                    (':':cs) -> cont TColon cs
                    ('(':cs) -> cont TParenOpen cs
                    (')':cs) -> cont TParenClose cs
                    ('{':cs) -> cont TBraceOpen cs
                    ('}':cs) -> cont TBraceClose cs
                    ('>':cs) -> cont TGreater cs
                    ('<':cs) -> cont TLower cs
                    ('|':cs) -> cont TVerticalSeparator cs
                    (',':cs) -> cont TComma cs
                    unknown -> failP "Secuencia de caracteres no soportada." s

lexerComment :: Int -> (Token -> P a) -> P a
lexerComment deep cont s = case s of
    ('-':('-':cs)) -> lexerComment deep cont $ dropWhile ((/=) '\n') cs
    ('{':('-':cs)) -> lexerComment (deep+1) cont cs
    ('-':('}':cs)) -> if deep==0 then lexer cont cs
                      else lexerComment (deep-1) cont cs
    ('\n':cs) -> \line -> lexerComment deep cont cs (line+1)
    (_:cs) -> lexerComment deep cont cs

lexerInt :: (Token -> P a) -> P a
lexerInt cont s = let (intString, cs) = span isDigit s
                      i = read intString :: Int
                  in cont (TInt i) cs

lexerIdentifiers :: (Token -> P a) -> P a
lexerIdentifiers cont s = case span (\x-> isAlpha x || x=='_' || isDigit x) s of
    ("Data", cs) -> cont TData cs
    ("Record", cs) -> cont TRecord cs
    (var, cs)      -> let t = if isUpper (head var)
                                  then TUppercaseIdentifier var
                                  else TLowercaseIdentifier var
                      in cont t cs

parseDefs s = parseDefsP s 1
parseEvalExp s = parseEvalExpP s 1
parseTerm s = parseTermP s 1
}

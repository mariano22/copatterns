{
module Parse where
import Common
import Data.Maybe
import Data.Char
import qualified Data.Map as M

}

%monad { P } { thenP } { returnP }
%name parseType Type
%name parseRule Rule
%name parseRules Rules
%name parseDefs Defs

%name aux_ Rule
%name aux_2 Term

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='       { TEquals    }
    ':'       { TColon     }
    '('       { TParenOpen }
    ')'       { TParenClose     }
    '{'       { TBraceOpen }
    '}'       { TBraceClose     }
    '>'       { TGreater     }
    '<'       { TLower     }
    '|'       { TSeparator     }
    ','       { TComma     }
    '->'      { TArrow     }
    NAT       { TNat $$    }
    IDENTIFIER       { TIdentifier $$    }
    UPPERCASEIDENTIFIER       { TUppercaseIdentifier $$    }
    DESTRUCTORIDENTIFIER       { TDestructor $$    }
    RECORD     { TRecord     }
    DATA     { TData     }
    UNIT { TUnit }
    ENDLINEBLOCK { TEndlineBlock }
    ENDLINESIMPLE { TEndlineSimple }
    '1+' { TPrePlus }
    '+1' { TPostPlus }

%nonassoc '='    
%nonassoc IDENTIFIER
%right '->'

%%

Defs    :: { [Def'] }
         :  RDef Defs           { $1 : $2 }
         |  TDef Defs           { $1 : $2 }
         |  TDef ENDLINESIMPLE Defs           { $1 : $3 }
         |  ENDLINEBLOCK Defs { $2 }
         | {[]} 


RDef     :: { Def' }            
         :  IDENTIFIER ':' Type Rules { RuleDef $1 $3 $4 }

TDef     :: { Def' }            
         :  UPPERCASEIDENTIFIER '=' Type { TypeDef $1 $3 }

Type    :: { VType }
        : UPPERCASEIDENTIFIER { VTyVar $1 }
        | UNIT                      { VTyUnit }
        | '(' Type ',' Type ')'                 { VTyTuple $2 $4 }
        | DATA '(' UPPERCASEIDENTIFIER ')' '<' DataTypeList '>' { VTyData $3 $6 }
        | Type '->' Type               { VTyFun $1 $3 }
        | RECORD '(' UPPERCASEIDENTIFIER ')' '{' RecordTypeList '}' { VTyRecord $3 $6 }
        | '(' Type ')'                 { $2 }

DataTypeList :: { M.Map Label VType }
             : UPPERCASEIDENTIFIER ':' Type '|' DataTypeList  { M.insert $1 $3 $5 }
             | UPPERCASEIDENTIFIER ':' Type { M.singleton $1 $3 }
             | { M.empty }

RecordTypeList :: { M.Map Label VType }
               : DESTRUCTORIDENTIFIER ':' Type '|' RecordTypeList  { M.insert $1 $3 $5 }
               | DESTRUCTORIDENTIFIER ':' Type { M.singleton $1 $3 }
               | { M.empty }

Rules :: { [(Term,Term)] }
      : ENDLINESIMPLE Rule Rules { $2 : $3 }
      | {[]}

Rule  :: { (Term,Term) }
      : Term '=' Term { ($1,$3) }


AtomicTerm         :: { Term }
		   : IDENTIFIER { Var $1 }
		   | UNIT { Unit }
		   | '(' Term ',' Term ')' { Tuple $2 $4 }
		   | '(' Term ')' { $2 }
           | NAT { natToTerm $1 }


Term :: {Term}
     : AtomicTerm { $1 }
     | Term AtomicTerm { App $1 $2 }
     | UPPERCASEIDENTIFIER AtomicTerm { Constructor $1 $2  }
     | DESTRUCTORIDENTIFIER AtomicTerm { Destructor $1 $2 }
     | AtomicTerm '+1' { Constructor "Succ" $1 }
     | '+1' AtomicTerm { Constructor "Succ" $2 }

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
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token =   TEquals 
             | TColon
             | TParenOpen
             | TParenClose
             | TBraceOpen
             | TBraceClose
             | TGreater
             | TLower
             | TSeparator
             | TComma
             | TArrow
             | TNat Int
             | TIdentifier String
             | TUppercaseIdentifier String
             | TDestructor String
             | TRecord
             | TData
             | TUnit
             | TEOF
             | TEndlineBlock
             | TEndlineSimple
             | TPostPlus
             | TPrePlus
             deriving Show


----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':cs) ->   dropSpaces 1 cs

                    ('+':('1':cs)) -> cont TPostPlus cs
                    ('1':('+':cs)) -> cont TPrePlus cs

                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexIdentifiers (c:cs)
                          | isDigit c -> lexNat (c:cs)
                    ('.':cs) -> let (var,rest) = span isAlpha cs in cont (TDestructor ('.':var)) rest

                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"


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
                    ('|':cs) -> cont TSeparator cs
                    (',':cs) -> cont TComma cs

                    unknown -> \line -> Failed $ "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexIdentifiers cs = case span (\x-> isAlpha x || x=='_' || isDigit x) cs of
                                                    ("Data",rest) -> cont TData rest
                                                    ("Record",rest) -> cont TRecord rest
                                                    (var,rest)   -> cont ((if isUpper (head cs) then TUppercaseIdentifier else TIdentifier) var) rest
                          lexNat cs = let (nat,rest) = span isDigit cs in cont (TNat (read nat)) rest
                          consumirBK anidado cl cont s = case s of
                                                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
		                                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
		                                              ('-':('}':cs)) -> case anidado of
		                                                                     0 -> \line -> lexer cont cs (line+cl)
		                                                                     _ -> consumirBK (anidado-1) cl cont cs
	                                                      ('\n':cs) -> consumirBK anidado (cl+1) cont cs
	                                                      (_:cs) -> consumirBK anidado cl cont cs
                          dropSpaces x ('\n':cs) = dropSpaces (x+1) cs
                          dropSpaces 1 [] = \line -> cont TEndlineBlock [] (line+1)
                          dropSpaces 1 ds = \line -> cont TEndlineSimple ds (line+1)
                          dropSpaces x ds = \line -> cont TEndlineBlock ds (line+x)

    
type_parse s = parseType s 1
rule_parse s = parseRule s 1
rules_parse s = parseRules s 1
defs_parse s = parseDefs s 1

aux s = aux_ s 1
aux2 s = aux_2 s 1
}

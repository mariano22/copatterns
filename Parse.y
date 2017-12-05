{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseType Type
%name parseRule Rule
%name parseRules Rules
%name parseDefs Defs
%name parseCopattern Copattern

%name aux_ Rules

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='       { TEquals    }
    ':'       { TColon     }
    '-'       { TDash      }
    '('       { TParenOpen }
    ')'       { TParenClose     }
    '{'       { TBraceOpen }
    '}'       { TBraceClose     }
    '>'       { TGreater     }
    '<'       { TLower     }
    '|'       { TSeparator     }
    ','       { TComma     }
    '->'      { TArrow     }
    IDENTIFIER       { TIdentifier $$    }
    UPPERCASEIDENTIFIER       { TUppercaseIdentifier $$    }
    DESTRUCTORIDENTIFIER       { TDestructor $$    }
    RULES     { TRules     }
    RECORD     { TRecord     }
    DATA     { TData     }
    UNIT { TUnit }

    
%right IDENTIFIER
%right UPPERCASEIDENTIFIER
%left '=' 
%right '->'

%%

Defs    :: { [Def] }
         :  Def Defs           { $1 : $2 }
         | {[]} 

Def     :: { Def }            
         :  RULES '(' IDENTIFIER ':' Type ')' '=' '{' Rules '}' { ($3, ($5, $9)) }

Type    :: { Type }
        : UPPERCASEIDENTIFIER { TyVar $1 }
        | UNIT                      { TyUnit }
        | '(' Type ',' Type ')'                 { TyTuple $2 $4 }
        | DATA '(' UPPERCASEIDENTIFIER ')' '<' DataTypeList '>' { TyData $3 $6 }
        | Type '->' Type               { TyFun $1 $3 }
        | RECORD '(' UPPERCASEIDENTIFIER ')' '{' RecordTypeList '}' { TyRecord $3 $6 }
        | '(' Type ')'                 { $2 }

DataTypeList :: { LabeledTypes }
             : UPPERCASEIDENTIFIER ':' Type '|' DataTypeList  { ($1,$3) : $5 }
             | UPPERCASEIDENTIFIER ':' Type { [($1,$3)] }
             | {[]}

RecordTypeList :: { LabeledTypes }
               : DESTRUCTORIDENTIFIER ':' Type '|' RecordTypeList  { ($1,$3) : $5 }
               | DESTRUCTORIDENTIFIER ':' Type { [($1,$3)] }
               | {[]}

Rules :: { [Rule] }
      : Rule Rules { $1 : $2 }
      | {[]}

Rule  :: { (Copattern,Term) }
      : Copattern '=' Term { ($1,$3) }

Pattern  :: { Pattern }
         : IDENTIFIER { PVar $1 }
         | UNIT { PUnit }
         | '(' Pattern ',' Pattern ')' { PTuple $2 $4 }
         | UPPERCASEIDENTIFIER Pattern { PConstructor $1 $2 }

Copattern  :: { Copattern }
           : '-' { Hole }
           | Copattern Pattern { CApp $1 $2 }
           | Copattern DESTRUCTORIDENTIFIER { CDestructor $2 $1 }

AtomicTerm         :: { Term }
		   : IDENTIFIER { Var $1 }
		   | UNIT { Unit }
		   | '(' Term ',' Term ')' { Tuple $2 $4 }
		   | '(' Term ')' { $2 }


Term :: {Term}
     : AtomicTerm { $1 }
     | Term AtomicTerm { App $1 $2 }
     | UPPERCASEIDENTIFIER AtomicTerm { Constructor $1 $2  }
     | Term DESTRUCTORIDENTIFIER { Destructor $2 $1 }

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
             | TDash
             | TParenOpen
             | TParenClose
             | TBraceOpen
             | TBraceClose
             | TGreater
             | TLower
             | TSeparator
             | TComma
             | TArrow
             | TIdentifier String
             | TUppercaseIdentifier String
             | TDestructor String
             | TRules
             | TRecord
             | TData
             | TNatType
             | TNatural Integer
             | TUnit
             | TEOF
             deriving Show


----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexIdentifiers (c:cs)
                    ('.':cs) -> let (var,rest) = span isAlpha cs in cont (TDestructor ('.':var)) rest

                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"

                    ('-':('>':cs)) -> cont TArrow cs
                    ('(':(')':cs)) -> cont TUnit cs
                    ('=':cs) -> cont TEquals cs
                    (':':cs) -> cont TColon cs
                    ('-':cs) -> cont TDash cs
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
                              	          	    ("Rules",rest)   -> cont TRules rest
                                                    ("Data",rest) -> cont TData rest
                                                    ("Record",rest) -> cont TRecord rest
                                                    (var,rest)   -> cont ((if isUpper (head cs) then TUppercaseIdentifier else TIdentifier) var) rest
                          consumirBK anidado cl cont s = case s of
                                                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
		                                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
		                                              ('-':('}':cs)) -> case anidado of
		                                                                     0 -> \line -> lexer cont cs (line+cl)
		                                                                     _ -> consumirBK (anidado-1) cl cont cs
	                                                      ('\n':cs) -> consumirBK anidado (cl+1) cont cs
	                                                      (_:cs) -> consumirBK anidado cl cont cs

    
type_parse s = parseType s 1
rule_parse s = parseRule s 1
rules_parse s = parseRules s 1
defs_parse s = parseDefs s 1
copattern_parse s = parseCopattern s 1

aux s = aux_ s 1
}

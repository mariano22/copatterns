{-# OPTIONS_GHC -w #-}
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
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn6 (ProgramDefs)
	| HappyAbsSyn7 (TypeDef)
	| HappyAbsSyn8 (SignatureDef)
	| HappyAbsSyn9 (TermDef)
	| HappyAbsSyn10 (Term)
	| HappyAbsSyn12 (Type)
	| HappyAbsSyn13 (M.Map Symbol Type)
	| HappyAbsSyn15 (TypedTerm)
	| HappyAbsSyn16 (LineNumber)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77 :: () => Int -> ({-HappyReduction (P) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32 :: () => ({-HappyReduction (P) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,156) ([0,0,4,32832,51,1024,824,0,0,0,4096,0,32832,48,0,0,16384,13184,0,0,0,32832,48,1024,776,0,0,0,0,0,32864,48,0,0,0,0,0,14340,3,0,64,0,0,0,16384,0,0,4,0,64,1280,776,8192,0,0,2053,3,0,0,1024,464,0,0,0,0,0,41152,48,256,0,16384,7424,0,0,0,32832,51,0,4,16384,7424,0,0,0,64,0,1024,0,0,0,0,53252,1,32832,51,0,0,0,0,0,0,0,32832,48,0,4,0,256,0,4096,0,24704,0,1024,464,49152,12416,0,1024,0,0,0,0,4,0,0,0,53252,1,128,0,2048,0,0,8,0,16,0,16512,0,0,0,0,512,0,4096,0,1024,0,1024,464,0,2,0,2,0,64,29,0,0,0,80,0,0,0,0,1,0,5,0,512,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseDefsP","%start_parseEvalExpP","%start_parseTermP","Defs","TypeDef","SignatureDef","TermDef","Term","AtomicTerm","Type","DataTypeList","RecordTypeList","EvalExp","lineno","'='","':'","'('","')'","'{'","'}'","'>'","'<'","'|'","','","'->'","LOWERCASEIDENTIFIER","UPPERCASEIDENTIFIER","DESTRUCTORIDENTIFIER","RECORD","DATA","UNIT","INT","ENDLINE","%eof"]
        bit_start = st * 36
        bit_end = (st + 1) * 36
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..35]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (35) = happyShift action_17
action_0 (36) = happyReduce_7
action_0 (6) = happyGoto action_15
action_0 (16) = happyGoto action_16
action_0 _ = happyReduce_32

action_1 (19) = happyShift action_7
action_1 (28) = happyShift action_8
action_1 (29) = happyShift action_9
action_1 (30) = happyShift action_10
action_1 (33) = happyShift action_11
action_1 (34) = happyShift action_12
action_1 (10) = happyGoto action_13
action_1 (11) = happyGoto action_6
action_1 (15) = happyGoto action_14
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (19) = happyShift action_7
action_2 (28) = happyShift action_8
action_2 (29) = happyShift action_9
action_2 (30) = happyShift action_10
action_2 (33) = happyShift action_11
action_2 (34) = happyShift action_12
action_2 (10) = happyGoto action_5
action_2 (11) = happyGoto action_6
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (16) = happyGoto action_4
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (29) = happyShift action_30
action_4 (7) = happyGoto action_19
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (19) = happyShift action_7
action_5 (28) = happyShift action_8
action_5 (33) = happyShift action_11
action_5 (34) = happyShift action_12
action_5 (36) = happyAccept
action_5 (11) = happyGoto action_25
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_11

action_7 (19) = happyShift action_7
action_7 (28) = happyShift action_8
action_7 (29) = happyShift action_9
action_7 (30) = happyShift action_10
action_7 (33) = happyShift action_11
action_7 (34) = happyShift action_12
action_7 (10) = happyGoto action_29
action_7 (11) = happyGoto action_6
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_15

action_9 (19) = happyShift action_7
action_9 (28) = happyShift action_8
action_9 (33) = happyShift action_11
action_9 (34) = happyShift action_12
action_9 (11) = happyGoto action_28
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (19) = happyShift action_7
action_10 (28) = happyShift action_8
action_10 (33) = happyShift action_11
action_10 (34) = happyShift action_12
action_10 (11) = happyGoto action_27
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_16

action_12 _ = happyReduce_19

action_13 (18) = happyShift action_26
action_13 (19) = happyShift action_7
action_13 (28) = happyShift action_8
action_13 (33) = happyShift action_11
action_13 (34) = happyShift action_12
action_13 (11) = happyGoto action_25
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (36) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (36) = happyAccept
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (19) = happyShift action_7
action_16 (28) = happyShift action_23
action_16 (29) = happyShift action_24
action_16 (30) = happyShift action_10
action_16 (33) = happyShift action_11
action_16 (34) = happyShift action_12
action_16 (7) = happyGoto action_19
action_16 (8) = happyGoto action_20
action_16 (9) = happyGoto action_21
action_16 (10) = happyGoto action_22
action_16 (11) = happyGoto action_6
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (35) = happyShift action_17
action_17 (36) = happyReduce_7
action_17 (6) = happyGoto action_18
action_17 (16) = happyGoto action_16
action_17 _ = happyReduce_32

action_18 _ = happyReduce_6

action_19 (35) = happyShift action_17
action_19 (36) = happyReduce_7
action_19 (6) = happyGoto action_44
action_19 (16) = happyGoto action_16
action_19 _ = happyReduce_32

action_20 (35) = happyShift action_17
action_20 (36) = happyReduce_7
action_20 (6) = happyGoto action_43
action_20 (16) = happyGoto action_16
action_20 _ = happyReduce_32

action_21 (35) = happyShift action_17
action_21 (36) = happyReduce_7
action_21 (6) = happyGoto action_42
action_21 (16) = happyGoto action_16
action_21 _ = happyReduce_32

action_22 (17) = happyShift action_41
action_22 (19) = happyShift action_7
action_22 (28) = happyShift action_8
action_22 (33) = happyShift action_11
action_22 (34) = happyShift action_12
action_22 (11) = happyGoto action_25
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (18) = happyShift action_40
action_23 _ = happyReduce_15

action_24 (17) = happyShift action_31
action_24 (19) = happyShift action_7
action_24 (28) = happyShift action_8
action_24 (33) = happyShift action_11
action_24 (34) = happyShift action_12
action_24 (11) = happyGoto action_28
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_12

action_26 (19) = happyShift action_35
action_26 (29) = happyShift action_36
action_26 (31) = happyShift action_37
action_26 (32) = happyShift action_38
action_26 (33) = happyShift action_39
action_26 (12) = happyGoto action_34
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_14

action_28 _ = happyReduce_13

action_29 (19) = happyShift action_7
action_29 (20) = happyShift action_32
action_29 (26) = happyShift action_33
action_29 (28) = happyShift action_8
action_29 (33) = happyShift action_11
action_29 (34) = happyShift action_12
action_29 (11) = happyGoto action_25
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (17) = happyShift action_31
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (19) = happyShift action_35
action_31 (29) = happyShift action_36
action_31 (31) = happyShift action_37
action_31 (32) = happyShift action_38
action_31 (33) = happyShift action_39
action_31 (12) = happyGoto action_52
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_18

action_33 (19) = happyShift action_7
action_33 (28) = happyShift action_8
action_33 (29) = happyShift action_9
action_33 (30) = happyShift action_10
action_33 (33) = happyShift action_11
action_33 (34) = happyShift action_12
action_33 (10) = happyGoto action_51
action_33 (11) = happyGoto action_6
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (27) = happyShift action_50
action_34 _ = happyReduce_31

action_35 (19) = happyShift action_35
action_35 (29) = happyShift action_36
action_35 (31) = happyShift action_37
action_35 (32) = happyShift action_38
action_35 (33) = happyShift action_39
action_35 (12) = happyGoto action_49
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_20

action_37 (19) = happyShift action_48
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (19) = happyShift action_47
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_21

action_40 (19) = happyShift action_35
action_40 (29) = happyShift action_36
action_40 (31) = happyShift action_37
action_40 (32) = happyShift action_38
action_40 (33) = happyShift action_39
action_40 (12) = happyGoto action_46
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (19) = happyShift action_7
action_41 (28) = happyShift action_8
action_41 (29) = happyShift action_9
action_41 (30) = happyShift action_10
action_41 (33) = happyShift action_11
action_41 (34) = happyShift action_12
action_41 (10) = happyGoto action_45
action_41 (11) = happyGoto action_6
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_5

action_43 _ = happyReduce_4

action_44 _ = happyReduce_3

action_45 (19) = happyShift action_7
action_45 (28) = happyShift action_8
action_45 (33) = happyShift action_11
action_45 (34) = happyShift action_12
action_45 (11) = happyGoto action_25
action_45 _ = happyReduce_10

action_46 (27) = happyShift action_50
action_46 _ = happyReduce_9

action_47 (29) = happyShift action_58
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (29) = happyShift action_57
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (20) = happyShift action_55
action_49 (26) = happyShift action_56
action_49 (27) = happyShift action_50
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (19) = happyShift action_35
action_50 (29) = happyShift action_36
action_50 (31) = happyShift action_37
action_50 (32) = happyShift action_38
action_50 (33) = happyShift action_39
action_50 (12) = happyGoto action_54
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (19) = happyShift action_7
action_51 (20) = happyShift action_53
action_51 (28) = happyShift action_8
action_51 (33) = happyShift action_11
action_51 (34) = happyShift action_12
action_51 (11) = happyGoto action_25
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (27) = happyShift action_50
action_52 _ = happyReduce_8

action_53 _ = happyReduce_17

action_54 (27) = happyShift action_50
action_54 _ = happyReduce_24

action_55 _ = happyReduce_26

action_56 (19) = happyShift action_35
action_56 (29) = happyShift action_36
action_56 (31) = happyShift action_37
action_56 (32) = happyShift action_38
action_56 (33) = happyShift action_39
action_56 (12) = happyGoto action_61
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (20) = happyShift action_60
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (20) = happyShift action_59
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (24) = happyShift action_64
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (21) = happyShift action_63
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (20) = happyShift action_62
action_61 (27) = happyShift action_50
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_22

action_63 (30) = happyShift action_68
action_63 (14) = happyGoto action_67
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (29) = happyShift action_66
action_64 (13) = happyGoto action_65
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (23) = happyShift action_72
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (19) = happyShift action_35
action_66 (29) = happyShift action_36
action_66 (31) = happyShift action_37
action_66 (32) = happyShift action_38
action_66 (33) = happyShift action_39
action_66 (12) = happyGoto action_71
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (22) = happyShift action_70
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (18) = happyShift action_69
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (19) = happyShift action_35
action_69 (29) = happyShift action_36
action_69 (31) = happyShift action_37
action_69 (32) = happyShift action_38
action_69 (33) = happyShift action_39
action_69 (12) = happyGoto action_74
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_25

action_71 (25) = happyShift action_73
action_71 (27) = happyShift action_50
action_71 _ = happyReduce_28

action_72 _ = happyReduce_23

action_73 (29) = happyShift action_66
action_73 (13) = happyGoto action_76
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (25) = happyShift action_75
action_74 (27) = happyShift action_50
action_74 _ = happyReduce_30

action_75 (30) = happyShift action_68
action_75 (14) = happyGoto action_77
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_27

action_77 _ = happyReduce_29

happyReduce_3 = happySpecReduce_3  6 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn6
		 (addTypeDef (Def { getDefLineNo = happy_var_1, getDef = happy_var_2 }) happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn6
		 (addSignatureDef (Def { getDefLineNo = happy_var_1, getDef = happy_var_2 }) happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn6
		 (addTermDef (Def { getDefLineNo = happy_var_1, getDef = happy_var_2 }) happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  6 happyReduction_7
happyReduction_7  =  HappyAbsSyn6
		 (ProgramDefs [] [] []
	)

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TUppercaseIdentifier happy_var_1))
	 =  HappyAbsSyn7
		 (TypeDef { getTypeVar = happy_var_1, getType = happy_var_3 }
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TLowercaseIdentifier happy_var_1))
	 =  HappyAbsSyn8
		 (SignatureDef { getSymbol = happy_var_1, getSymbolType = happy_var_3 }
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (TermDef { lhs = happy_var_1, rhs = happy_var_3 }
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  10 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (App happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  10 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TUppercaseIdentifier happy_var_1))
	 =  HappyAbsSyn10
		 (Constructor happy_var_1 happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  10 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TDestructor happy_var_1))
	 =  HappyAbsSyn10
		 (Destructor happy_var_1 happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyTerminal (TLowercaseIdentifier happy_var_1))
	 =  HappyAbsSyn10
		 (Var happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn10
		 (Unit
	)

happyReduce_17 = happyReduce 5 11 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Tuple happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 (HappyTerminal (TInt happy_var_1))
	 =  HappyAbsSyn10
		 (makeIntTerm happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 (HappyTerminal (TUppercaseIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 (TyVar happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  12 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn12
		 (TyUnit
	)

happyReduce_22 = happyReduce 5 12 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TyTuple happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 7 12 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TUppercaseIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TyData $ M.map (ligate happy_var_3) happy_var_6
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  12 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (TyFun happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 7 12 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TUppercaseIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (TyRecord $ M.map (ligate happy_var_3) happy_var_6
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  12 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 13 happyReduction_27
happyReduction_27 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	(HappyTerminal (TUppercaseIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (M.insert happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_2  13 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_2)
	(HappyTerminal (TUppercaseIdentifier happy_var_1))
	 =  HappyAbsSyn13
		 (M.singleton happy_var_1 happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happyReduce 5 14 happyReduction_29
happyReduction_29 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TDestructor happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (M.insert happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_3  14 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TDestructor happy_var_1))
	 =  HappyAbsSyn13
		 (M.singleton happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  15 happyReduction_31
happyReduction_31 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn15
		 (TypedTerm happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happyMonadReduce 0 16 happyReduction_32
happyReduction_32 (happyRest) tk
	 = happyThen ((( getLineNo))
	) (\r -> happyReturn (HappyAbsSyn16 r))

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 36 36 tk (HappyState action) sts stk;
	TEquals -> cont 17;
	TColon -> cont 18;
	TParenOpen -> cont 19;
	TParenClose -> cont 20;
	TBraceOpen -> cont 21;
	TBraceClose -> cont 22;
	TGreater -> cont 23;
	TLower -> cont 24;
	TVerticalSeparator -> cont 25;
	TComma -> cont 26;
	TArrow -> cont 27;
	TLowercaseIdentifier happy_dollar_dollar -> cont 28;
	TUppercaseIdentifier happy_dollar_dollar -> cont 29;
	TDestructor happy_dollar_dollar -> cont 30;
	TRecord -> cont 31;
	TData -> cont 32;
	TUnit -> cont 33;
	TInt happy_dollar_dollar -> cont 34;
	TEndLine -> cont 35;
	_ -> happyError' (tk, [])
	})

happyError_ explist 36 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> P a
happyError' tk = (\(tokens, explist) -> happyError) tk
parseDefsP = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

parseEvalExpP = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

parseTermP = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

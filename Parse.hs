{-# OPTIONS_GHC -w #-}
module Parse where
import Common
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn9 ([Def'])
	| HappyAbsSyn10 (Def')
	| HappyAbsSyn12 (VType)
	| HappyAbsSyn13 (M.Map Label VType)
	| HappyAbsSyn15 ([(Term,Term)])
	| HappyAbsSyn16 ((Term,Term))
	| HappyAbsSyn17 (Term)

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
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85 :: () => Int -> ({-HappyReduction (P) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

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
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39 :: () => ({-HappyReduction (P) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

action_0 (21) = happyShift action_28
action_0 (32) = happyShift action_29
action_0 (34) = happyShift action_30
action_0 (35) = happyShift action_31
action_0 (36) = happyShift action_32
action_0 (12) = happyGoto action_27
action_0 _ = happyFail

action_1 (21) = happyShift action_11
action_1 (30) = happyShift action_12
action_1 (31) = happyShift action_13
action_1 (32) = happyShift action_14
action_1 (33) = happyShift action_15
action_1 (36) = happyShift action_16
action_1 (40) = happyShift action_17
action_1 (16) = happyGoto action_26
action_1 (17) = happyGoto action_9
action_1 (18) = happyGoto action_19
action_1 _ = happyFail

action_2 (38) = happyShift action_25
action_2 (15) = happyGoto action_24
action_2 _ = happyReduce_27

action_3 (31) = happyShift action_8
action_3 (32) = happyShift action_22
action_3 (37) = happyShift action_23
action_3 (9) = happyGoto action_20
action_3 (10) = happyGoto action_7
action_3 (11) = happyGoto action_21
action_3 _ = happyReduce_10

action_4 (21) = happyShift action_11
action_4 (30) = happyShift action_12
action_4 (31) = happyShift action_13
action_4 (32) = happyShift action_14
action_4 (33) = happyShift action_15
action_4 (36) = happyShift action_16
action_4 (40) = happyShift action_17
action_4 (16) = happyGoto action_18
action_4 (17) = happyGoto action_9
action_4 (18) = happyGoto action_19
action_4 _ = happyFail

action_5 (21) = happyShift action_11
action_5 (30) = happyShift action_12
action_5 (31) = happyShift action_13
action_5 (32) = happyShift action_14
action_5 (33) = happyShift action_15
action_5 (36) = happyShift action_16
action_5 (40) = happyShift action_17
action_5 (17) = happyGoto action_9
action_5 (18) = happyGoto action_10
action_5 _ = happyFail

action_6 (31) = happyShift action_8
action_6 (10) = happyGoto action_7
action_6 _ = happyFail

action_7 (31) = happyShift action_8
action_7 (32) = happyShift action_22
action_7 (37) = happyShift action_23
action_7 (9) = happyGoto action_50
action_7 (10) = happyGoto action_7
action_7 (11) = happyGoto action_21
action_7 _ = happyReduce_10

action_8 (20) = happyShift action_49
action_8 _ = happyFail

action_9 (40) = happyShift action_48
action_9 _ = happyReduce_34

action_10 (21) = happyShift action_11
action_10 (30) = happyShift action_12
action_10 (31) = happyShift action_13
action_10 (36) = happyShift action_16
action_10 (41) = happyAccept
action_10 (17) = happyGoto action_42
action_10 _ = happyFail

action_11 (21) = happyShift action_11
action_11 (30) = happyShift action_12
action_11 (31) = happyShift action_13
action_11 (32) = happyShift action_14
action_11 (33) = happyShift action_15
action_11 (36) = happyShift action_16
action_11 (40) = happyShift action_17
action_11 (17) = happyGoto action_9
action_11 (18) = happyGoto action_47
action_11 _ = happyFail

action_12 _ = happyReduce_33

action_13 _ = happyReduce_29

action_14 (21) = happyShift action_11
action_14 (30) = happyShift action_12
action_14 (31) = happyShift action_13
action_14 (36) = happyShift action_16
action_14 (17) = happyGoto action_46
action_14 _ = happyFail

action_15 (21) = happyShift action_11
action_15 (30) = happyShift action_12
action_15 (31) = happyShift action_13
action_15 (36) = happyShift action_16
action_15 (17) = happyGoto action_45
action_15 _ = happyFail

action_16 _ = happyReduce_30

action_17 (21) = happyShift action_11
action_17 (30) = happyShift action_12
action_17 (31) = happyShift action_13
action_17 (36) = happyShift action_16
action_17 (17) = happyGoto action_44
action_17 _ = happyFail

action_18 (41) = happyAccept
action_18 _ = happyFail

action_19 (19) = happyShift action_43
action_19 (21) = happyShift action_11
action_19 (30) = happyShift action_12
action_19 (31) = happyShift action_13
action_19 (36) = happyShift action_16
action_19 (17) = happyGoto action_42
action_19 _ = happyFail

action_20 (41) = happyAccept
action_20 _ = happyFail

action_21 (31) = happyShift action_8
action_21 (32) = happyShift action_22
action_21 (37) = happyShift action_23
action_21 (38) = happyShift action_41
action_21 (9) = happyGoto action_40
action_21 (10) = happyGoto action_7
action_21 (11) = happyGoto action_21
action_21 _ = happyReduce_10

action_22 (19) = happyShift action_39
action_22 _ = happyFail

action_23 (31) = happyShift action_8
action_23 (32) = happyShift action_22
action_23 (37) = happyShift action_23
action_23 (9) = happyGoto action_38
action_23 (10) = happyGoto action_7
action_23 (11) = happyGoto action_21
action_23 _ = happyReduce_10

action_24 (41) = happyAccept
action_24 _ = happyFail

action_25 (21) = happyShift action_11
action_25 (30) = happyShift action_12
action_25 (31) = happyShift action_13
action_25 (32) = happyShift action_14
action_25 (33) = happyShift action_15
action_25 (36) = happyShift action_16
action_25 (40) = happyShift action_17
action_25 (16) = happyGoto action_37
action_25 (17) = happyGoto action_9
action_25 (18) = happyGoto action_19
action_25 _ = happyFail

action_26 (41) = happyAccept
action_26 _ = happyFail

action_27 (29) = happyShift action_36
action_27 (41) = happyAccept
action_27 _ = happyFail

action_28 (21) = happyShift action_28
action_28 (32) = happyShift action_29
action_28 (34) = happyShift action_30
action_28 (35) = happyShift action_31
action_28 (36) = happyShift action_32
action_28 (12) = happyGoto action_35
action_28 _ = happyFail

action_29 _ = happyReduce_13

action_30 (21) = happyShift action_34
action_30 _ = happyFail

action_31 (21) = happyShift action_33
action_31 _ = happyFail

action_32 _ = happyReduce_14

action_33 (32) = happyShift action_62
action_33 _ = happyFail

action_34 (32) = happyShift action_61
action_34 _ = happyFail

action_35 (22) = happyShift action_59
action_35 (28) = happyShift action_60
action_35 (29) = happyShift action_36
action_35 _ = happyFail

action_36 (21) = happyShift action_28
action_36 (32) = happyShift action_29
action_36 (34) = happyShift action_30
action_36 (35) = happyShift action_31
action_36 (36) = happyShift action_32
action_36 (12) = happyGoto action_58
action_36 _ = happyFail

action_37 (38) = happyShift action_25
action_37 (15) = happyGoto action_57
action_37 _ = happyReduce_27

action_38 _ = happyReduce_9

action_39 (21) = happyShift action_28
action_39 (32) = happyShift action_29
action_39 (34) = happyShift action_30
action_39 (35) = happyShift action_31
action_39 (36) = happyShift action_32
action_39 (12) = happyGoto action_56
action_39 _ = happyFail

action_40 _ = happyReduce_7

action_41 (31) = happyShift action_8
action_41 (32) = happyShift action_22
action_41 (37) = happyShift action_23
action_41 (9) = happyGoto action_55
action_41 (10) = happyGoto action_7
action_41 (11) = happyGoto action_21
action_41 _ = happyReduce_10

action_42 _ = happyReduce_35

action_43 (21) = happyShift action_11
action_43 (30) = happyShift action_12
action_43 (31) = happyShift action_13
action_43 (32) = happyShift action_14
action_43 (33) = happyShift action_15
action_43 (36) = happyShift action_16
action_43 (40) = happyShift action_17
action_43 (17) = happyGoto action_9
action_43 (18) = happyGoto action_54
action_43 _ = happyFail

action_44 _ = happyReduce_39

action_45 _ = happyReduce_37

action_46 _ = happyReduce_36

action_47 (21) = happyShift action_11
action_47 (22) = happyShift action_52
action_47 (28) = happyShift action_53
action_47 (30) = happyShift action_12
action_47 (31) = happyShift action_13
action_47 (36) = happyShift action_16
action_47 (17) = happyGoto action_42
action_47 _ = happyFail

action_48 _ = happyReduce_38

action_49 (21) = happyShift action_28
action_49 (32) = happyShift action_29
action_49 (34) = happyShift action_30
action_49 (35) = happyShift action_31
action_49 (36) = happyShift action_32
action_49 (12) = happyGoto action_51
action_49 _ = happyFail

action_50 _ = happyReduce_6

action_51 (29) = happyShift action_36
action_51 (38) = happyShift action_25
action_51 (15) = happyGoto action_67
action_51 _ = happyReduce_27

action_52 _ = happyReduce_32

action_53 (21) = happyShift action_11
action_53 (30) = happyShift action_12
action_53 (31) = happyShift action_13
action_53 (32) = happyShift action_14
action_53 (33) = happyShift action_15
action_53 (36) = happyShift action_16
action_53 (40) = happyShift action_17
action_53 (17) = happyGoto action_9
action_53 (18) = happyGoto action_66
action_53 _ = happyFail

action_54 (21) = happyShift action_11
action_54 (30) = happyShift action_12
action_54 (31) = happyShift action_13
action_54 (36) = happyShift action_16
action_54 (17) = happyGoto action_42
action_54 _ = happyReduce_28

action_55 _ = happyReduce_8

action_56 (29) = happyShift action_36
action_56 _ = happyReduce_12

action_57 _ = happyReduce_26

action_58 (29) = happyShift action_36
action_58 _ = happyReduce_17

action_59 _ = happyReduce_19

action_60 (21) = happyShift action_28
action_60 (32) = happyShift action_29
action_60 (34) = happyShift action_30
action_60 (35) = happyShift action_31
action_60 (36) = happyShift action_32
action_60 (12) = happyGoto action_65
action_60 _ = happyFail

action_61 (22) = happyShift action_64
action_61 _ = happyFail

action_62 (22) = happyShift action_63
action_62 _ = happyFail

action_63 (26) = happyShift action_71
action_63 _ = happyFail

action_64 (23) = happyShift action_70
action_64 _ = happyFail

action_65 (22) = happyShift action_69
action_65 (29) = happyShift action_36
action_65 _ = happyFail

action_66 (21) = happyShift action_11
action_66 (22) = happyShift action_68
action_66 (30) = happyShift action_12
action_66 (31) = happyShift action_13
action_66 (36) = happyShift action_16
action_66 (17) = happyGoto action_42
action_66 _ = happyFail

action_67 _ = happyReduce_11

action_68 _ = happyReduce_31

action_69 _ = happyReduce_15

action_70 (33) = happyShift action_75
action_70 (14) = happyGoto action_74
action_70 _ = happyReduce_25

action_71 (32) = happyShift action_73
action_71 (13) = happyGoto action_72
action_71 _ = happyReduce_22

action_72 (25) = happyShift action_79
action_72 _ = happyFail

action_73 (20) = happyShift action_78
action_73 _ = happyFail

action_74 (24) = happyShift action_77
action_74 _ = happyFail

action_75 (20) = happyShift action_76
action_75 _ = happyFail

action_76 (21) = happyShift action_28
action_76 (32) = happyShift action_29
action_76 (34) = happyShift action_30
action_76 (35) = happyShift action_31
action_76 (36) = happyShift action_32
action_76 (12) = happyGoto action_81
action_76 _ = happyFail

action_77 _ = happyReduce_18

action_78 (21) = happyShift action_28
action_78 (32) = happyShift action_29
action_78 (34) = happyShift action_30
action_78 (35) = happyShift action_31
action_78 (36) = happyShift action_32
action_78 (12) = happyGoto action_80
action_78 _ = happyFail

action_79 _ = happyReduce_16

action_80 (27) = happyShift action_83
action_80 (29) = happyShift action_36
action_80 _ = happyReduce_21

action_81 (27) = happyShift action_82
action_81 (29) = happyShift action_36
action_81 _ = happyReduce_24

action_82 (33) = happyShift action_75
action_82 (14) = happyGoto action_85
action_82 _ = happyReduce_25

action_83 (32) = happyShift action_73
action_83 (13) = happyGoto action_84
action_83 _ = happyReduce_22

action_84 _ = happyReduce_20

action_85 _ = happyReduce_23

happyReduce_6 = happySpecReduce_2  9 happyReduction_6
happyReduction_6 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  9 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  9 happyReduction_10
happyReduction_10  =  HappyAbsSyn9
		 ([]
	)

happyReduce_11 = happyReduce 4 10 happyReduction_11
happyReduction_11 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (RuleDef happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  11 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TUppercaseIdentifier happy_var_1))
	 =  HappyAbsSyn10
		 (TypeDef happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  12 happyReduction_13
happyReduction_13 (HappyTerminal (TUppercaseIdentifier happy_var_1))
	 =  HappyAbsSyn12
		 (VTyVar happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn12
		 (VTyUnit
	)

happyReduce_15 = happyReduce 5 12 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (VTyTuple happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 7 12 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TUppercaseIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (VTyData happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (VTyFun happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 7 12 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TUppercaseIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (VTyRecord happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 5 13 happyReduction_20
happyReduction_20 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TUppercaseIdentifier happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (M.insert happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TUppercaseIdentifier happy_var_1))
	 =  HappyAbsSyn13
		 (M.singleton happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_0  13 happyReduction_22
happyReduction_22  =  HappyAbsSyn13
		 (M.empty
	)

happyReduce_23 = happyReduce 5 14 happyReduction_23
happyReduction_23 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TDestructor happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (M.insert happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  14 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TDestructor happy_var_1))
	 =  HappyAbsSyn13
		 (M.singleton happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_0  14 happyReduction_25
happyReduction_25  =  HappyAbsSyn13
		 (M.empty
	)

happyReduce_26 = happySpecReduce_3  15 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2 : happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  15 happyReduction_27
happyReduction_27  =  HappyAbsSyn15
		 ([]
	)

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ((happy_var_1,happy_var_3)
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  17 happyReduction_29
happyReduction_29 (HappyTerminal (TIdentifier happy_var_1))
	 =  HappyAbsSyn17
		 (Var happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn17
		 (Unit
	)

happyReduce_31 = happyReduce 5 17 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Tuple happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_3  17 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 (HappyTerminal (TNat happy_var_1))
	 =  HappyAbsSyn17
		 (natToTerm happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  18 happyReduction_35
happyReduction_35 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (App happy_var_1 happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  18 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_2)
	(HappyTerminal (TUppercaseIdentifier happy_var_1))
	 =  HappyAbsSyn17
		 (Constructor happy_var_1 happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  18 happyReduction_37
happyReduction_37 (HappyAbsSyn17  happy_var_2)
	(HappyTerminal (TDestructor happy_var_1))
	 =  HappyAbsSyn17
		 (Destructor happy_var_1 happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  18 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Constructor "Succ" happy_var_1
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  18 happyReduction_39
happyReduction_39 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (Constructor "Succ" happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 41 41 tk (HappyState action) sts stk;
	TEquals -> cont 19;
	TColon -> cont 20;
	TParenOpen -> cont 21;
	TParenClose -> cont 22;
	TBraceOpen -> cont 23;
	TBraceClose -> cont 24;
	TGreater -> cont 25;
	TLower -> cont 26;
	TSeparator -> cont 27;
	TComma -> cont 28;
	TArrow -> cont 29;
	TNat happy_dollar_dollar -> cont 30;
	TIdentifier happy_dollar_dollar -> cont 31;
	TUppercaseIdentifier happy_dollar_dollar -> cont 32;
	TDestructor happy_dollar_dollar -> cont 33;
	TRecord -> cont 34;
	TData -> cont 35;
	TUnit -> cont 36;
	TEndlineBlock -> cont 37;
	TEndlineSimple -> cont 38;
	TPrePlus -> cont 39;
	TPostPlus -> cont 40;
	_ -> happyError' tk
	})

happyError_ 41 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> P a
happyError' tk = (\token -> happyError) tk

parseType = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

parseRule = happySomeParser where
  happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn16 z -> happyReturn z; _other -> notHappyAtAll })

parseRules = happySomeParser where
  happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

parseDefs = happySomeParser where
  happySomeParser = happyThen (happyParse action_3) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

aux_ = happySomeParser where
  happySomeParser = happyThen (happyParse action_4) (\x -> case x of {HappyAbsSyn16 z -> happyReturn z; _other -> notHappyAtAll })

aux_2 = happySomeParser where
  happySomeParser = happyThen (happyParse action_5) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

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
happyFail  i tk (HappyState (action)) sts stk =
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

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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

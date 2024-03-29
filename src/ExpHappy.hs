{-# OPTIONS_GHC -w #-}
module ExpHappy where

import Control.Applicative (liftA2)
import ExpAlex as A
import Exp as E
import BoolForm as B
import Data.Maybe (listToMaybe)
import Control.Monad(join)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Maybe (Exp Char))
	| HappyAbsSyn5 (Maybe (BoolForm Integer))
	| HappyAbsSyn6 ([Maybe (Exp Char)])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
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
 action_39 :: () => Prelude.Int -> ({-HappyReduction (Maybe) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Maybe) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
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
 happyReduce_20 :: () => ({-HappyReduction (Maybe) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Maybe) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Maybe) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,82) ([16384,71,18240,14336,0,0,0,0,0,16384,71,13444,14336,0,18240,16384,71,0,8960,64,13444,0,0,13444,0,0,0,14336,8,0,8960,8,0,33792,52,13444,33792,52,1024,8192,0,48,0,0,20288,768,0,0,512,0,0,14464,0,2048,0,0,0,16384,71,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_expfromString","Exp","Form","ExpList","NonEmptyList","virg","ou","et","non","somme","conc","star","symb","atom","vide","eps","po","pf","bot","top","vert","%eof"]
        bit_start = st Prelude.* 24
        bit_end = (st Prelude.+ 1) Prelude.* 24
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..23]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (15) = happyShift action_3
action_0 (17) = happyShift action_4
action_0 (18) = happyShift action_5
action_0 (19) = happyShift action_6
action_0 (23) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (15) = happyShift action_3
action_1 (17) = happyShift action_4
action_1 (18) = happyShift action_5
action_1 (19) = happyShift action_6
action_1 (23) = happyShift action_7
action_1 (4) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (12) = happyShift action_9
action_2 (13) = happyShift action_10
action_2 (14) = happyShift action_11
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_7

action_4 _ = happyReduce_5

action_5 _ = happyReduce_6

action_6 (15) = happyShift action_3
action_6 (17) = happyShift action_4
action_6 (18) = happyShift action_5
action_6 (19) = happyShift action_6
action_6 (23) = happyShift action_7
action_6 (4) = happyGoto action_18
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (11) = happyShift action_13
action_7 (16) = happyShift action_14
action_7 (19) = happyShift action_15
action_7 (21) = happyShift action_16
action_7 (22) = happyShift action_17
action_7 (5) = happyGoto action_12
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (12) = happyShift action_9
action_8 (13) = happyShift action_10
action_8 (14) = happyShift action_11
action_8 (24) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (15) = happyShift action_3
action_9 (17) = happyShift action_4
action_9 (18) = happyShift action_5
action_9 (19) = happyShift action_6
action_9 (23) = happyShift action_7
action_9 (4) = happyGoto action_27
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (15) = happyShift action_3
action_10 (17) = happyShift action_4
action_10 (18) = happyShift action_5
action_10 (19) = happyShift action_6
action_10 (23) = happyShift action_7
action_10 (4) = happyGoto action_26
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_3

action_12 (9) = happyShift action_22
action_12 (10) = happyShift action_23
action_12 (14) = happyShift action_24
action_12 (23) = happyShift action_25
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (11) = happyShift action_13
action_13 (16) = happyShift action_14
action_13 (19) = happyShift action_15
action_13 (21) = happyShift action_16
action_13 (22) = happyShift action_17
action_13 (5) = happyGoto action_21
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_16

action_15 (11) = happyShift action_13
action_15 (16) = happyShift action_14
action_15 (19) = happyShift action_15
action_15 (21) = happyShift action_16
action_15 (22) = happyShift action_17
action_15 (5) = happyGoto action_20
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_15

action_17 _ = happyReduce_14

action_18 (12) = happyShift action_9
action_18 (13) = happyShift action_10
action_18 (14) = happyShift action_11
action_18 (20) = happyShift action_19
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_4

action_20 (9) = happyShift action_22
action_20 (10) = happyShift action_23
action_20 (14) = happyShift action_24
action_20 (20) = happyShift action_33
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_13

action_22 (11) = happyShift action_13
action_22 (16) = happyShift action_14
action_22 (19) = happyShift action_15
action_22 (21) = happyShift action_16
action_22 (22) = happyShift action_17
action_22 (5) = happyGoto action_32
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (11) = happyShift action_13
action_23 (16) = happyShift action_14
action_23 (19) = happyShift action_15
action_23 (21) = happyShift action_16
action_23 (22) = happyShift action_17
action_23 (5) = happyGoto action_31
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (11) = happyShift action_13
action_24 (16) = happyShift action_14
action_24 (19) = happyShift action_15
action_24 (21) = happyShift action_16
action_24 (22) = happyShift action_17
action_24 (5) = happyGoto action_30
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (19) = happyShift action_29
action_25 (6) = happyGoto action_28
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (14) = happyShift action_11
action_26 _ = happyReduce_2

action_27 (13) = happyShift action_10
action_27 (14) = happyShift action_11
action_27 _ = happyReduce_1

action_28 _ = happyReduce_8

action_29 (15) = happyShift action_3
action_29 (17) = happyShift action_4
action_29 (18) = happyShift action_5
action_29 (19) = happyShift action_6
action_29 (20) = happyShift action_36
action_29 (23) = happyShift action_7
action_29 (4) = happyGoto action_34
action_29 (7) = happyGoto action_35
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (9) = happyShift action_22
action_30 (10) = happyShift action_23
action_30 _ = happyReduce_10

action_31 _ = happyReduce_9

action_32 (10) = happyShift action_23
action_32 _ = happyReduce_11

action_33 _ = happyReduce_12

action_34 (8) = happyShift action_38
action_34 (12) = happyShift action_9
action_34 (13) = happyShift action_10
action_34 (14) = happyShift action_11
action_34 _ = happyReduce_20

action_35 (20) = happyShift action_37
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_17

action_37 _ = happyReduce_18

action_38 (15) = happyShift action_3
action_38 (17) = happyShift action_4
action_38 (18) = happyShift action_5
action_38 (19) = happyShift action_6
action_38 (23) = happyShift action_7
action_38 (4) = happyGoto action_34
action_38 (7) = happyGoto action_39
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_19

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (liftA2 Sum happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (liftA2 E.Concat happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (fmap E.Star happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn4
		 (Just E.Empty
	)

happyReduce_6 = happySpecReduce_1  4 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn4
		 (Just E.Epsilon
	)

happyReduce_7 = happySpecReduce_1  4 happyReduction_7
happyReduction_7 (HappyTerminal (A.Symb happy_var_1))
	 =  HappyAbsSyn4
		 (fmap E.Symbol (listToMaybe happy_var_1)
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 4 happyReduction_8
happyReduction_8 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (join $ liftA2 E.consTilde happy_var_2 (sequence happy_var_4)
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (liftA2 B.And happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (liftA2 B.And happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (liftA2 B.Or happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (fmap B.Not happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  5 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn5
		 (Just B.Top
	)

happyReduce_15 = happySpecReduce_1  5 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn5
		 (Just B.Bot
	)

happyReduce_16 = happySpecReduce_1  5 happyReduction_16
happyReduction_16 (HappyTerminal (A.Number happy_var_1))
	 =  HappyAbsSyn5
		 (Just $ Atom happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  6 happyReduction_17
happyReduction_17 _
	_
	 =  HappyAbsSyn6
		 ([]
	)

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  7 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  7 happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 24 24 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	A.Virg -> cont 8;
	A.Or -> cont 9;
	A.And -> cont 10;
	A.Non -> cont 11;
	A.Somme -> cont 12;
	A.Conc -> cont 13;
	A.Star -> cont 14;
	A.Symb happy_dollar_dollar -> cont 15;
	A.Number happy_dollar_dollar -> cont 16;
	A.Vide -> cont 17;
	A.Epsilon -> cont 18;
	A.ParO -> cont 19;
	A.ParF -> cont 20;
	A.Bot -> cont 21;
	A.Top -> cont 22;
	A.Vert -> cont 23;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 24 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Maybe a -> (a -> Maybe b) -> Maybe b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Maybe a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Maybe a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Maybe a
happyError' = (\(tokens, _) -> parseError tokens)
expfromString tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError _ = Nothing
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
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





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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

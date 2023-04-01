{-# LANGUAGE DeriveFunctor #-}
module BoolForm where

data BoolForm a
  = Atom a
  | Bot
  | Top
  | And (BoolForm a) (BoolForm a)
  | Or (BoolForm a) (BoolForm a)
  | Not (BoolForm a)
  deriving (Functor)

subst :: Eq a => BoolForm a -> a -> BoolForm a -> BoolForm a
subst _ _ Bot = Bot
subst _ _ Top = Top
subst f a (Atom b)
  | b == a = f
  | otherwise = Atom b
subst f a (And f1 f2) = And (subst f a f1) (subst f a f2)
subst f a (Or f1 f2) = Or (subst f a f1) (subst f a f2)
subst f a (Not f1) = Not (subst f a f1)

reduce :: BoolForm a -> BoolForm a
reduce Bot = Bot
reduce Top = Top
reduce f@(Atom _) = f
reduce (And f g) =
  case reduce f of
    Bot -> Bot
    Top -> reduce g
    f' -> case reduce g of
      Bot -> Bot
      Top -> f'
      g' -> And f' g'
reduce (Or f g) =
  case reduce f of
    Bot -> reduce g
    Top -> Top
    f' -> case reduce g of
      Bot -> f'
      Top -> Top
      g' -> Or f' g'
reduce (Not f) =
  case reduce f of
    Bot -> Top
    Top -> Bot
    f' -> Not f'

rename :: (a -> b) -> BoolForm a -> BoolForm b
rename = fmap

module Prelude where

undefined : a
undefined = _|_

id x = x
const a b = a
flip f a b = f b a

unifiable : a -> a -> a
unifiable = _|_


data Bool : * where
  False : Bool
  True  : Bool

data Ordering : * where
  GT : Ordering
  LT : Ordering
  EQ : Ordering

not b = case b of
  True  -> False
  False -> True

and a b = case a of
  True  -> b
  False -> False

or a b = case a of
  True -> True
  False -> b

data Nat : * where
  Z : Nat
  S : Nat -> Nat

add a b = case a of
  Z   -> b
  S n -> S (add n b)

data Pair : * -> * -> * where
  MkPair : a -> b -> Pair a b

pair f p = case p of
  MkPair a b -> f a b

data Identity : * -> * where
  MkIdentity : a -> Identity a

runIdentity i = case i of
  MkIdentity a -> a

data Maybe : * -> * where
  Nothing : Maybe a
  Just    : a -> Maybe a

maybe d f m = case m of
  Nothing -> d
  Just x  -> f x

fromMaybe d m = case m of
  Nothing -> d
  Just x  -> x

data Either : * -> * -> * where
  Left  : a -> Either a b
  Right : b -> Either a b

either f g e = case e of
  Left  a -> f a
  Right b -> g b

data List : * -> * where
  Nil  : List a
  Cons : a -> List a -> List a

data StateT : * -> (* -> *) -> * -> * where
  MkStateT : (s -> m (Pair s a)) -> StateT s m a


data Apply : (a -> *) -> a -> * where
  MkApply : m a -> Apply m a

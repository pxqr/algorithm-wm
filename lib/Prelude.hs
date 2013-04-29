module Prelude where

undefined : a
undefined = _|_

id x = x
const a b = a
flip f a b = f b a

unifiable : a -> a -> a
unifiable = _|_

data Void : *

voidTest : Void
voidTest = undefined

data Unit : * where
  MkUnit : Unit

{- ------------------------------------------------------------------ -}
data Bool : * where
  False : Bool
  True  : Bool

otherwise : Bool
otherwise = True

not b = case b of
  True  -> False
  False -> True

if p a b = case p of
  True -> a
  False -> b

con : Bool -> Bool -> Bool
con a b = if a b False

dis : Bool -> Bool -> Bool
dis a b = if a True b

{- ------------------------------------------------------------------ -}
data Nat : * where
  Z : Nat
  S : Nat -> Nat

succ : Nat -> Nat
succ = S

zero  = 0
one   = 1
two   = 2
three = 3
four  = 4
five  = 5
six   = 6
seven = 7
eight = 8
nine  = 9
ten   = 10

add a b = case a of
  Z   -> b
  S n -> S (add n b)

{- ------------------------------------------------------------------ -}
data Ordering : * where
  GT : Ordering
  LT : Ordering
  EQ : Ordering

plusOrd : Ordering -> Ordering -> Ordering
plusOrd a b = undefined

isEQ : Ordering -> Bool
isEQ x = case x of
  EQ -> True
  _  -> False

isLT : Ordering -> Bool
isLT x = case x of
  LT -> True
  _  -> False

isGT : Ordering -> Bool
isGT x = case x of
  GT -> True
  _  -> False

compareNat : Nat -> Nat -> Ordering
compareNat a b = case a of
  Z -> case b of
    Z -> EQ
    S bp -> LT
  S ap -> case b of
    Z -> GT
    S bp -> undefined

eqNat : Nat -> Nat -> Bool
eqNat n m = isEQ (compareNat n m)

ltNat : Nat -> Nat -> Bool
ltNat n m = isLT (compareNat n m)

gtNat : Nat -> Nat -> Bool
gtNat n m = isGT (compareNat n m)


data Pair : * -> * -> * where
  MkPair : a -> b -> Pair a b

pair f p = case p of
  (a, b) -> f a b

fst = pair const
snd = pair (flip const)

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

null : [a] -> Bool
null l = case l of
  [] -> True
  x : xs -> False

concat : [a] -> [a] -> [a]
concat as bs = case as of
  [] -> bs
  x : xs -> x : concat xs bs

map f l = case l of
  [] -> []
  x : xs -> f x : map f xs

replicate n a = case n of
  Z -> Nil
  S n -> a : replicate n a



{-
lookupNat : Nat -> List (Pair Nat a) -> Maybe a
lookupNat a l = case l of
  Nil       -> Nothing
  Cons x xs ->
    case x of
      (k, v) ->
        case eqNat k a of
          True  -> Just x
          False -> lookupNat a xs
-}

foldr f a l = case l of
  [] -> a
  x : xs -> f x (foldr f a xs)

{- -------------------- specialized folds -}
sum : List Nat -> Nat
sum = foldr add 0

and : List Bool -> Bool
and = foldr con True

or : List Bool -> Bool
or = foldr dis False

all : (a -> Bool) -> List a -> Bool
all f xs = and (map f xs)

any : (a -> Bool) -> List a -> Bool
any f xs = or (map f xs)

data StateT : * -> (* -> *) -> * -> * where
  MkStateT : (s -> m (Pair s a)) -> StateT s m a


data Apply : (a -> *) -> a -> * where
  MkApply : m a -> Apply m a


{-
test1 = (1, case (_|_, 3) of (n, m) -> (1, m))

-}
module Prelude where

undefined : a
undefined = _|_

id x = x
const a b = a
flip f a b = f b a
o f g x = f (g x)
fix f = f (fix f)


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

{- elimNat : a -> (a -> a) -> Nat -> a -}
elimNat f n a = case n of
  Z   -> a
  S p -> f (elimNat f a p)


add = elimNat succ

mul a b = case a of
  Z   -> Z
  S n -> add b (mul n b)


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
    S bp -> compareNat ap bp

eqNat : Nat -> Nat -> Bool
eqNat n m = isEQ (compareNat n m)

ltNat : Nat -> Nat -> Bool
ltNat n m = isLT (compareNat n m)

gtNat : Nat -> Nat -> Bool
gtNat n m = isGT (compareNat n m)


data Pair : * -> * -> * where
  MkPair : a -> b -> Pair a b

pair : (a -> b -> c) -> (a, b) -> c
pair f p = case p of
  (a, b) -> f a b

fst : (a, b) -> a
fst = pair const

snd : (a, b) -> b
snd = pair (flip const)

both : (a -> b) -> (c -> d) -> (a, c) -> (b, d)
both f g p = case p of
  (a, c) -> (f a, g c)

first : (a -> b) -> (a, c) -> (b, c)
first f = both f id

second : (b -> c) -> (a, b) -> (a, c)
second = both id


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

fromMaybe : a -> Maybe a -> a
fromMaybe d m = case m of
  Nothing -> d
  Just x  -> x

isNothing : Maybe a -> Bool
isNothing = maybe True (const False)

isJust : Maybe a -> Bool
isJust = maybe False (const True)


data Either : * -> * -> * where
  Left  : a -> Either a b
  Right : b -> Either a b

either : (a -> c) -> (b -> c) -> Either a b -> c
either f g e = case e of
  Left  a -> f a
  Right b -> g b

bimap : (a -> c) -> (b -> d) -> Either a b -> Either c d
bimap f g = either (o Left f) (o Right g)

mapLeft : (a -> b) -> Either a c -> Either b c
mapLeft f = bimap f id

mapRight : (a -> b) -> Either a b -> Either a c
mapRight g = bimap id g


data List : * -> * where
  Nil  : List a
  Cons : a -> List a -> List a

isNil : List a -> Bool
isNil l = case l of
  [] -> True
  x : xs -> False

isCons : List a -> Bool
isCons l = case l of
  [] -> False
  x : xs -> True

maybeToList : Maybe a -> [a]
maybeToList m = case m of
  Nothing -> []
  Just x  -> [x]

listToMaybe : [a] -> Maybe a
listToMaybe l = case l of
  [] -> Nothing
  x : xs -> Just x

null : [a] -> Bool
null l = case l of
  [] -> True
  x : xs -> False

replicate n a = case n of
  Z -> Nil
  S n -> a : replicate n a

repeat : a -> [a]
repeat a = a : repeat a

drop : Nat -> [a] -> [a]
drop n l = case n of
  Z   -> l
  S p -> case l of
    [] -> []
    x : xs -> drop p xs

take : Nat -> [a] -> [a]
take n l = case n of
  Z   -> []
  S p -> case l of
    [] -> []
    x : xs -> x : take p xs

dropWhile : (a -> Bool) -> [a] -> [a]
dropWhile f l = case l of
  [] -> []
  x : xs -> if (f x) (dropWhile f xs) l

takeWhile : (a -> Bool) -> [a] -> [a]
takeWhile f l = case l of
  [] -> []
  x : xs -> if (f x) (x : takeWhile f xs) []

iterate : (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

enumFrom : Nat -> [Nat]
enumFrom = iterate succ

lookupNat : Nat -> [(Nat, a)] -> Maybe a
lookupNat a l = case l of
  []  -> Nothing
  x : xs -> case x of
    (k, v) -> if (eqNat k a) (Just v) (lookupNat a xs)

foldr : (a -> b -> b) -> b -> [a] -> b
foldr f a l = case l of
  [] -> a
  x : xs -> f x (foldr f a xs)

map : (a -> b) -> [a] -> [b]
map f = foldr (o Cons f) []

{-
mapMaybe : [Maybe a] -> [a]
mapMaybe
-}
{- -------------------- specialized folds -}
concat : [a] -> [a] -> [a]
concat = flip (foldr Cons)

sum : [Nat] -> Nat
sum = foldr add 0

product : [Nat] -> Nat
product = foldr mul 0

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
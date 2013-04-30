module Prelude where

undefined : a
undefined = _|_

id : forall a. a -> a
id x = x

const : a -> b -> a
const a b = a

flip : (a -> b -> c) -> b -> a -> c
flip f a b = f b a

o : (b -> c) -> (a -> b) -> (a -> c)
o f g x = f (g x)

oo : (c -> d) -> (b -> c) -> (a -> b) -> (a -> d)
oo f g = o (o f) (o g)

fix : (a -> a) -> a
fix f = f (fix f)


unifiable : a -> a -> a
unifiable = _|_

data Void : *

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

min : Nat -> Nat -> Nat
min a b = case a of
  Z -> Z
  S n -> case b of
    Z -> Z
    S m -> S (min n m)

max : Nat -> Nat -> Nat
max a b = case a of
  Z -> b
  S n -> case b of
    Z -> a
    S m -> S (max n m)

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

mapM : (a -> b) -> Maybe a -> Maybe b
mapM f = maybe Nothing (o Just f)

joinM : Maybe (Maybe a) -> Maybe a
joinM = maybe Nothing id

bindM : Maybe a -> (a -> Maybe b) -> Maybe b
bindM m f = joinM (mapM f m)

bindFM = flip bindM

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

foldr1 : (a -> b -> b) -> [a] -> b
foldr1 f = foldr f undefined

foldl : (b -> a -> b) -> b -> [a] -> b
foldl f a l = case l of
  [] -> a
  x : xs -> foldl f (f a x) xs

unfoldr : (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
  Nothing -> []
  Just p  -> case p of
    (x, b1) -> x : unfoldr f b1

{- take 1 (unfoldr (const (Just (1, undefined))) undefined) -}

{-
mapMaybe : [Maybe a] -> [a]
mapMaybe
-}
{- -------------------- specialized folds -}
reverse : [a] -> [a]
reverse = foldl (flip Cons) []

map : (a -> b) -> [a] -> [b]
map f = foldr (o Cons f) []

minimum : Nat -> [Nat] -> Nat
minimum = foldr min

maximum : Nat -> [Nat] -> Nat
maximum = foldr max

length : [a] -> Nat
length = foldr (const succ) 0

append : [a] -> [a] -> [a]
append = flip (foldr Cons)

concat : [[a]] -> [a]
concat = foldr append []

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


{-
test1 = (1, case (_|_, 3) of (n, m) -> (1, m))

-}

data Queue : * -> * where
  Qu : [a] -> [a] -> Queue a

emptyQ : Queue a
emptyQ = Qu [] []

isEmptyQ : Queue a -> Bool
isEmptyQ q = case q of
  Qu a b -> con (null a) (null b)

showQ : Queue a -> [a]
showQ q = case q of
  Qu a b -> append a (reverse b)

balanceQ : Queue a -> Queue a
balanceQ q = case q of
  Qu a b -> case b of
    [] -> Qu [] (reverse a)
    x : xs -> q

enqueue : a -> Queue a -> Queue a
enqueue e q = case q of
  Qu a b -> Qu (e : a) b

dequeue : Queue a -> Maybe (a, Queue a)
dequeue q = case balanceQ q of
  Qu a b -> case b of
    [] -> Nothing
    x : xs -> Just (x, Qu a xs)

fromListQ : [a] -> Queue a
fromListQ = foldr enqueue emptyQ

toListQ : Queue a -> [a]
toListQ = unfoldr dequeue

reverseQ : Queue a -> Queue a
reverseQ = o fromListQ toListQ


data Tree : * -> * where
  Node : a -> [Tree a] -> Tree a



data Zero : *
data Succ : * -> *
data Vec  : * -> * -> * where
    NilV  : Vec Zero a
    ConsV : a -> Vec n a -> Vec (Succ n) a

test100 = case () of
  () -> NilV
  () -> ConsV 0 NilV

data StateT : * -> (* -> *) -> * -> * where
   MkStateT : (s -> m (Pair s a)) -> StateT s m a


data Id : * -> * where
   MkId : a -> Id a

data Const : * -> a -> * where
   MkConst : a -> Const a b

data Apply : (a -> *) -> a -> * where
   MkApply : m a -> Apply m a

data Fix : (* -> *) -> * where
   MkFix : f (Fix f) -> Fix f

data Flip : (a -> b -> *) -> b -> a -> * where
   MkFlip : f b a -> Flip f a b

data Compose : (b -> *) -> (a -> b) -> * -> * where
   MkCompose : f (g a) -> Compose f g a

data On : (b -> b -> *) -> (a -> b) -> a -> a -> * where
   MkOn : g (f a) (f b) -> On g f a b

data First : (* -> *) -> (* -> * -> *) -> * -> * -> * where
   MkFirst : p (f a) b -> First f p a b

data Second : (* -> *) -> (* -> * -> *) -> * -> * -> * where
   MkSecond : p a (g b) -> First g p a b

data Both : (* -> *) -> (* -> *) -> (* -> * -> *) -> * -> * -> * where
   MkBoth : p (f a) (g a) -> Both f g p a b


toNat : Fix Maybe -> Nat
toNat f = case f of
  MkFix m -> case m of
    Nothing -> Z
    Just f1 -> S (toNat f1)

fromNat : Nat -> Fix Maybe
fromNat n =
  MkFix (
    case n of
      Z -> Nothing
      S n1 -> Just (fromNat n1)
  )

sizeF : Fix List -> Nat
sizeF f = case f of
  MkFix xs -> S (sum (map sizeF xs))

depthF : Fix List -> Nat
depthF f = case f of
  MkFix xs -> succ (maximum 0 (map depthF xs))




test0 : Id (Id ())
test0 = MkId (MkId ())

test1 : Const Bool StateT
test1 = MkConst True

test2 : Apply (Const Ordering) List
test2 = MkApply (MkConst EQ)

test3 : Flip Either Bool Nat
test3 = MkFlip (Left 0)

test4 : Compose List Maybe Bool
test4 = MkCompose [Just True]

test5 : Fix List
test5 = MkFix [ MkFix []
              , MkFix [ MkFix []
                      , MkFix [ MkFix [] ]
                      , MkFix [], MkFix [] ]
              , MkFix [] ]

test6 : On Pair List Nat Bool
test6 = MkOn ([1, 2], [False])

test7 : First List Const Bool Unit
test7 = MkFirst (MkConst undefined)
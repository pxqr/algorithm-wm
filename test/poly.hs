module PolyTest where

undefined : a
undefined = _|_

id x = x
const a b = a
flip f a b = f b a
compose f g x = f (g x)
apply f x = f x
save x f = f x

coerse : a -> b
coerse = undefined

unifiable : a -> a -> a
unifiable = const

polyLet = let id = \ x. x in
            id id id id id id (id id) id ((id id) id) id id id id id
          end

assert1 = unifiable 1 2
assert2 = unifiable id polyLet
assert3 = unifiable coerse id

main = 0
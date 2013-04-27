module ParserTest where

data Int : *
data State : * -> (* -> *) -> *

caseTest1 = case 0 of
  1 -> 'b'
  _ -> let id = \ x. x in id end

caseTest2 = case let f = \ x. x in 1 end of a -> a

main = caseTest1
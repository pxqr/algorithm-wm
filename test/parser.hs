data Int : *;
data State : * -> (* -> *) -> *;

caseTest = case let int = 1 in (\a. \b. a) 10 'a' end of {
             0 => 'a';
             1 => 'b';
             _ => let id = \x. x in id 'c' end
           };

main = caseTest;
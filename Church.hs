{- Church Encodings  -
 - - - - - - - - - - - 
 - By Risto Stevcev  -}

module Church where
import Prelude hiding (succ, pred, and, or, not, exp)


-- Church Boolean (True)
-- λt.λf.t
true :: a -> a -> a
true = \t -> \f -> t

-- Church Boolean (False)
-- λt.λf.f
false :: a -> a -> a
false = \t -> \f -> f

-- Church AND
-- λa.λb.a b false
and :: (a1 -> (a -> a -> a) -> a0) -> a1 -> a0
and = \a -> \b -> a b false

-- Church OR
-- λa.λb.a true b
or :: ((a -> a -> a) -> a1 -> a0) -> a1 -> a0
or = \a -> \b -> a true b

-- Church NOT
-- λp.λa.λb.p b a
not = \p -> \a -> \b -> p b a

-- Church XOR
-- λa.λb.a (not b) b
xor = \a -> \b -> a (not b) b


-- Church Numeral - 0
-- λf.λx.x
zero :: (a -> a) -> a -> a
zero = \f -> \x -> x

-- Church Numeral - 1
-- λf.λx.f x
one :: (a -> a) -> a -> a
one = \f -> \x -> f x

-- Church Numeral - 2
-- λf.λx.f (f x)
two :: (a -> a) -> a -> a
two = \f -> \x -> f (f x)

-- Church Numeral - 3
-- λf.λx.f (f (f x))
three :: (a -> a) -> a -> a
three = \f -> \x -> f (f (f x))

-- Church Numeral - n (any)
-- num 0 = λf.λx.x
-- num n = λf.λx.f (num (n-1) f x)
num :: Integer -> (a -> a) -> a -> a;
num 0 = \f -> \x -> x;
num n = \f -> \x -> f (num (n-1) f x)


-- Church Conditional (If/Else)
-- λp.λa.λb.p a b
ifelse :: (a -> a -> a) -> a -> a -> a
ifelse = \p -> \a -> \b -> p a b

-- Church Comparison (Check if 0)
-- λn.n (λx.false) true
is_zero :: ((a2 -> a -> a -> a) -> (a1 -> a1 -> a1) -> a0) -> a0
is_zero = \n -> n (\x -> false) true

-- Church Comparison (<=)
{- λm.λn.is_zero (sub m n)
 - Requires working sub:
leq = \m -> \n -> is_zero (sub m n)
 -} 

-- Church Comparison (==)
{- λm.λn.and (leq m n) (leq n m)
 - Rquires working sub:
eq = \m -> \n -> and (leq m n) (leq n m) 
 -}


-- Convert Church Numeral to Haskell Integer
-- λa.a (λb.b+1) (0)
unchurch_num :: ((Integer -> Integer) -> Integer -> a) -> a
unchurch_num = \a -> a (\b -> b + 1) (0)

-- Convert Church Boolean to Haskell Bool
-- (λa.λb.λc.c a b) True False
unchurch_bool :: (Bool -> Bool -> a) -> a
unchurch_bool = (\a -> \b -> \c -> c a b) True False


-- Church Successor
-- λn.λf.λx.f (n f x)
succ :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a 
succ = \n -> \f -> \x -> f (n f x)

-- Church Predecessor
-- λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
pred
  :: (((a3 -> a2) -> (a2 -> a1) -> a1)
     -> (a4 -> a5) -> (a6 -> a6) -> a)
     -> a3 -> a5 -> a
pred = \n -> \f -> \x -> n (\g -> \h -> h (g f)) (\u -> x) (\u -> u)


-- Church Addition
-- λm.λn.λf.λx.m f (n f x)
add :: (a2 -> a1 -> a) -> (a2 -> a3 -> a1) -> a2 -> a3 -> a
add = \m -> \n -> \f -> \x -> m f (n f x)

-- Church Subtraction
-- λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
{- Might be tempted to write:

sub
  :: (((a3 -> a2) -> (a2 -> a1) -> a1)
      -> (a4 -> a5) -> (a6 -> a6) -> a)
     -> a3 -> a5 -> a
sub = \n -> \f -> \x -> n (\g -> \h -> h (g f)) (\u -> x) (\u -> u)

 - But this solution is too polymorphic for Haskell's Hindley-Milner type 
 - inference to handle. There is a solution, but it's hairy. More on this
 - some other time. (Church himself didn't think this was possible) 
 -}

-- Church Multiplication
-- λm.λn.λf.m (n f)
mult :: (a1 -> a) -> (a2 -> a1) -> a2 -> a
mult = \m -> \n -> \f -> m (n f)

-- Church division
{- λc.λn.λm.λf.λx.(λd.is_zero d (0 f x) (f (c d m f x))) (sub n m)
 - Rquires working sub:
div = \c -> \n -> \m -> \f -> \x -> (\d -> is_zero d (0 f x) (f (c d m f x))) (sub n m)
 -}

-- Church Exponentiation
-- λm.λn.n m
exp :: a1 -> (a1 -> a) -> a
exp = \m -> \n -> n m


-- Church Pairs
-- λx.λy.λz.z x y
pair :: a1 -> a2 -> (a1 -> a2 -> a) -> a
pair = \x -> \y -> \z -> z x y

-- Church Pairs (first item)
-- λp.p -> (λx.λy.x)
first :: ((a2 -> a1 -> a2) -> a) -> a
first = \p -> p (\x -> \y -> x)

-- Church Pairs (second item)
-- λp.p -> (λx.λy.y)
second :: ((a1 -> a2 -> a2) -> a) -> a
second = \p -> p (\x -> \y -> y)

nil :: ((a1 -> a1 -> a1) -> (a2 -> a2 -> a2) -> a) -> a
nil = pair true true

is_nil :: ((a2 -> a1 -> a2) -> a) -> a
is_nil = first

cons :: a2 -> a3 -> ((a1 -> a1 -> a1) -> ((a2 -> a3 -> a4) -> a4) -> a) -> a
cons = \h -> \t -> pair false (pair h t)

head :: ((a3 -> a4 -> a4) -> (a2 -> a1 -> a2) -> a) -> a
head = \z -> first (second z)

tail :: ((a3 -> a4 -> a4) -> (a1 -> a2 -> a2) -> a) -> a
tail = \z -> second (second z)

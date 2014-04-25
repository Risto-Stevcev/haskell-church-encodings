{-# LANGUAGE RankNTypes #-}
{- Church Encodings  -
 - - - - - - - - - - - 
 - By Risto Stevcev  -}

module Church where
import Prelude hiding (succ, pred, and, or, not, exp)


type ChurchBool = forall a. a -> a -> a

-- Church Boolean (True)
-- λt.λf.t
true :: ChurchBool
true = \t -> \f -> t

-- Church Boolean (False)
-- λt.λf.f
false :: ChurchBool
false = \t -> \f -> f

-- Church AND
-- λa.λb.a b false
and :: ChurchBool -> ChurchBool -> ChurchBool
and = \a -> \b -> a b false

-- Church OR
-- λa.λb.a true b
or :: ChurchBool -> ChurchBool -> ChurchBool
or = \a -> \b -> a true b

-- Church NOT
-- λp.λa.λb.p b a
not :: ChurchBool -> ChurchBool
not = \p -> \a -> \b -> p b a

-- Church XOR
-- λa.λb.a (not b) b
xor :: ChurchBool -> ChurchBool -> ChurchBool
xor = \a -> \b -> a (not b) b


-- Rolling/unrolling the ChurchNum is required for Church subtraction 
-- The type system isn't powerful enough to handle it otherwise.
newtype Church = Church { unChurch :: Church.ChurchNum }

type ChurchNum = forall a. (a -> a) -> a -> a

-- Church Numeral: 0
-- λf.λx.x
zero :: Church
zero = Church $ \f -> \x -> x

-- Church Numeral: 1
-- λf.λx.f x
one :: Church
one = Church $ \f -> \x -> f x

-- Church Numeral: 2
-- λf.λx.f (f x)
two :: Church
two = Church $ \f -> \x -> f (f x)

-- Church Numeral: 3
-- λf.λx.f (f (f x))
three :: Church
three = Church $ \f -> \x -> f (f (f x))

-- Church Numeral: n (where n ∈ ℕ)
-- num 0 = λf.λx.x
-- num n = λf.λx.f (num (n-1) f x)
num :: Integer -> Church
num 0 = Church $ \f -> \x -> x;
num n = Church $ \f -> \x -> f ((unChurch $ num (n-1)) f x)

-- Church Conditional (If/Else)
-- λp.λa.λb.p a b
ifelse :: ChurchBool -> a -> a -> a;
ifelse = \p -> \a -> \b -> p a b


-- Convert Church Numeral to Haskell Integer
-- λa.a (λb.b+1) (0)
unchurch_num :: Church -> Integer
unchurch_num = \a -> unChurch a (\b -> b + 1) (0)

-- Convert Church Boolean to Haskell Bool
-- (λa.λb.λc.c a b) True False
unchurch_bool :: (Bool -> Bool -> a) -> a
unchurch_bool = (\a -> \b -> \c -> c a b) True False


-- Y Combinator
-- Y = λf.(λx.f (x x)) (λx.f (x x))
--
-- Beta reduction of this gives,
-- Y g = (λf.(λx.f (x x)) (λx.f (x x))) g
--     = (λx.g (x x)) (λx.g (x x))
--     = g((λx.g (x x)) (λx.g (x x)))
--     = g (Y g)
y g = g (y g)

-- A non-recursive version of the Y combinator
newtype Mu a = Mu (Mu a -> a)
ynr f = (\h -> h $ Mu h) (\x -> f . (\(Mu g) -> g) x $ x)


-- Church Successor
-- λn.λf.λx.f (n f x)
succ :: Church -> Church;
succ = \n -> Church $ \f -> \x -> f (unChurch n f x)

-- Church Predecessor
-- λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
pred :: Church -> Church
pred = \n -> Church $ 
       \f -> \x -> unChurch n (\g -> \h -> h (g f)) (\u -> x) (\u -> u)

-- Church Addition
-- λm.λn.λf.λx.m f (n f x)
add :: Church -> Church -> Church
add = \m -> \n -> Church $ \f -> \x -> unChurch m f (unChurch n f x)

-- Church Subtraction
-- λm.λn. n pred m
sub :: Church -> Church -> Church
sub = \m -> \n -> unChurch n pred m

-- Church Multiplication
-- λm.λn.λf.m (n f)
mult :: Church -> Church -> Church
mult = \m -> \n -> Church $ \f -> unChurch m (unChurch n f)

-- Church Division
-- λd n m.ifelse (geq n m) (succ (d (sub n m) m)) zero
div :: Church -> Church -> Church
div = y (\d n m -> ifelse (geq n m) (succ (d (sub n m) m)) zero)

-- Church Exponentiation
-- λm.λn.n m
exp :: Church -> Church -> Church;
exp = \m -> \n -> Church $ (unChurch n) (unChurch m)

-- Church Factorial
-- \f n.ifelse (is_zero n) one (mult n (fac (pred n)))
fac :: Church -> Church
fac = y (\f n -> ifelse (is_zero n) one (mult n (fac (pred n))))


-- Church Comparison (== 0)
-- λn.n (λx.false) true
is_zero :: Church -> ChurchBool;
is_zero = \n -> unChurch n (\x -> Church.false) Church.true

-- Church Comparison (<)
-- λm.λn.and (is_zero (sub m n)) (not (is_zero (sub n m)))
lt :: Church -> Church -> ChurchBool
lt = \m -> \n -> and (is_zero $ sub m n) (not (is_zero $ sub n m))

-- Church Comparison (<=)
-- λm.λn.is_zero (sub m n)
leq :: Church -> Church -> ChurchBool
leq = \m -> \n -> is_zero (sub m n)

-- Church Comparison (==)
-- λm.λn.and (leq m n) (leq n m)
eq :: Church -> Church -> ChurchBool
eq = \m -> \n -> and (leq m n) (leq n m) 

-- Church Comparison (>=)
-- λm.λn.or (not (leq m n)) (eq m n)
geq :: Church -> Church -> ChurchBool
geq = \m -> \n -> or (not (leq m n)) (eq m n)

-- Church Comparison (>)
-- λm.λn.not (leq m n)
gt :: Church -> Church -> ChurchBool;
gt = \m -> \n -> not (leq m n)


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

-- Church Pairs (nil)
-- pair true true
nil :: ((a1 -> a1 -> a1) -> (a2 -> a2 -> a2) -> a) -> a
nil = pair true true

-- Church Comparison (is_nil)
-- first (true for nil pair)
is_nil :: ((a2 -> a1 -> a2) -> a) -> a
is_nil = first

-- Church Cons
-- λh.λt.pair false (pair h t)
cons :: a2 -> a3 -> ((a1 -> a1 -> a1) -> ((a2 -> a3 -> a4) -> a4) -> a) -> a
cons = \h -> \t -> pair false (pair h t)

-- Church Head
-- λz.first (second z)
head :: ((a3 -> a4 -> a4) -> (a2 -> a1 -> a2) -> a) -> a
head = \z -> first (second z)

-- Church Tail
-- λz.second (second z)
tail :: ((a3 -> a4 -> a4) -> (a1 -> a2 -> a2) -> a) -> a
tail = \z -> second (second z)

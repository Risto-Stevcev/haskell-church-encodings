{-# LANGUAGE RankNTypes #-}
{- Church Encodings  -
 - - - - - - - - - - - 
 - By Risto Stevcev  -}

module Church where
import Prelude hiding (succ, pred, and, or, not, exp, div, head, tail)



{- Church Logical Operators  -
 - - - - - - - - - - - - - - -}

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

-- Convert Church Boolean to Haskell Bool
-- (λa.λb.λc.c a b) True False
unchurch_bool :: (Bool -> Bool -> a) -> a
unchurch_bool = (\a -> \b -> \c -> c a b) True False



{- Church Natural Numbers (n ∈ ℕ)  -
 - - - - - - - - - - - - - - - - - -}

-- Rolling/unrolling the ChurchNum is required for Church subtraction 
-- The type system isn't powerful enough to handle it otherwise.
--
type ChurchNum = forall a. (a -> a) -> a -> a
newtype Church = Church { unChurch :: ChurchNum }

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
num 0 = Church $ \f -> \x -> x
num n = Church $ \f -> \x -> f ((unChurch $ num (n-1)) f x)

-- Convert Church Numeral (n ∈ ℕ) to Haskell Integer
-- λa.a (λb.b+1) (0)
unchurch_num :: Church -> Integer
unchurch_num = \a -> unChurch a (\b -> b + 1) (0)



{- Church Conditionals -
 - - - - - - - - - - - -}

-- Church Conditional (If/Else)
-- λp.λa.λb.p a b
ifelse :: ChurchBool -> a -> a -> a
ifelse = \p -> \a -> \b -> p a b



{- Church Loops -
 - - - - - - - - -}

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



{- Church Arithmetic Operators (n ∈ ℕ) -
 - - - - - - - - - - - - - - - - - - - -}

-- Church Successor
-- λn.λf.λx.f (n f x)
succ :: Church -> Church

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

-- Church Division (gets the floor if divides to a fraction)
-- λd n m.ifelse (geq n m) (succ (d (sub n m) m)) zero
div :: Church -> Church -> Church
div = y (\d n m -> ifelse (geq n m) (succ (d (sub n m) m)) zero)

-- Church Exponentiation
-- λm.λn.n m
exp :: Church -> Church -> Church
exp = \m -> \n -> Church $ (unChurch n) (unChurch m)

-- Church Factorial
-- λf n.ifelse (is_zero n) one (mult n (fac (pred n)))
fac :: Church -> Church
fac = y (\f n -> ifelse (is_zero n) one (mult n $ f $ pred n))



{- Church Comparison Operators -
 - - - - - - - - - - - - - - - -}

-- Church Comparison (== 0)
-- λn.n (λx.false) true
is_zero :: Church -> ChurchBool
is_zero = \n -> unChurch n (\x -> false) true

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
gt :: Church -> Church -> ChurchBool
gt = \m -> \n -> not (leq m n)



{- Church Lists  - 
 - - - - - - - - -}

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



{- Church Tuples -
 - - - - - - - - -}

-- Tuples differ from lists in two ways,
-- 1. Elements in a tuple can only be of one type (in this case, a ChurchBool or a ChurchNum)
-- 2. Tuples are fixed in size
data ChurchElem = ChurchNumber Church | ChurchBoolean ChurchBool
type ChurchTuple2 = forall a. (ChurchElem -> ChurchElem -> ChurchElem) -> ChurchElem

-- Church Tuple (of size 2)
tuple2 :: ChurchElem -> ChurchElem -> ChurchTuple2
tuple2 = \x -> \y -> \z -> z x y

tuple2_first :: ChurchTuple2 -> ChurchElem
tuple2_first = \p -> p (\x -> \y -> x)

tuple2_second :: ChurchTuple2 -> ChurchElem
tuple2_second = \p -> p (\x -> \y -> y)

unchurch_bool_elem :: ChurchElem -> Bool
unchurch_bool_elem (ChurchBoolean x) = unchurch_bool x

unchurch_num_elem :: ChurchElem -> Integer
unchurch_num_elem (ChurchNumber x) = unchurch_num x



{- Church Integers (n ∈ ℤ) -
 - - - - - - - - - - - - - -}

type ChurchInteger = forall a. (Church -> Church -> Church) -> Church


-- Convert Church Numeral (natural number) to Church Integer
-- λx.pair x zero
convertNZ :: Church -> ChurchInteger
convertNZ = \x -> pair x zero

-- Church Negation
-- λx.pair (second x) (first x)
neg :: ChurchInteger -> ChurchInteger
neg = \x -> pair (second x) (first x)

-- Church OneZero 
-- (Fixes incorrect integer representations that don't have a zero in the pair. 
-- Ex: (7, 2) == 7 - 2 == 5) 
-- λoneZ x.ifelse (is_zero (first x)) 
--   x (ifelse (is_zero (second x)) x (oneZ (pair (pred (first x)) (pred (second x)))))
onezero :: ChurchInteger -> ChurchInteger
onezero  = y ( \oneZ x -> ifelse (is_zero $ first x) x 
                            (ifelse (is_zero $ second x) x 
                              (oneZ $ pair (pred $ first x) (pred $ second x))) )

-- Convert Church Integer to Haskell Integer
-- λx.ifelse (is_zero (first x)) (-1*(unchurch_num (second x))) 
--                               (unchurch_num (first x))
unchurch_int :: ChurchInteger -> Integer
unchurch_int = \x -> ifelse (is_zero (first x)) 
                       ((-1)*(unchurch_num $ second x)) (unchurch_num $ first x)



{- Church Arithmetic Operators (n ∈ ℤ) -
 - - - - - - - - - - - - - - - - - - - -}

-- Church Addition
-- λx.λy.onezero (pair (add (first x) (first y)) (add (second x) (second y)))
addZ :: ChurchInteger -> ChurchInteger -> ChurchInteger
addZ = \x -> \y -> onezero (pair (add (first x) (first y)) (add (second x) (second y)))

-- Church Subtraction
-- λx.λy.onezero (pair (add (first x) (second y)) (add (second x) (first y)))
subZ :: ChurchInteger -> ChurchInteger -> ChurchInteger
subZ = \x -> \y -> onezero (pair (add (first x) (second y)) (add (second x) (first y)))

-- Church Multiplication
-- λx.λy.pair (add (mult (first x) (first y)) (mult (second x) (second y)))
--            (add (mult (first x) (second y)) (mult (second x) (first y)))
multZ :: ChurchInteger -> ChurchInteger -> ChurchInteger
multZ = \x -> \y -> pair (add (mult (first x) (first y)) (mult (second x) (second y))) 
                         (add (mult (first x) (second y)) (mult (second x) (first y)))

-- Church DivNoZero
-- (Divides only if the value is not zero)
-- λx.λy.is_zero y zero (div x y)
divnZ :: Church -> Church -> Church
divnZ = \x -> \y -> is_zero y zero (div x y)

-- Church Division
-- λx.λy.pair (add (divnZ (first x) (first y)) (divnZ (second x) (second y)))
--            (add (divnZ (first x) (second y)) (divnZ (second x) (first y)))
divZ :: ChurchInteger -> ChurchInteger -> ChurchInteger
divZ = \x -> \y -> pair (add (divnZ (first x) (first y)) (divnZ (second x) (second y)))
                        (add (divnZ (first x) (second y)) (divnZ (second x) (first y)))

-- Church Boolean (True)
-- λt.λf.t
church_true :: a -> a -> a
church_true = \t -> \f -> t

-- Church Boolean (False)
-- λt.λf.f
church_false :: a -> a -> a
church_false = \t -> \f -> f

-- Church AND
-- λa.λb.a b church_false
church_and :: (a1 -> (a -> a -> a) -> a0) -> a1 -> a0
church_and = \a -> \b -> a b church_false

-- Church OR
-- λa.λb.a church_true b
church_or :: ((a -> a -> a) -> a1 -> a0) -> a1 -> a0
church_or = \a -> \b -> a church_true b

-- Church NOT
-- λp.λa.λb.p b a
church_not = \p -> \a -> \b -> p b a

-- Church XOR
-- λa.λb.a (church_not b) b
church_xor = \a -> \b -> a (church_not b) b


-- Church Numeral - 0
-- λf.λx.x
church_zero :: (a -> a) -> a -> a
church_zero = \f -> \x -> x

-- Church Numeral - 1
-- λf.λx.f x
church_one :: (a -> a) -> a -> a
church_one = \f -> \x -> f x

-- Church Numeral - 2
-- λf.λx.f (f x)
church_two :: (a -> a) -> a -> a
church_two = \f -> \x -> f (f x)

-- Church Numeral - 3
-- λf.λx.f (f (f x))
church_three :: (a -> a) -> a -> a
church_three = \f -> \x -> f (f (f x))

-- Church Numeral - n (any)
-- church_num 0 = λf.λx.x
-- church_num n = λf.λx.f (church_num (n-1) f x)
church_num :: Integer -> (a -> a) -> a -> a;
church_num 0 = \f -> \x -> x;
church_num n = \f -> \x -> f (church_num (n-1) f x)


-- Church Conditional (If/Else)
-- λp.λa.λb.p a b
church_ifelse :: (a -> a -> a) -> a -> a -> a
church_ifelse = \p -> \a -> \b -> p a b

-- Church Comparison (Check if 0)
-- λn.n (λx.church_false) church_true
church_is_zero :: ((a2 -> a -> a -> a) -> (a1 -> a1 -> a1) -> a0) -> a0
church_is_zero = \n -> n (\x -> church_false) church_true

-- Church Comparison (<=)
{- λm.λn.church_is_zero (church_sub m n)
 - Requires working church_sub:
church_leq = \m -> \n -> church_is_zero (church_sub m n)
 -} 

-- Church Comparison (==)
{- λm.λn.church_and (church_leq m n) (church_leq n m)
 - Rquires working church_sub:
church_eq = \m -> \n -> church_and (church_leq m n) (church_leq n m) 
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
church_succ :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a 
church_succ = \n -> \f -> \x -> f (n f x)

-- Church Predecessor
-- λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
church_pred
  :: (((a3 -> a2) -> (a2 -> a1) -> a1)
      -> (a4 -> a5) -> (a6 -> a6) -> a)
     -> a3 -> a5 -> a
church_pred = \n -> \f -> \x -> n (\g -> \h -> h (g f)) (\u -> x) (\u -> u)


-- Church Addition
-- λm.λn.λf.λx.m f (n f x)
church_add :: (a2 -> a1 -> a) -> (a2 -> a3 -> a1) -> a2 -> a3 -> a
church_add = \m -> \n -> \f -> \x -> m f (n f x)

-- Church Subtraction
-- λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
{- Might be tempted to write:

church_sub
  :: (((a3 -> a2) -> (a2 -> a1) -> a1)
      -> (a4 -> a5) -> (a6 -> a6) -> a)
     -> a3 -> a5 -> a
church_sub = \n -> \f -> \x -> n (\g -> \h -> h (g f)) (\u -> x) (\u -> u)

 - But this solution is too polymorphic for Haskell's Hindley-Milner type 
 - inference to handle. There is a solution, but it's hairy. More on this
 - some other time. (Church himself didn't think this was possible) 
 -}

-- Church Multiplication
-- λm.λn.λf.m (n f)
church_mult :: (a1 -> a) -> (a2 -> a1) -> a2 -> a
church_mult = \m -> \n -> \f -> m (n f)

-- Church division
{- λc.λn.λm.λf.λx.(λd.church_is_zero d (0 f x) (f (c d m f x))) (church_sub n m)
 - Rquires working church_sub:
church_div = \c -> \n -> \m -> \f -> \x -> (\d -> church_is_zero d (0 f x) (f (c d m f x))) (church_sub n m)
 -}

-- Church Exponentiation
-- λm.λn.n m
church_exp :: a1 -> (a1 -> a) -> a
church_exp = \m -> \n -> n m

{- 
main = do
  mapM_ print
    [
      (unchurch_bool (church_true)),                   -- True
      (unchurch_bool (church_false)),                  -- False
      ((unchurch_num (church_succ church_zero)) == 1), -- True
      ((unchurch_num (church_succ church_one)) == 2),  -- True
      ((unchurch_num (church_succ church_two)) == 3),  -- True
      ((unchurch_num (church_succ church_three)) == 4) -- True
    ]
  mapM_ print
    [
      (church_true 2.5 3.2),              -- 2.5
      (church_false 5 6),                 -- 6
      (church_ifelse church_true 42 58),  -- 42 
      (church_ifelse church_false 42 58)  -- 58
    ]
  mapM_ print
    [
      (unchurch_num church_zero),               -- 0
      (unchurch_num church_one),                -- 1
      (unchurch_num church_two),                -- 2
      (unchurch_num church_three),              -- 3
      (unchurch_num (church_succ church_three)) -- 4
      (unchurch_num (church_plus church_two church_two)) -- 4
      (unchurch_num (church_plus church_one church_three)) -- 4
    ]
-}

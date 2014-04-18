-- λt. λf. t
church_true :: a -> a -> a
church_true = \x -> \y -> x

-- λt. λf. f
church_false :: a -> a -> a
church_false = \x -> \y -> y


church_ifelse :: (a -> a -> a) -> a -> a -> a
church_ifelse = \p -> \a -> \b -> p a b

church_zero :: (a -> a) -> a -> a
church_zero = \f -> \x -> x

church_one :: (a -> a) -> a -> a
church_one = \f -> \x -> f x

church_two :: (a -> a) -> a -> a
church_two = \f -> \x -> f (f x)

church_three :: (a -> a) -> a -> a
church_three = \f -> \x -> f (f (f x))

church_succ :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a 
church_succ = \n -> \f -> \x -> f (n f x)

unchurch_num :: ((Integer -> Integer) -> Integer -> a) -> a
unchurch_num = \a -> a (\b -> b + 1) (0)

church_cons :: a -> a -> (a -> a -> a) -> a
church_cons = \a -> \b -> \c -> c a b

unchurch_bool :: (Bool -> Bool -> Bool) -> Bool
unchurch_bool = church_cons True False

--{- 
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
    ]
---}

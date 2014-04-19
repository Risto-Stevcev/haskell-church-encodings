{- Church Encoding Tests -
 - - - - - - - - - - - - - 
 - By Risto Stevcev      -}

import Church


main = do
  mapM_ print
    [
      ("true == True", Church.unchurch_bool (Church.true) == True),
      ("true 2.5 3.2 == 2.5", Church.true 2 3 == 2),
      ("false == False", Church.unchurch_bool (Church.false) == False),
      ("false 5 6 == 6", Church.false 5 6 == 6),

      ("succ zero == 1", Church.unchurch_num (Church.succ Church.zero) == 1),
      ("succ one == 2", Church.unchurch_num (Church.succ Church.one) == 2),
      ("succ two == 3", Church.unchurch_num (Church.succ Church.two) == 3),
      ("succ three == 4", Church.unchurch_num (Church.succ Church.three) == 4),

      ("ifelse true 42 58 == 42", Church.ifelse Church.true 42 58 == 42), 
      ("ifelse false 42 58 == 58", Church.ifelse Church.false 42 58 == 58),

      ("zero == 0", Church.unchurch_num Church.zero == 0),
      ("one == 1", Church.unchurch_num Church.one == 1),
      ("two == 2", Church.unchurch_num Church.two == 2),
      ("three == 3", Church.unchurch_num Church.three == 3),
      ("succ three == 4", Church.unchurch_num (Church.succ Church.three) == 4),

      ("add two two == 4", Church.unchurch_num (Church.add Church.two Church.two) == 4),
      ("add one three == 4", Church.unchurch_num (Church.add Church.one Church.three) == 4)
    ]


{-
*Main> let l = church_cons church_three (church_cons church_two (church_cons church_one (church_nil)))
*Main> unchurch_bool (church_is_nil (church_head (church_tail (church_tail (church_tail l)))))
True
*Main> unchurch_num (church_head (church_tail (church_tail l)))
1
*Main> unchurch_num (church_head (church_tail l))
2
*Main> unchurch_num (church_head l)
3
-}

{- Church Encoding Tests -
 - - - - - - - - - - - - - 
 - By Risto Stevcev      -}

import Test.HUnit
import Church
import Prelude hiding (succ, pred, and, or, not, exp, head, tail)


test_true = TestCase $ assertBool "true == True" (unchurch_bool true)
test_true_num = TestCase $ assertEqual "true 2.5 3.2 == 2.5" 2.5 (true 2.5 3.2)
test_false = TestCase $ assertEqual "false == False" False (unchurch_bool false)
test_false_num = TestCase $ assertEqual "false 5 6 == 6" 6 (false 5 6)

tests_bools = 
  TestList [TestLabel "test true" test_true,
            TestLabel "test true number" test_true_num,
            TestLabel "test false" test_false,
            TestLabel "test false number" test_false_num]

test_true_and_true = TestCase $ assertBool "true and true == True" 
  (unchurch_bool $ and true true)
test_true_and_false = TestCase $ assertEqual "true and false == False"
  False (unchurch_bool $ and true false)
test_false_and_true = TestCase $ assertEqual "false and true == False"
  False (unchurch_bool $ and false true)
test_false_and_false = TestCase $ assertEqual "false and false == False"
  False (unchurch_bool $ and false false)

test_true_or_true = TestCase $ assertBool "true or true == True"
  (unchurch_bool $ or true true)
test_true_or_false = TestCase $ assertBool "true or false == True"
  (unchurch_bool $ or true false)
test_false_or_true = TestCase $ assertBool "false or true == True"
  (unchurch_bool $ or false true)
test_false_or_false = TestCase $ assertEqual "false or false == False"
  False (unchurch_bool $ or false false)

test_true_xor_true = TestCase $ assertEqual "true xor true == False"
  False (unchurch_bool $ xor true true)
test_true_xor_false = TestCase $ assertBool "true xor false == True"
  (unchurch_bool $ xor true false)
test_false_xor_true = TestCase $ assertBool "false xor true == True"
  (unchurch_bool $ xor false true)
test_false_xor_false = TestCase $ assertEqual "false xor false == False"
  False (unchurch_bool $ xor false false)

test_not_true = TestCase $ assertEqual "not true == False"
  False (unchurch_bool $ not true)
test_not_false = TestCase $ assertBool "not false == True"
  (unchurch_bool $ not false)
test_not_false_or_true = TestCase $ assertEqual "not false or true == False"
  False (unchurch_bool $ not $ or false true)
test_not_true_and_false = TestCase $ assertBool "not true and false == True"
  (unchurch_bool $ not $ and true false)
test_not_true_xor_true = TestCase $ assertBool "not true xor true == True"
  (unchurch_bool $ not $ xor true true)

test_ifelse_true = TestCase $ assertEqual "ifelse true two three == 2"
  2 (unchurch_num $ ifelse true two three)
test_ifelse_false = TestCase $ assertEqual "ifelse false two three == 3"
  3 (unchurch_num $ ifelse false two three) 

tests_conditionals = 
  TestList [TestLabel "test true and true" test_true_and_true,
            TestLabel "test true and false" test_true_and_false,
            TestLabel "test false and true" test_false_and_true,
            TestLabel "test false and false" test_false_and_false,

            TestLabel "test true or true" test_true_or_true,
            TestLabel "test true or false" test_true_or_false,
            TestLabel "test false or true" test_false_or_true,
            TestLabel "test false or false" test_false_or_false,

            TestLabel "test true xor true" test_true_xor_true,
            TestLabel "test true xor false" test_true_xor_false,
            TestLabel "test false xor true" test_false_xor_true,
            TestLabel "test false xor false" test_false_xor_false,

            TestLabel "test not true" test_not_true,
            TestLabel "test not false" test_not_false,
            TestLabel "test not false or true" test_not_false_or_true,
            TestLabel "test not true or false" test_not_true_and_false,
            TestLabel "test not true xor true" test_not_true_xor_true,

            TestLabel "test ifelse true" test_ifelse_true,
            TestLabel "test ifelse false" test_ifelse_false]

test_zero = TestCase $ assertEqual "numeral zero == 0"
  0 (unchurch_num zero)
test_one = TestCase $ assertEqual "numeral one == 1"
  1 (unchurch_num one)
test_two = TestCase $ assertEqual "numeral two == 2"
  2 (unchurch_num two)
test_three = TestCase $ assertEqual "numeral three == 3"
  3 (unchurch_num three)
test_num_12 = TestCase $ assertEqual "numeral three == 12"
  12 (unchurch_num $ num 12)
test_num_17 = TestCase $ assertEqual "numeral three == 17"
  17 (unchurch_num $ num 17)
test_num_123779 = TestCase $ assertEqual "numeral 123779 == 123779"
  123779 (unchurch_num $ num 123779)

tests_numerals = 
  TestList [TestLabel "test zero" test_zero,
            TestLabel "test one" test_one,
            TestLabel "test two" test_two,
            TestLabel "test three" test_three,
            TestLabel "test numeral 12" test_num_12,
            TestLabel "test numeral 17" test_num_17,
            TestLabel "test numeral 123779" test_num_123779]

test_if_zero = TestCase $ assertBool "is_zero zero == True"
  (unchurch_bool $ is_zero zero)

tests_comparisons =
  TestList [TestLabel "test if zero" test_if_zero]

test_succ_zero = TestCase $ assertEqual "succ zero == 1"
  1 (unchurch_num $ succ zero)
test_succ_129 = TestCase $ assertEqual "succ 129 == 130"
  130 (unchurch_num $ succ $ num 129)
test_pred_zero = TestCase $ assertEqual "pred zero == 0"
  0 (unchurch_num $ pred zero)
test_pred_one = TestCase $ assertEqual "pred one == 0"
  0 (unchurch_num $ pred one)
test_pred_130 = TestCase $ assertEqual "pred 130 == 129"
  129 (unchurch_num $ pred $ num 130)

test_add_zero_zero = TestCase $ assertEqual "add zero zero == 0"
  0 (unchurch_num $ add zero zero)
test_add_zero_one = TestCase $ assertEqual "add zero one == 1"
  1 (unchurch_num $ add zero one)
test_add_three_zero = TestCase $ assertEqual "add three zero == 3"
  3 (unchurch_num $ add three zero)
test_add_77_213 = TestCase $ assertEqual "add 77 213 == 290"
  290 (unchurch_num $ add (num 77) (num 213))
test_mult_zero_zero = TestCase $ assertEqual "mult zero zero == 0"
  0 (unchurch_num $ mult zero zero)
test_mult_three_zero = TestCase $ assertEqual "mult three zero == 0"
  0 (unchurch_num $ mult three zero)
test_mult_13_99 = TestCase $ assertEqual "mult 13 99 == 1287"
  1287 (unchurch_num $ mult (num 13) (num 99))
test_mult_1_121 = TestCase $ assertEqual "mult 1 121 == 121"
  121 (unchurch_num $ mult (num 1) (num 121))
test_exp_zero_zero = TestCase $ assertEqual "exp zero zero == 1"
  1 (unchurch_num $ exp zero zero)
test_exp_zero_3 = TestCase $ assertEqual "exp zero 3 == 0"
  0 (unchurch_num $ exp zero (num 3))
test_exp_2_8 = TestCase $ assertEqual "exp 2 8 == 256"
  256 (unchurch_num $ exp (num 2) (num 8))
test_exp_133_1 = TestCase $ assertEqual "exp 133 1 == 133"
  133 (unchurch_num $ exp (num 133) (num 1))

tests_operations =
  TestList [TestLabel "test succ zero" test_succ_zero,
            TestLabel "test succ 129" test_succ_129,
            TestLabel "test pred zero" test_pred_zero,
            TestLabel "test pred one" test_pred_one,
            TestLabel "test pred 130" test_pred_130,
            TestLabel "test add zero zero" test_add_zero_zero,
            TestLabel "test add zero one" test_add_zero_one,
            TestLabel "test add three zero" test_add_three_zero,
            TestLabel "test add 77 213" test_add_77_213,
            TestLabel "test mult zero zero" test_mult_zero_zero,
            TestLabel "test mult three zero" test_mult_three_zero,
            TestLabel "test mult 13 99" test_mult_13_99,
            TestLabel "test mult 1 121" test_mult_1_121,
            TestLabel "test exp zero zero" test_exp_zero_zero,
            TestLabel "test exp zero 3" test_exp_zero_3,
            TestLabel "test exp 2 8" test_exp_2_8,
            TestLabel "test exp 133 1" test_exp_133_1]

test_pair_zero_zero_first = TestCase $ assertEqual "first (pair zero zero) == 0"
  0 (unchurch_num $ first $ pair zero zero)
test_pair_zero_zero_second = TestCase $ assertEqual "second (pair zero zero) == 0"
  0 (unchurch_num $ second $ pair zero zero)
test_pair_5_12_first = TestCase $ assertEqual "first (pair 5 12) == 5"
  5 (unchurch_num $ first $ pair (num 5) (num 12))
test_pair_5_12_second = TestCase $ assertEqual "second (pair 5 12) == 5"
  12 (unchurch_num $ second $ pair (num 5) (num 12))
test_nil_pair_first = TestCase $ assertBool "first nil == true"
  (unchurch_bool $ first nil)
test_nil_pair_second = TestCase $ assertBool "second nil == true"
  (unchurch_bool $ second nil)
test_is_nil_nil = TestCase $ assertBool "is_nil nil == true"
  (unchurch_bool $ is_nil nil)

-- 133:412:7:[] == [133,412,7]
test_list = cons (num 133) (cons (num 412) (cons (num 7) nil))

test_list_node1 = TestCase $ assertEqual "node 1 (133 :412:7:[]) == 133"
  133 (unchurch_num $ head test_list)
test_list_node2 = TestCase $ assertEqual "node 2 (133: 412 :7:[]) == 412"
  412 (unchurch_num $ head $ tail test_list)
test_list_node3 = TestCase $ assertEqual "node 3 (133:412: 7 :[]) == 7"
  7 (unchurch_num $ head $ tail $ tail test_list)
test_list_node4 = TestCase $ assertBool "node 4 (133:412:7: []) == nil"
  (unchurch_bool $ is_nil $ head $ tail $ tail $ tail test_list)

tests_lists =
  TestList [TestLabel "test pair zero zero (first)" test_pair_zero_zero_first,
            TestLabel "test pair zero zero (second)" test_pair_zero_zero_second,
            TestLabel "test pair 5 12 (first)" test_pair_5_12_first,
            TestLabel "test pair 5 12 (second)" test_pair_5_12_second,
            TestLabel "test nil pair (first)" test_nil_pair_first,
            TestLabel "test nil pair (second)" test_nil_pair_second,
            TestLabel "test is_nil nil" test_is_nil_nil,
            TestLabel "test list node 1" test_list_node1,
            TestLabel "test list node 2" test_list_node2,
            TestLabel "test list node 3" test_list_node3,
            TestLabel "test list node 4" test_list_node4]          


main = do
  runTestTT tests_bools
  runTestTT tests_conditionals
  runTestTT tests_numerals
  runTestTT tests_comparisons
  runTestTT tests_operations
  runTestTT tests_lists

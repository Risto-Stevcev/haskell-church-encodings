{- Church Encoding Tests -
 - - - - - - - - - - - - - 
 - By Risto Stevcev      -}

import Test.HUnit
import Church


test_true = TestCase (assertBool "true == True" 
  (Church.unchurch_bool Church.true))
test_true_num = TestCase (assertEqual "true 2.5 3.2 == 2.5" 
  2.5 (Church.true 2.5 3.2))
test_false = TestCase (assertEqual "false == False"
  False (Church.unchurch_bool Church.false))
test_false_num = TestCase (assertEqual "false 5 6 == 6"
  6 (Church.false 5 6))

tests_bools = 
  TestList [TestLabel "test true" test_true,
            TestLabel "test true number" test_true_num,
            TestLabel "test false" test_false,
            TestLabel "test false number" test_false_num]

test_true_and_true = TestCase (assertBool "true and true == True"
  (Church.unchurch_bool (Church.and Church.true Church.true)))
test_true_and_false = TestCase (assertEqual "true and false == False"
  False (Church.unchurch_bool (Church.and Church.true Church.false)))
test_false_and_true = TestCase (assertEqual "false and true == False"
  False (Church.unchurch_bool (Church.and Church.false Church.true)))
test_false_and_false = TestCase (assertEqual "false and false == False"
  False (Church.unchurch_bool (Church.and Church.false Church.false)))

test_true_or_true = TestCase (assertBool "true or true == True"
  (Church.unchurch_bool (Church.or Church.true Church.true)))
test_true_or_false = TestCase (assertBool "true or false == True"
  (Church.unchurch_bool (Church.or Church.true Church.false)))
test_false_or_true = TestCase (assertBool "false or true == True"
  (Church.unchurch_bool (Church.or Church.false Church.true)))
test_false_or_false = TestCase (assertEqual "false or false == False"
  False (Church.unchurch_bool (Church.or Church.false Church.false)))

test_true_xor_true = TestCase (assertEqual "true xor true == False"
  False (Church.unchurch_bool (Church.xor Church.true Church.true)))
test_true_xor_false = TestCase (assertBool "true xor false == True"
  (Church.unchurch_bool (Church.xor Church.true Church.false)))
test_false_xor_true = TestCase (assertBool "false xor true == True"
  (Church.unchurch_bool (Church.xor Church.false Church.true)))
test_false_xor_false = TestCase (assertEqual "false xor false == False"
  False (Church.unchurch_bool (Church.xor Church.false Church.false)))

test_not_true = TestCase (assertEqual "not true == False"
  False (Church.unchurch_bool (Church.not Church.true)))
test_not_false = TestCase (assertBool "not false == True"
  (Church.unchurch_bool (Church.not Church.false)))
test_not_false_or_true = TestCase (assertEqual "not false or true == False"
  False (Church.unchurch_bool (Church.not
    (Church.or Church.false Church.true))))
test_not_true_and_false = TestCase (assertBool "not true and false == True"
  (Church.unchurch_bool (Church.not
    (Church.and Church.true Church.false))))
test_not_true_xor_true = TestCase (assertBool "not true xor true == True"
  (Church.unchurch_bool (Church.not
    (Church.xor Church.true Church.true))))

test_ifelse_true = TestCase (assertEqual "ifelse true two three == 2"
  2 (Church.unchurch_num 
    (Church.ifelse Church.true Church.two Church.three)))
test_ifelse_false = TestCase (assertEqual "ifelse false two three == 3"
  3 (Church.unchurch_num 
    (Church.ifelse Church.false Church.two Church.three))) 

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

test_zero = TestCase (assertEqual "numeral zero == 0"
  0 (Church.unchurch_num Church.zero))
test_one = TestCase (assertEqual "numeral one == 1"
  1 (Church.unchurch_num Church.one))
test_two = TestCase (assertEqual "numeral two == 2"
  2 (Church.unchurch_num Church.two))
test_three = TestCase (assertEqual "numeral three == 3"
  3 (Church.unchurch_num Church.three))
test_num_12 = TestCase (assertEqual "numeral three == 12"
  12 (Church.unchurch_num (Church.num 12)))
test_num_17 = TestCase (assertEqual "numeral three == 17"
  17 (Church.unchurch_num (Church.num 17)))
test_num_123779 = TestCase (assertEqual "numeral 123779 == 123779"
  123779 (Church.unchurch_num (Church.num 123779)))

tests_numerals = 
  TestList [TestLabel "test zero" test_zero,
            TestLabel "test one" test_one,
            TestLabel "test two" test_two,
            TestLabel "test three" test_three,
            TestLabel "test numeral 12" test_num_12,
            TestLabel "test numeral 17" test_num_17,
            TestLabel "test numeral 123779" test_num_123779]

test_if_zero = TestCase (assertBool "is_zero zero == True"
  (Church.unchurch_bool (Church.is_zero Church.zero)))

tests_comparisons =
  TestList [TestLabel "test if zero" test_if_zero]

test_succ_zero = TestCase (assertEqual "succ zero == 1"
  1 (Church.unchurch_num (Church.succ Church.zero)))
test_succ_129 = TestCase (assertEqual "succ 129 == 130"
  130 (Church.unchurch_num (Church.succ (Church.num 129))))
test_pred_zero = TestCase (assertEqual "pred zero == 0"
  0 (Church.unchurch_num (Church.pred Church.zero)))
test_pred_one = TestCase (assertEqual "pred one == 0"
  0 (Church.unchurch_num (Church.pred Church.one)))
test_pred_130 = TestCase (assertEqual "pred 130 == 129"
  129 (Church.unchurch_num (Church.pred (Church.num 130))))

test_add_zero_zero = TestCase (assertEqual "add zero zero == 0"
  0 (Church.unchurch_num (Church.add Church.zero Church.zero)))
test_add_zero_one = TestCase (assertEqual "add zero one == 1"
  1 (Church.unchurch_num (Church.add Church.zero Church.one)))
test_add_three_zero = TestCase (assertEqual "add three zero == 3"
  3 (Church.unchurch_num (Church.add Church.three Church.zero)))
test_add_77_213 = TestCase (assertEqual "add 77 213 == 290"
  290 (Church.unchurch_num (Church.add (Church.num 77) (Church.num 213))))
test_mult_zero_zero = TestCase (assertEqual "mult zero zero == 0"
  0 (Church.unchurch_num (Church.mult Church.zero Church.zero)))
test_mult_three_zero = TestCase (assertEqual "mult three zero == 0"
  0 (Church.unchurch_num (Church.mult Church.three Church.zero)))
test_mult_13_99 = TestCase (assertEqual "mult 13 99 == 1287"
  1287 (Church.unchurch_num (Church.mult (Church.num 13) (Church.num 99))))
test_mult_1_121 = TestCase (assertEqual "mult 1 121 == 121"
  121 (Church.unchurch_num (Church.mult (Church.num 1) (Church.num 121))))
test_exp_zero_zero = TestCase (assertEqual "exp zero zero == 1"
  1 (Church.unchurch_num (Church.exp Church.zero Church.zero)))
test_exp_zero_3 = TestCase (assertEqual "exp zero 3 == 0"
  0 (Church.unchurch_num (Church.exp Church.zero (Church.num 3))))
test_exp_2_8 = TestCase (assertEqual "exp 2 8 == 256"
  256 (Church.unchurch_num (Church.exp (Church.num 2) (Church.num 8))))
test_exp_133_1 = TestCase (assertEqual "exp 133 1 == 133"
  133 (Church.unchurch_num (Church.exp (Church.num 133) (Church.num 1))))

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

test_pair_zero_zero_first = TestCase (assertEqual "first (pair zero zero) == 0"
  0 (Church.unchurch_num (Church.first (Church.pair Church.zero Church.zero))))
test_pair_zero_zero_second = TestCase (assertEqual "second (pair zero zero) == 0"
  0 (Church.unchurch_num (Church.second (Church.pair Church.zero Church.zero))))
test_pair_5_12_first = TestCase (assertEqual "first (pair 5 12) == 5"
  5 (Church.unchurch_num (Church.first (Church.pair (Church.num 5) (Church.num 12)))))
test_pair_5_12_second = TestCase (assertEqual "second (pair 5 12) == 5"
  12 (Church.unchurch_num (Church.second (Church.pair (Church.num 5) (Church.num 12)))))
test_nil_pair_first = TestCase (assertBool "first nil == true"
  (Church.unchurch_bool (Church.first Church.nil)))
test_nil_pair_second = TestCase (assertBool "second nil == true"
  (Church.unchurch_bool (Church.second Church.nil)))
test_is_nil_nil = TestCase (assertBool "is_nil nil == true"
  (Church.unchurch_bool (Church.is_nil Church.nil)))

-- 133:412:7:[] == [133,412,7]
test_list = Church.cons (Church.num 133) (Church.cons (Church.num 412) (Church.cons (Church.num 7) Church.nil))

test_list_node1 = TestCase (assertEqual "node 1 (133 :412:7:[]) == 133"
  133 (Church.unchurch_num (Church.head test_list)))
test_list_node2 = TestCase (assertEqual "node 2 (133: 412 :7:[]) == 412"
  412 (Church.unchurch_num (Church.head (Church.tail test_list))))
test_list_node3 = TestCase (assertEqual "node 3 (133:412: 7 :[]) == 7"
  7 (Church.unchurch_num (Church.head (Church.tail (Church.tail test_list)))))
test_list_node4 = TestCase (assertBool "node 4 (133:412:7: []) == nil"
  (Church.unchurch_bool (Church.is_nil (Church.head (Church.tail (Church.tail (Church.tail test_list)))))))

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

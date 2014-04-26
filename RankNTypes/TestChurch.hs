{- Church Encoding Tests -
 - - - - - - - - - - - - - 
 - By Risto Stevcev      -}

import Test.HUnit
import Church
import Prelude hiding (succ, pred, and, or, not, exp, div, head, tail)



{- Test Logical Operators  -
 - - - - - - - - - - - - - -}

test_true = TestCase $ assertBool "true == True" (unchurch_bool true)
test_true_num = TestCase $ assertEqual "true 2.5 3.2 == 2.5" 2.5 (true 2.5 3.2)
test_false = TestCase $ assertEqual "false == False" False (unchurch_bool false)
test_false_num = TestCase $ assertEqual "false 5 6 == 6" 6 (false 5 6)

test_true_and_true = TestCase $ assertBool "true and true == true" 
  (unchurch_bool $ and true true)
test_true_and_false = TestCase $ assertEqual "true and false == false"
  False (unchurch_bool $ and true false)
test_false_and_true = TestCase $ assertEqual "false and true == false"
  False (unchurch_bool $ and false true)
test_false_and_false = TestCase $ assertEqual "false and false == false"
  False (unchurch_bool $ and false false)

test_true_or_true = TestCase $ assertBool "true or true == true"
  (unchurch_bool $ or true true)
test_true_or_false = TestCase $ assertBool "true or false == true"
  (unchurch_bool $ or true false)
test_false_or_true = TestCase $ assertBool "false or true == true"
  (unchurch_bool $ or false true)
test_false_or_false = TestCase $ assertEqual "false or false == false"
  False (unchurch_bool $ or false false)

test_not_true = TestCase $ assertEqual "not true == false"
  False (unchurch_bool $ not true)
test_not_false = TestCase $ assertBool "not false == true"
  (unchurch_bool $ not false)
test_not_false_or_true = TestCase $ assertEqual "not false or true == false"
  False (unchurch_bool $ not $ or false true)
test_not_true_and_false = TestCase $ assertBool "not true and false == true"
  (unchurch_bool $ not $ and true false)
test_not_true_xor_true = TestCase $ assertBool "not true xor true == true"
  (unchurch_bool $ not $ xor true true)

test_true_xor_true = TestCase $ assertEqual "true xor true == false"
  False (unchurch_bool $ xor true true)
test_true_xor_false = TestCase $ assertBool "true xor false == true"
  (unchurch_bool $ xor true false)
test_false_xor_true = TestCase $ assertBool "false xor true == true"
  (unchurch_bool $ xor false true)
test_false_xor_false = TestCase $ assertEqual "false xor false == false"
  False (unchurch_bool $ xor false false)


tests_logical_operators = 
  TestList [TestLabel "test true" test_true,
            TestLabel "test true number" test_true_num,
            TestLabel "test false" test_false,
            TestLabel "test false number" test_false_num,

            TestLabel "test true and true" test_true_and_true,
            TestLabel "test true and false" test_true_and_false,
            TestLabel "test false and true" test_false_and_true,
            TestLabel "test false and false" test_false_and_false,

            TestLabel "test true or true" test_true_or_true,
            TestLabel "test true or false" test_true_or_false,
            TestLabel "test false or true" test_false_or_true,
            TestLabel "test false or false" test_false_or_false,

            TestLabel "test not true" test_not_true,
            TestLabel "test not false" test_not_false,
            TestLabel "test not false or true" test_not_false_or_true,
            TestLabel "test not true or false" test_not_true_and_false,
            TestLabel "test not true xor true" test_not_true_xor_true,

            TestLabel "test true xor true" test_true_xor_true,
            TestLabel "test true xor false" test_true_xor_false,
            TestLabel "test false xor true" test_false_xor_true,
            TestLabel "test false xor false" test_false_xor_false]



{- Test Naturals (n ∈ ℕ) -
 - - - - - - - - - - - - -}

test_zero = TestCase $ assertEqual "numeral zero == 0"
  0 (unchurch_num zero)
test_one = TestCase $ assertEqual "numeral one == 1"
  1 (unchurch_num one)
test_two = TestCase $ assertEqual "numeral two == 2"
  2 (unchurch_num two)
test_three = TestCase $ assertEqual "numeral three == 3"
  3 (unchurch_num three)
test_num_12 = TestCase $ assertEqual "numeral 12 == 12"
  12 (unchurch_num $ num 12)
test_num_17 = TestCase $ assertEqual "numeral 17 == 17"
  17 (unchurch_num $ num 17)
test_num_123779 = TestCase $ assertEqual "numeral 123779 == 123779"
  123779 (unchurch_num $ num 123779)


tests_naturals = 
  TestList [TestLabel "test zero" test_zero,
            TestLabel "test one" test_one,
            TestLabel "test two" test_two,
            TestLabel "test three" test_three,
            TestLabel "test numeral 12" test_num_12,
            TestLabel "test numeral 17" test_num_17,
            TestLabel "test numeral 123779" test_num_123779]



{- Test Conditionals -
 - - - - - - - - - - -}

test_ifelse_true = TestCase $ assertEqual "ifelse true one three == 1"
  1 (unchurch_num $ ifelse true one three)
test_ifelse_false = TestCase $ assertEqual "ifelse false two three == 3"
  3 (unchurch_num $ ifelse false two three) 


tests_conditionals = 
  TestList [TestLabel "test ifelse true" test_ifelse_true,
            TestLabel "test ifelse false" test_ifelse_false]



{- Test Arithmetic Operators for Naturals (n ∈ ℕ)  -
 - - - - - - - - - - - - - - - - - - - - - - - - - -}

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

test_sub_zero_zero = TestCase $ assertEqual "sub zero zero == 0"
  0 (unchurch_num $ sub zero zero)
test_sub_zero_one = TestCase $ assertEqual "sub zero one == 0"
  0 (unchurch_num $ sub zero one)
test_sub_three_zero = TestCase $ assertEqual "sub three zero == 3"
  3 (unchurch_num $ sub three zero)
test_sub_213_77 = TestCase $ assertEqual "sub 213 77 == 136"
  136 (unchurch_num $ sub (num 213) (num 77))

test_mult_zero_zero = TestCase $ assertEqual "mult zero zero == 0"
  0 (unchurch_num $ mult zero zero)
test_mult_three_zero = TestCase $ assertEqual "mult three zero == 0"
  0 (unchurch_num $ mult three zero)
test_mult_13_99 = TestCase $ assertEqual "mult 13 99 == 1287"
  1287 (unchurch_num $ mult (num 13) (num 99))
test_mult_1_121 = TestCase $ assertEqual "mult 1 121 == 121"
  121 (unchurch_num $ mult (num 1) (num 121))

test_div_8_two = TestCase $ assertEqual "div 8 two == 4"
  4 (unchurch_num $ div (num 8) two)
test_div_zero_three = TestCase $ assertEqual "div zero three == 0"
  0 (unchurch_num $ div zero three)
test_div_99_4 = TestCase $ assertEqual "div 99 4 == 24"
  24 (unchurch_num $ div (num 99) (num 4))
test_div_1275_25 = TestCase $ assertEqual "div 1275 25 == 51"
  51 (unchurch_num $ div (num 1275) (num 25))

test_exp_zero_zero = TestCase $ assertEqual "exp zero zero == 1"
  1 (unchurch_num $ exp zero zero)
test_exp_zero_3 = TestCase $ assertEqual "exp zero 3 == 0"
  0 (unchurch_num $ exp zero (num 3))
test_exp_2_8 = TestCase $ assertEqual "exp 2 8 == 256"
  256 (unchurch_num $ exp (num 2) (num 8))
test_exp_133_1 = TestCase $ assertEqual "exp 133 1 == 133"
  133 (unchurch_num $ exp (num 133) (num 1))

test_fac_zero = TestCase $ assertEqual "fac zero = 1"
  1 (unchurch_num $ fac zero)
test_fac_one = TestCase $ assertEqual "fac one = 1"
  1 (unchurch_num $ fac one)
test_fac_three = TestCase $ assertEqual "fac three = 6"
  6 (unchurch_num $ fac three)
test_fac_5 = TestCase $ assertEqual "fac 5 = 120"
  120 (unchurch_num $ fac $ num 5)
test_fac_7 = TestCase $ assertEqual "fac 7 = 5040"
  5040 (unchurch_num $ fac $ num 7)


tests_arithmetic_operators_N =
  TestList [TestLabel "test succ zero" test_succ_zero,
            TestLabel "test succ 129" test_succ_129,

            TestLabel "test pred zero" test_pred_zero,
            TestLabel "test pred one" test_pred_one,
            TestLabel "test pred 130" test_pred_130,

            TestLabel "test add zero zero" test_add_zero_zero,
            TestLabel "test add zero one" test_add_zero_one,
            TestLabel "test add three zero" test_add_three_zero,
            TestLabel "test add 77 213" test_add_77_213,

            TestLabel "test sub zero zero" test_sub_zero_zero,
            TestLabel "test sub zero one" test_sub_zero_one,
            TestLabel "test sub three zero" test_sub_three_zero,
            TestLabel "test sub 213 77" test_sub_213_77,

            TestLabel "test mult zero zero" test_mult_zero_zero,
            TestLabel "test mult three zero" test_mult_three_zero,
            TestLabel "test mult 13 99" test_mult_13_99,
            TestLabel "test mult 1 121" test_mult_1_121,

            TestLabel "test div zero zero" test_div_8_two,
            TestLabel "test div zero three" test_div_zero_three,
            TestLabel "test div 99 4 (floor)" test_div_99_4,
            TestLabel "test div 1275 25" test_div_1275_25,

            TestLabel "test exp zero zero" test_exp_zero_zero,
            TestLabel "test exp zero 3" test_exp_zero_3,
            TestLabel "test exp 2 8" test_exp_2_8,
            TestLabel "test exp 133 1" test_exp_133_1,

            TestLabel "test fac zero" test_fac_zero,
            TestLabel "test fac one" test_fac_one,
            TestLabel "test fac three" test_fac_three,
            TestLabel "test fac 5" test_fac_5,
            TestLabel "test fac 7" test_fac_7] 



{- Test Comparison Operators -
 - - - - - - - - - - - - - - -}

test_if_zero = TestCase $ assertBool "is_zero zero == true"
  (unchurch_bool $ is_zero zero)
test_if_not_zero = TestCase $ assertEqual "is_zero one == false"
  False (unchurch_bool $ is_zero one)

test_lt_0_0 = TestCase $ assertEqual "lt 0 0 == false"
  False (unchurch_bool $ lt (num 0) (num 0)) 
test_lt_7_7 = TestCase $ assertEqual "lt 7 7 == false"
  False (unchurch_bool $ lt (num 7) (num 7)) 
test_lt_14_1 = TestCase $ assertEqual "lt 14 1 == false"
  False (unchurch_bool $ lt (num 14) (num 1)) 
test_lt_1_2 = TestCase $ assertBool "lt 1 2 == true"
  (unchurch_bool $ lt (num 1) (num 2)) 

test_leq_0_0 = TestCase $ assertBool "leq 0 0 == true"
  (unchurch_bool $ leq (num 0) (num 0)) 
test_leq_7_7 = TestCase $ assertBool "leq 7 7 == true"
  (unchurch_bool $ leq (num 7) (num 7)) 
test_leq_14_1 = TestCase $ assertEqual "leq 14 1 == false"
  False (unchurch_bool $ leq (num 14) (num 1)) 
test_leq_1_2 = TestCase $ assertBool "leq 1 2 == true"
  (unchurch_bool $ leq (num 1) (num 2)) 

test_eq_0_0 = TestCase $ assertBool "eq 0 0 == true"
  (unchurch_bool $ eq (num 0) (num 0)) 
test_eq_7_7 = TestCase $ assertBool "eq 7 7 == true"
  (unchurch_bool $ eq (num 7) (num 7)) 
test_eq_14_1 = TestCase $ assertEqual "eq 14 1 == false"
  False (unchurch_bool $ eq (num 14) (num 1)) 
test_eq_1_2 = TestCase $ assertEqual "eq 1 2 == false"
  False (unchurch_bool $ eq (num 1) (num 2)) 

test_geq_0_0 = TestCase $ assertBool "geq 0 0 == true"
  (unchurch_bool $ geq (num 0) (num 0)) 
test_geq_7_7 = TestCase $ assertBool "geq 7 7 == true"
  (unchurch_bool $ geq (num 7) (num 7)) 
test_geq_14_1 = TestCase $ assertBool "geq 14 1 == true"
  (unchurch_bool $ geq (num 14) (num 1)) 
test_geq_1_2 = TestCase $ assertEqual "geq 1 2 == false"
  False (unchurch_bool $ geq (num 1) (num 2)) 

test_gt_0_0 = TestCase $ assertEqual "gt 0 0 == false"
  False (unchurch_bool $ gt (num 0) (num 0)) 
test_gt_7_7 = TestCase $ assertEqual "gt 7 7 == false"
  False (unchurch_bool $ gt (num 7) (num 7)) 
test_gt_14_1 = TestCase $ assertBool "gt 14 1 == true"
  (unchurch_bool $ gt (num 14) (num 1)) 
test_gt_1_2 = TestCase $ assertEqual "gt 1 2 == false"
  False (unchurch_bool $ gt (num 1) (num 2)) 


tests_comparison_operators =
  TestList [TestLabel "test if zero" test_if_zero,
            TestLabel "test if not zero" test_if_not_zero,

            TestLabel "test lt 0 0" test_lt_0_0,
            TestLabel "test lt 7 7" test_lt_7_7,
            TestLabel "test lt 14 1" test_lt_14_1,
            TestLabel "test lt 1 2" test_lt_1_2,

            TestLabel "test leq 0 0" test_leq_0_0,
            TestLabel "test leq 7 7" test_leq_7_7,
            TestLabel "test leq 14 1" test_leq_14_1,
            TestLabel "test leq 1 2" test_leq_1_2,

            TestLabel "test eq 0 0" test_eq_0_0,
            TestLabel "test eq 7 7" test_eq_7_7,
            TestLabel "test eq 14 1" test_eq_14_1,
            TestLabel "test eq 1 2" test_eq_1_2,

            TestLabel "test geq 0 0" test_geq_0_0,
            TestLabel "test geq 7 7" test_geq_7_7,
            TestLabel "test geq 14 1" test_geq_14_1,
            TestLabel "test geq 1 2" test_geq_1_2,

            TestLabel "test gt 0 0" test_gt_0_0,
            TestLabel "test gt 7 7" test_gt_7_7,
            TestLabel "test gt 14 1" test_gt_14_1,
            TestLabel "test gt 1 2" test_gt_1_2]



{- Test Lists  -
 - - - - - - - -}

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

-- List construction is done using cons
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



{- Test Tuples -
 - - - - - - - -}

test_tuple2_zero_zero_first = TestCase $ assertEqual "first (tuple2 zero zero) == 0"
  0 (unchurch_num_elem $ tuple2_first $ tuple2 (ChurchNumber zero) (ChurchNumber zero))
test_tuple2_zero_zero_second = TestCase $ assertEqual "second (tuple2 zero zero) == 0"
  0 (unchurch_num_elem $ tuple2_second $ tuple2 (ChurchNumber zero) (ChurchNumber zero))

test_tuple2_5_12_first = TestCase $ assertEqual "first (tuple2 5 12) == 5"
  5 (unchurch_num_elem $ tuple2_first $ tuple2 (ChurchNumber $ num 5) (ChurchNumber $ num 12))
test_tuple2_5_12_second = TestCase $ assertEqual "second (tuple2 5 12) == 12"
  12 (unchurch_num_elem $ tuple2_second $ tuple2 (ChurchNumber $ num 5) (ChurchNumber $ num 12))

test_tuple2_true_false_first = TestCase $ assertBool "first (tuple2 true false) == true"
  (unchurch_bool_elem $ tuple2_first $ tuple2 (ChurchBoolean true) (ChurchBoolean false))
test_tuple2_true_false_second = TestCase $ assertEqual "second (tuple2 true false) == false"
  False (unchurch_bool_elem $ tuple2_second $ tuple2 (ChurchBoolean true) (ChurchBoolean false))

test_tuple2_false_true_first = TestCase $ assertEqual "first (tuple2 false true) == false"
  False (unchurch_bool_elem $ tuple2_first $ tuple2 (ChurchBoolean false) (ChurchBoolean true))
test_tuple2_false_true_second = TestCase $ assertBool "second (tuple2 false true) == true"
  (unchurch_bool_elem $ tuple2_second $ tuple2 (ChurchBoolean false) (ChurchBoolean true))

test_tuple2_true_true_first = TestCase $ assertBool "first (tuple2 true true) == true"
  (unchurch_bool_elem $ tuple2_first $ tuple2 (ChurchBoolean true) (ChurchBoolean true))
test_tuple2_true_true_second = TestCase $ assertBool "second (tuple2 true true) == true"
  (unchurch_bool_elem $ tuple2_second $ tuple2 (ChurchBoolean true) (ChurchBoolean true))


tests_tuples = 
  TestList [TestLabel "first (tuple2 zero zero)" test_tuple2_zero_zero_first,
            TestLabel "second (tuple2 zero zero)" test_tuple2_zero_zero_second,

            TestLabel "first (tuple2 5 12)" test_tuple2_5_12_first,
            TestLabel "second (tuple2 5 12)" test_tuple2_5_12_second,

            TestLabel "first (tuple2 true false)" test_tuple2_true_false_first,
            TestLabel "second (tuple2 true false)" test_tuple2_true_false_second,

            TestLabel "first (tuple2 false true)" test_tuple2_false_true_first,
            TestLabel "second (tuple2 false true)" test_tuple2_false_true_second,

            TestLabel "first (tuple2 true true)" test_tuple2_true_true_first,
            TestLabel "second (tuple2 true true)" test_tuple2_true_true_second]



{- Test Integers (n ∈ ℤ) -
 - - - - - - - - - - - - -}

test_convertNZ_zero = TestCase $ assertEqual "convertNZ zero == zero" 
  0 (unchurch_int $ convertNZ zero)
test_convertNZ_three = TestCase $ assertEqual "convertNZ three == three"
  3 (unchurch_int $ convertNZ three)
test_convertNZ_45 = TestCase $ assertEqual "convertNZ 45 == 45"
  45 (unchurch_int $ convertNZ $ num 45)
test_neg_zero = TestCase $ assertEqual "neg 0 == 0"
  0 (unchurch_int $ neg $ convertNZ $ zero)
test_neg_27 = TestCase $ assertEqual "neg 27 == -27"
  (-27) (unchurch_int $ neg $ convertNZ $ num 27)


tests_integers = 
  TestList [TestLabel "convertNZ zero" test_convertNZ_zero,
            TestLabel "convertNZ three" test_convertNZ_three,
            TestLabel "convertNZ 45" test_convertNZ_45,
            TestLabel "neg 0" test_neg_zero,
            TestLabel "neg 27" test_neg_27]



{- Test Arithmetic Operators for Integers (n ∈ ℤ) -
 - - - - - - - - - - - - - - -- - - - - - - - - - -}

test_addZ_zero_zero = TestCase $ assertEqual "addZ zero zero == 0"
  0 (unchurch_int $ addZ (convertNZ zero) (convertNZ zero))
test_addZ_zero_one = TestCase $ assertEqual "addZ zero one == 1"
  1 (unchurch_int $ addZ (convertNZ zero) (convertNZ one))
test_addZ_three_zero = TestCase $ assertEqual "addZ three zero == 3"
  3 (unchurch_int $ addZ (convertNZ three) (convertNZ zero))
test_addZ_77_n213 = TestCase $ assertEqual "addZ 77 -213 == -136"
  (-136) (unchurch_int $ addZ (convertNZ $ num 77) (neg $ convertNZ $ num 213))

test_subZ_zero_zero = TestCase $ assertEqual "subZ zero zero == 0"
  0 (unchurch_int $ subZ (convertNZ zero) (convertNZ zero))
test_subZ_zero_one = TestCase $ assertEqual "subZ zero one == -1"
  (-1) (unchurch_int $ subZ (convertNZ zero) (convertNZ one))
test_subZ_n14_99 = TestCase $ assertEqual "subZ -14 99 == -113"
  (-113) (unchurch_int $ subZ (neg $ convertNZ $ num 14) (convertNZ $ num 99))
test_subZ_213_77 = TestCase $ assertEqual "subZ 213 77 == 136"
  136 (unchurch_int $ subZ (convertNZ $ num 213) (convertNZ $ num 77))
test_subZ_n3_n99 = TestCase $ assertEqual "subZ -3 -99 == 96"
  96 (unchurch_int $ subZ (neg $ convertNZ $ num 3) (neg $ convertNZ $ num 99))

test_multZ_zero_zero = TestCase $ assertEqual "multZ zero zero == 0"
  0 (unchurch_int $ multZ (convertNZ zero) (convertNZ zero))
test_multZ_three_zero = TestCase $ assertEqual "multZ three zero == 0"
  0 (unchurch_int $ multZ (convertNZ three) (convertNZ zero))
test_multZ_n3_99 = TestCase $ assertEqual "multZ -3 99 == -297"
  (-297) (unchurch_int $ multZ (neg $ convertNZ $ num 3) (convertNZ $ num 99))
test_multZ_n1_n121 = TestCase $ assertEqual "mult -1 -121 == 121"
  121 (unchurch_int $ multZ (neg $ convertNZ $ num 1) (neg $ convertNZ $ num 121))

test_divZ_8_two = TestCase $ assertEqual "divZ 8 two == 4"
  4 (unchurch_int $ divZ (convertNZ $ num 8) (convertNZ two))
test_divZ_zero_three = TestCase $ assertEqual "divZ zero three == 0"
  0 (unchurch_int $ divZ (convertNZ zero) (convertNZ three))
test_divZ_n99_4 = TestCase $ assertEqual "divZ -99 4 == 24"
  (-24) (unchurch_int $ divZ (neg $ convertNZ $ num 99) (convertNZ $ num 4))
test_divZ_n1275_n25 = TestCase $ assertEqual "divZ -1275 -25 == 51"
  51 (unchurch_int $ divZ (neg $ convertNZ $ num 1275) (neg $ convertNZ $ num 25))


tests_arithmetic_operators_Z =
  TestList [TestLabel "addZ zero zero" test_addZ_zero_zero,
            TestLabel "addZ zero one" test_addZ_zero_one,
            TestLabel "addZ three zero" test_addZ_three_zero,
            TestLabel "addZ 77 -213" test_addZ_77_n213,

            TestLabel "subZ zero zero" test_subZ_zero_zero,
            TestLabel "subZ zero one" test_subZ_zero_one,
            TestLabel "subZ -14 99" test_subZ_n14_99,
            TestLabel "subZ 213 77" test_subZ_213_77,
            TestLabel "subZ -3 -99" test_subZ_n3_n99,

            TestLabel "multZ zero zero" test_multZ_zero_zero,
            TestLabel "multZ three zero" test_multZ_three_zero,
            TestLabel "multZ -3 99" test_multZ_n3_99,
            TestLabel "multZ -1 -121" test_multZ_n1_n121,

            TestLabel "divZ 8 two" test_divZ_8_two,
            TestLabel "divZ zero three" test_divZ_zero_three,
            TestLabel "divZ -99 4" test_divZ_n99_4,
            TestLabel "divZ -1275 -25" test_divZ_n1275_n25]



main = do
  runTestTT tests_logical_operators
  runTestTT tests_naturals
  runTestTT tests_conditionals
  runTestTT tests_arithmetic_operators_N
  runTestTT tests_comparison_operators
  runTestTT tests_lists
  runTestTT tests_tuples
  runTestTT tests_integers
  runTestTT tests_arithmetic_operators_Z

open OUnit2
open Basics

let test_factorial _ =
  assert_equal 1 (factorial 1) ~msg:"factorial (1)";
  assert_equal 2 (factorial 2) ~msg:"factorial (2)";
  assert_equal 6 (factorial 3) ~msg:"factorial (3)";
  assert_equal 120 (factorial 5) ~msg:"factorial (4)"

let test_pow _ =
  assert_equal 2 (pow 2 1) ~msg:"pow (1)";
  assert_equal 4 (pow 2 2) ~msg:"pow (2)";
  assert_equal 3 (pow 3 1) ~msg:"pow (3)";
  assert_equal 27 (pow 3 3) ~msg:"pow (4)";
  assert_equal 625 (pow 5 4) ~msg:"pow (5)";
  assert_equal (-27) (pow (-3) 3) ~msg:"pow (6)"

let test_log _ =
  assert_equal 1 (log 4 4) ~msg:"log (1)";
  assert_equal 2 (log 4 16) ~msg:"log (2)";
  assert_equal 1 (log 4 15) ~msg:"log (3)";
  assert_equal 3 (log 4 64) ~msg:"log (4)"

let test_is_prime _ =
  assert_equal false (is_prime 1) ~msg:"is_prime (1)";
  assert_equal true (is_prime 2) ~msg:"is_prime (2)";
  assert_equal true (is_prime 3) ~msg:"is_prime (3)";
  assert_equal false (is_prime 4) ~msg:"is_prime (4)";
  assert_equal true (is_prime 5) ~msg:"is_prime (5)";
  assert_equal false (is_prime 60) ~msg:"is_prime (6)";
  assert_equal true (is_prime 61) ~msg:"is_prime (7)"

let test_next_prime _ =
  assert_equal 2 (next_prime 1) ~msg:"next_prime (1)";
  assert_equal 2 (next_prime 2) ~msg:"next_prime (2)";
  assert_equal 3 (next_prime 3) ~msg:"next_prime (3)";
  assert_equal 5 (next_prime 4) ~msg:"next_prime (4)";
  assert_equal 61 (next_prime 60) ~msg:"next_prime (5)"

let suite =
  "student" >::: [
    "factorial" >:: test_factorial;
    "pow" >:: test_pow;
    "log" >:: test_log;
    "is_prime" >:: test_is_prime;
    "next_prime" >:: test_next_prime;
  ]

let _ = run_test_tt_main suite

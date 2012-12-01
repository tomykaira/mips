open Reg
open Id
open OUnit

let test_rest3 =
  TestCase (fun ()-> assert_equal (`I 4)
    (List.hd (Reg.rest [`I 3])))

let test_rest3567 =
  TestCase (fun ()-> assert_equal (`I 4)
    (List.hd (Reg.rest [`I 3; `I 5; `I 6; `I 7])))

let tests =
  TestList [ test_rest3; test_rest3567 ]

let _ =
  run_test_tt_main tests

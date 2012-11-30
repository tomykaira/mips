open RegisterAllocation
open Reg
open Id
open OUnit

let live = (S.of_list [V "argc.1"; V "c.4"])
let usage = [(`I 3, V "argc.1")]

let test_using_registers =
  TestCase (fun ()-> assert_equal [`I 3]
    (using_registers live usage))

let test_allocate =
  TestCase (fun ()-> assert_equal [(`I 4, V "c.4")]
    (fst (allocate live usage [V "c.4"])))

let tests =
  TestList [ test_allocate; test_using_registers ]

let _ =
  run_test_tt_main tests

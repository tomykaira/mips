open Id
open OUnit
open Definition
open Syntax

module M = Alpha.M

let test_rename_parameter =
  TestCase (fun ()->
    Id.counter := 0;
    assert_equal (Parameter(Int, V "argc.1"))
      (snd (Alpha.rename_parameter M.empty (Parameter(Int, "argc")))))

let tests =
  TestList [ test_rename_parameter ]

let _ =
  run_test_tt_main tests

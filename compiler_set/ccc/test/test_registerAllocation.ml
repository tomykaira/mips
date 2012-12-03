open OUnit
open RegisterAllocation

let test_adjacent_nodes =
  TestCase (fun ()->
    add_edge "c.2" "c.3";
    assert_equal ["c.3"] (S.elements (adjacent_nodes "c.2")))

let test_is_adjacent =
  TestCase (fun ()->
    add_edge "c.2" "c.3";
    assert_equal true (is_adjacent ("c.2", "c.3"));
    assert_equal true (is_adjacent ("c.3", "c.2")))

let tests =
  TestList [ test_adjacent_nodes ]

let _ =
  run_test_tt_main tests

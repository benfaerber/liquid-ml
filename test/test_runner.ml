open Alcotest

(* Run all test suites *)
let () =
  run "Liquid ML Tests" [
    Test_rendering.suite;
    Test_filters.suite;
    Test_string_filters.suite;
    Test_number_filters.suite;
    Test_list_filters.suite;
    Test_for_loops.suite;
    Test_variables.suite;
  ]

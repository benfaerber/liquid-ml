open Alcotest

(* Run all test suites *)
let () =
  run "Liquid ML Tests" [
    Test_rendering.suite;
    Test_filters.suite;
    Test_variables.suite;
  ]

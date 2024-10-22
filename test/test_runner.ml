(* open Cube_problem *)

(**
     Entrypoint for the test runner.
     This aggregates all the tests and call Alcotest to run them. When
     creating a new test suite, don't forget to add it here!
   *)

let () =
  let open Alcotest in
  run "Starter_lib"
    [
      ( "find_free_place_correctly",
        Lib_test.Cube_problem_test.find_free_place_correctly );
      ( "get_append_data_correctly",
        Lib_test.Cube_problem_test.get_append_data_correctly );
    ]

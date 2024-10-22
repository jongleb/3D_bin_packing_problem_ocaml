open Cube_problem

module Cube_problem_cell_testable = struct
  include Cell

  let pp =
    Fmt.of_to_string (fun i ->
        i |> sexp_of_t |> Sexplib0.Sexp.to_string_hum ~indent:2)
end

let () =
  let _ = Cube_problem.init ~length:5 ~width:5 ~height:5 [] in
  ()

let can_find_free_place_correctly () =
  let layer = Base.Map.empty (module Cell) in
  let cube = 4 in
  let expected = Some Cell.{ x = 0; y = 0 } in
  let result = find_free_place layer cube in
  Alcotest.(check @@ Alcotest.option (module Cube_problem_cell_testable))
    "same cell" expected result

let can_find_free_place_correctly_2 () =
  let layer = Base.Map.empty (module Cell) in
  let cube = 6 in
  let expected = None in
  let result = find_free_place layer cube in
  Alcotest.(check @@ Alcotest.option (module Cube_problem_cell_testable))
    "same cell" expected result

let can_find_free_place_correctly_3 () =
  let alist =
    [
      (Cell.{ x = 0; y = 0 }, true);
      (Cell.{ x = 1; y = 0 }, true);
      (Cell.{ x = 2; y = 0 }, true);
      (Cell.{ x = 3; y = 0 }, true);
      (Cell.{ x = 0; y = 1 }, true);
      (Cell.{ x = 1; y = 1 }, true);
      (Cell.{ x = 2; y = 1 }, true);
      (Cell.{ x = 3; y = 1 }, true);
      (Cell.{ x = 0; y = 2 }, true);
      (Cell.{ x = 1; y = 2 }, true);
      (Cell.{ x = 2; y = 2 }, true);
      (Cell.{ x = 3; y = 2 }, true);
      (Cell.{ x = 0; y = 3 }, true);
      (Cell.{ x = 1; y = 3 }, true);
      (Cell.{ x = 2; y = 3 }, true);
      (Cell.{ x = 3; y = 3 }, true);
    ]
  in
  let layer = Base.Map.of_alist_exn (module Cell) alist in
  let cube = 1 in
  let expected = Some Cell.{ x = 4; y = 0 } in
  let result = find_free_place layer cube in
  Alcotest.(check @@ Alcotest.option (module Cube_problem_cell_testable))
    "same cell" expected result

let can_find_free_place_correctly_4 () =
  (*x*)
  (* let width = 5 in
     (*y*)
     let length = 5 in *)
  let alist =
    [
      (Cell.{ x = 0; y = 0 }, true);
      (Cell.{ x = 1; y = 0 }, true);
      (Cell.{ x = 2; y = 0 }, true);
      (Cell.{ x = 3; y = 0 }, true);
      (Cell.{ x = 0; y = 1 }, true);
      (Cell.{ x = 1; y = 1 }, true);
      (Cell.{ x = 2; y = 1 }, true);
      (Cell.{ x = 3; y = 1 }, true);
      (Cell.{ x = 0; y = 2 }, true);
      (Cell.{ x = 1; y = 2 }, true);
      (Cell.{ x = 2; y = 2 }, true);
      (Cell.{ x = 3; y = 2 }, true);
      (Cell.{ x = 0; y = 3 }, true);
      (Cell.{ x = 1; y = 3 }, true);
      (Cell.{ x = 2; y = 3 }, true);
      (Cell.{ x = 3; y = 3 }, true);
      (Cell.{ x = 4; y = 0 }, true);
      (Cell.{ x = 4; y = 1 }, true);
      (Cell.{ x = 4; y = 2 }, true);
      (Cell.{ x = 4; y = 3 }, true);
    ]
  in
  let layer = Base.Map.of_alist_exn (module Cell) alist in
  let cube = 1 in
  let expected = Some Cell.{ x = 0; y = 4 } in
  let result = find_free_place layer cube in
  Alcotest.(check @@ Alcotest.option (module Cube_problem_cell_testable))
    "same cell" expected result

let can_get_append_data_correctly () =
  let cube = 2 in
  let cell = Cell.{ x = 0; y = 0 } in
  let expected =
    [
      Cell.{ x = 0; y = 0 };
      Cell.{ x = 1; y = 0 };
      Cell.{ x = 0; y = 1 };
      Cell.{ x = 1; y = 1 };
    ]
  in
  let result = gen_append_data ~cube cell in
  Alcotest.(check @@ Alcotest.list (module Cube_problem_cell_testable))
    "same cell list" expected result

let can_get_append_data_correctly_2 () =
  let cube = 4 in
  let cell = Cell.{ x = 1; y = 1 } in
  let expected =
    [
      Cell.{ x = 1; y = 1 };
      Cell.{ x = 2; y = 1 };
      Cell.{ x = 3; y = 1 };
      Cell.{ x = 4; y = 1 };
      Cell.{ x = 1; y = 2 };
      Cell.{ x = 2; y = 2 };
      Cell.{ x = 3; y = 2 };
      Cell.{ x = 4; y = 2 };
      Cell.{ x = 1; y = 3 };
      Cell.{ x = 2; y = 3 };
      Cell.{ x = 3; y = 3 };
      Cell.{ x = 4; y = 3 };
      Cell.{ x = 1; y = 4 };
      Cell.{ x = 2; y = 4 };
      Cell.{ x = 3; y = 4 };
      Cell.{ x = 4; y = 4 };
    ]
  in
  let result = gen_append_data ~cube cell in
  Alcotest.(check @@ Alcotest.list (module Cube_problem_cell_testable))
    "same cell list" expected result

let can_get_append_data_correctly_3 () =
  let cube = 2 in
  let cell = Cell.{ x = 3; y = 3 } in
  let expected =
    [
      Cell.{ x = 3; y = 3 };
      Cell.{ x = 4; y = 3 };
      Cell.{ x = 3; y = 4 };
      Cell.{ x = 4; y = 4 };
    ]
  in
  let result = gen_append_data ~cube cell in
  Alcotest.(check @@ Alcotest.list (module Cube_problem_cell_testable))
    "same cell list" expected result

let can_get_append_data_correctly_4 () =
  let cube = 1 in
  let cell = Cell.{ x = 3; y = 0 } in
  let expected = [ Cell.{ x = 3; y = 0 } ] in
  let result = gen_append_data ~cube cell in
  Alcotest.(check @@ Alcotest.list (module Cube_problem_cell_testable))
    "same cell list" expected result

let can_get_append_data_correctly_5 () =
  let cube = 1 in
  let cell = Cell.{ x = 2; y = 2 } in
  let expected = [ Cell.{ x = 2; y = 2 } ] in
  let result = gen_append_data ~cube cell in
  Alcotest.(check @@ Alcotest.list (module Cube_problem_cell_testable))
    "same cell list" expected result

let find_free_place_correctly =
  [
    ("cube 4, empty layer, box: 5 x 5", `Quick, can_find_free_place_correctly);
    ("cube 6, empty layer, box: 5 x 5", `Quick, can_find_free_place_correctly_2);
    ( "cube 1, non empty (4x4) in, box: 5 x 5",
      `Quick,
      can_find_free_place_correctly_3 );
    ("cube 1, should pick x: 0, y: 5", `Quick, can_find_free_place_correctly_4);
  ]

let get_append_data_correctly =
  [
    ("cube: 2, cell: {x=0;y=0}", `Quick, can_get_append_data_correctly);
    ("cube: 4, cell: {x=1;y=1}", `Quick, can_get_append_data_correctly_2);
    ("cube: 2, cell: {x=3;y=3}", `Quick, can_get_append_data_correctly_3);
    ("cube: 1, cell: {x=3;y=0}", `Quick, can_get_append_data_correctly_4);
    ("cube: 1, cell: {x=2;y=2}", `Quick, can_get_append_data_correctly_5);
  ]

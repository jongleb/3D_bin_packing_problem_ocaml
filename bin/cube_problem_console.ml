let () =
  let get_result ~length ~width ~height list =
    let open Cube_problem in
    let state = init ~length ~width ~height list in
    let result = loop state in
    match result with Fail -> -1 | Success i -> i
  in

  let get_result ~l ~w ~h tail =
    let open Base.Option in
    let tail =
      List.mapi
        (fun idx cube ->
          let idx = if idx = 0 then 1 else idx * 2 in
          List.init cube (Fun.const idx))
        tail
    in
    let tail = List.concat tail in
    get_result ~length:l ~width:w ~height:h tail
  in

  let all_ints list = list |> List.map int_of_string_opt |> Base.Option.all in

  let parse_and_eval line =
    let open Base.Option in
    let line_list = Base.String.split ~on:' ' line in
    let list = all_ints line_list in
    let result =
      list >>| function
      | l :: w :: h :: tail when List.length tail > 0 ->
          get_result ~l ~w ~h tail
      | _ -> -1
    in
    Option.value ~default:(-1) result
  in

  let rec read_ints () =
    try
      () |> read_line |> parse_and_eval |> string_of_int |> print_endline
      |> read_ints
    with End_of_file -> ()
  in

  read_ints ()

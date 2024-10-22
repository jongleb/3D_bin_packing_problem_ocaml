open Base

module Cell = struct
  module T = struct
    type t = { x : int; y : int } [@@deriving compare, sexp_of, equal]
  end

  include T
  include Comparator.Make (T)

  let make x y = { x; y }
end

type layer = (Cell.t, bool, Cell.comparator_witness) Map.t
type cube_stack = int Stack.t
type cube_list = int list
type layer_state = { busy_area : int; layer_data : layer }
type box = (int, layer_state, Int.comparator_witness) Map.t

type t = {
  box_data : box;
  current_layer : int;
  current_supplies : int Stack.t;
  discarded_supplies : int list;
  cubes_now : int;
}

let box_length = ref 0
let box_width = ref 0
let box_height = ref 0
let layer_square = ref 0

let find_free_place layer cube =
  let rec go x y =
    if y > !box_width then None
    else if x > !box_length then go 0 @@ (y + 1)
    else
      let cell = Cell.{ x; y } in
      let is_busy = cell |> Map.find layer |> Option.value ~default:false in
      if
        (not is_busy) && y - 1 + cube < !box_width && x - 1 + cube < !box_length
      then Some cell
      else go (x + 1) y
  in
  go 0 0

let gen_append_data ~cube cell =
  let init_line_for_cube () = List.init cube ~f:Fn.id in
  let cell_list_x = init_line_for_cube () in
  let cell_list_y = init_line_for_cube () in
  let append x =
    List.map
      ~f:(fun y -> Cell.make (cell.Cell.x + y) (cell.Cell.y + x))
      cell_list_y
  in
  List.concat_map cell_list_x ~f:append

let take_layer_field_place layer =
  List.fold ~init:layer ~f:(fun acc key ->
      let add = Map.add ~key ~data:true acc in
      match add with `Duplicate -> acc | `Ok m -> m)

let get_new_layer_state layer append_data cube =
  let layer_data = take_layer_field_place layer.layer_data append_data in
  let busy_area = layer.busy_area + (cube * cube) in
  { layer_data; busy_area }

let put_cube_opt cube layer =
  let open Option.Let_syntax in
  let%map cell = find_free_place layer.layer_data cube in
  gen_append_data ~cube cell

type level_status = Done | Supplies_is_over | Continue_filling

let get_current_status current_supplies busy_area =
  if !layer_square = busy_area then Done
  else if Stack.is_empty current_supplies then Supplies_is_over
  else Continue_filling

let get_current_layer s = Map.find s.box_data s.current_layer

type loop_result = Success of int | Fail

let discard_supplies discard state =
  let discarded_supplies = discard :: state.discarded_supplies in
  { state with discarded_supplies }

let rec get_next_cube state =
  let open Option.Let_syntax in
  let%bind cube = Stack.pop state.current_supplies in

  let exists = ( = ) cube in

  if Base.List.exists state.discarded_supplies ~f:exists then
    let discarded_supplies = cube :: state.discarded_supplies in
    let state = { state with discarded_supplies } in
    get_next_cube state
  else Some (state, cube)

let spread_over_the_layer ~current_layer ~append_data state cube =
  cube
  |> List.init ~f:(( + ) current_layer)
  |> List.fold ~init:state ~f:(fun state i ->
         let layer = Map.find_exn state.box_data i in
         let layer_state = get_new_layer_state layer append_data cube in
         let box_data = Map.set ~key:i ~data:layer_state state.box_data in
         { state with box_data })

let rec loop state =
  state |> get_current_layer
  |> Option.value_map
       ~f:(fun current_layer ->
         let current_status =
           get_current_status state.current_supplies current_layer.busy_area
         in
         match current_status with
         | Done ->
             let current_layer = state.current_layer + 1 in
             List.iter
               ~f:(Base.Stack.push state.current_supplies)
               state.discarded_supplies;
             let discarded_supplies = [] in
             loop @@ { state with current_layer; discarded_supplies }
         | Supplies_is_over -> Fail
         | Continue_filling -> next_cube state current_layer)
       ~default:(Success state.cubes_now)

and next_cube state current_layer =
  match get_next_cube state with
  | Some (state, cube) -> (
      if !box_height - state.current_layer < cube then loop state
      else
        let current_layer = put_cube_opt cube current_layer in
        match current_layer with
        | Some append_data ->
            let state =
              spread_over_the_layer ~current_layer:state.current_layer
                ~append_data state cube
            in
            let state = { state with cubes_now = state.cubes_now + 1 } in
            loop state
        | _ -> loop @@ discard_supplies cube state)
  | _ -> Fail

let init ~length ~width ~height list =
  let current_supplies = Base.Stack.create () in
  List.iter ~f:(Stack.push current_supplies) list;
  let layers = List.init height ~f:Fn.id in
  let box_data =
    List.fold_left
      ~f:(fun acc key ->
        let busy_area = 0 in
        let layer_data = Base.Map.empty (module Cell) in
        let data = { busy_area; layer_data } in
        Map.add_exn ~key ~data acc)
      ~init:(Map.empty (module Int))
      layers
  in
  let current_layer = 0 in
  let cubes_now = 0 in
  let discarded_supplies = [] in
  box_height := height;
  box_length := length;
  box_width := width;
  layer_square := length * width;
  { current_supplies; box_data; current_layer; cubes_now; discarded_supplies }

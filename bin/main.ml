(* Game of Life implementation in OCaml with graphical display *)

(* open Unix *)

let width = 600 (* Width of the window *)
let height = 600 (* Height of the window *)
let cell_size = 10 (* Size of each cell *)

let rows = height / cell_size (* Number of rows in the grid *)
let cols = width / cell_size (* Number of columns in the grid *)

(* Initialize the grid with random values *)
let grid = Array.make_matrix rows cols false

let init_grid () =
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      grid.(i).(j) <- Random.bool ()
    done
  done

(* Draw the grid on the screen *)
let draw_grid () =
  Graphics.clear_graph ();
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      if grid.(i).(j) then
        Graphics.fill_rect (j * cell_size) (i * cell_size) cell_size cell_size
    done
  done

(* Count the number of live neighbors of a cell *)
let count_neighbors i j =
  let count = ref 0 in
  for x = i - 1 to i + 1 do
    for y = j - 1 to j + 1 do
      if x >= 0 && x < rows && y >= 0 && y < cols && (x <> i || y <> j) then
        if grid.(x).(y) then count := !count + 1
    done
  done;
  !count

(* Update the grid for the next generation *)
let update_grid () =
  let new_grid = Array.make_matrix rows cols false in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let count = count_neighbors i j in
      if grid.(i).(j) && (count = 2 || count = 3) then
        new_grid.(i).(j) <- true
      else if not grid.(i).(j) && count = 3 then
        new_grid.(i).(j) <- true
    done
  done;
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      grid.(i).(j) <- new_grid.(i).(j)
    done
  done

(* Main loop *)
let rec main_loop () =
  draw_grid ();
  update_grid ();
  Unix.sleepf 0.1; (* Wait for 0.1 seconds *)
  main_loop ()

(* Entry point *)
let () =
  Random.self_init ();
  init_grid ();
  Graphics.open_graph (Printf.sprintf " %dx%d" width height);
  main_loop ()

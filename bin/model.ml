type col = Left | Middle | Right

type t = {
  holding : (int * col) option;
  moves : int;
  left : int list;
  middle : int list;
  right : int list;
  error : col option;
}

let init n =
  let rec aux acc = function 0 -> acc | n -> aux (n :: acc) (n - 1) in
  {
    holding = None;
    moves = 0;
    left = aux [] n;
    middle = [];
    right = [];
    error = None;
  }

let is_win = function
  | { left = []; middle = []; holding = None; _ } -> true
  | _ -> false

let droppable x = function [] -> true | y :: _ -> x < y

let move col state =
  let state = { state with error = None } in
  match (col, state) with
  (* Taking a disk *)
  | Left, { holding = None; left = x :: xs; _ } ->
      { state with holding = Some (x, Left); left = xs }
  | Middle, { holding = None; middle = x :: xs; _ } ->
      { state with holding = Some (x, Middle); middle = xs }
  | Right, { holding = None; right = x :: xs; _ } ->
      { state with holding = Some (x, Right); right = xs }
  (* Cancelling a move *)
  | Left, { holding = Some (n, Left); _ } ->
      { state with holding = None; left = n :: state.left }
  | Middle, { holding = Some (n, Middle); _ } ->
      { state with holding = None; middle = n :: state.middle }
  | Right, { holding = Some (n, Right); _ } ->
      { state with holding = None; right = n :: state.right }
  (* Normal moves *)
  (*   L -> M *)
  | Middle, { holding = Some (x, Left); middle; _ } when droppable x middle ->
      {
        state with
        holding = None;
        moves = state.moves + 1;
        middle = x :: state.middle;
      }
  (*   L -> R *)
  | Right, { holding = Some (x, Left); right; _ } when droppable x right ->
      { state with holding = None; moves = state.moves + 1; right = x :: right }
  (*   M -> L *)
  | Left, { holding = Some (x, Middle); left; _ } when droppable x left ->
      { state with holding = None; moves = state.moves + 1; left = x :: left }
  (*   M -> R *)
  | Right, { holding = Some (x, Middle); right; _ } when droppable x right ->
      { state with holding = None; moves = state.moves + 1; right = x :: right }
  (*   R -> L *)
  | Left, { holding = Some (x, Right); left; _ } when droppable x left ->
      { state with holding = None; moves = state.moves + 1; left = x :: left }
  (*   R -> M *)
  | Middle, { holding = Some (x, Right); middle; _ } when droppable x middle ->
      {
        state with
        holding = None;
        moves = state.moves + 1;
        middle = x :: middle;
      }
  (* Impossible moves *)
  (*   Taking from an empty column *)
  | Left, { holding = None; left = []; _ }
  | Middle, { holding = None; middle = []; _ }
  | Right, { holding = None; right = []; _ } ->
      state
  (*  Moving a disk on a lower disk. Forbidden! *)
  | col, { holding = Some _; _ } -> { state with error = Some col }

let col_of_arrow = function
  | `Left -> Left
  | `Right -> Right
  | `Up | `Down -> Middle

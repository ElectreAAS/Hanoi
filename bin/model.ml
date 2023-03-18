type col = Left | Middle | Right

type t = {
  holding : (int * col * col option) option;
  moves : int;
  left : int list;
  middle : int list;
  right : int list;
}

let init n =
  let rec aux acc = function 0 -> acc | n -> aux (n :: acc) (n - 1) in
  { holding = None; moves = 0; left = aux [] n; middle = []; right = [] }

let is_win = function
  | { left = []; middle = []; holding = None; _ } -> true
  | _ -> false

let droppable x = function [] -> true | y :: _ -> x < y

let move col state =
  match (col, state) with
  (* Taking a disk *)
  | Left, { holding = None; left = x :: xs; _ } ->
      { state with holding = Some (x, Left, None); left = xs }
  | Middle, { holding = None; middle = x :: xs; _ } ->
      { state with holding = Some (x, Middle, None); middle = xs }
  | Right, { holding = None; right = x :: xs; _ } ->
      { state with holding = Some (x, Right, None); right = xs }
  (* Cancelling a move *)
  | Left, { holding = Some (n, Left, _); _ } ->
      { state with holding = None; left = n :: state.left }
  | Middle, { holding = Some (n, Middle, _); _ } ->
      { state with holding = None; middle = n :: state.middle }
  | Right, { holding = Some (n, Right, _); _ } ->
      { state with holding = None; right = n :: state.right }
  (* Normal moves *)
  (*   -> L *)
  | Left, { holding = Some (x, _, _); left; _ } when droppable x left ->
      { state with holding = None; moves = state.moves + 1; left = x :: left }
  (*   -> M *)
  | Middle, { holding = Some (x, _, _); middle; _ } when droppable x middle ->
      {
        state with
        holding = None;
        moves = state.moves + 1;
        middle = x :: state.middle;
      }
  (*   -> R *)
  | Right, { holding = Some (x, _, _); right; _ } when droppable x right ->
      { state with holding = None; moves = state.moves + 1; right = x :: right }
  (* Impossible moves *)
  (*   Taking from an empty column *)
  | Left, { holding = None; left = []; _ }
  | Middle, { holding = None; middle = []; _ }
  | Right, { holding = None; right = []; _ } ->
      state
  (*  Moving a disk on a lower disk. Forbidden! *)
  | c2, { holding = Some (n, c1, _); _ } ->
      { state with holding = Some (n, c1, Some c2) }

let col_of_arrow = function
  | `Left -> Left
  | `Right -> Right
  | `Up | `Down -> Middle

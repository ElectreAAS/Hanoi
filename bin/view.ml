open Notty

let vmul i n =
  let rec aux acc n = if n < 2 then acc else aux I.(i <-> acc) (n - 1) in
  aux i n

let hmul i n =
  let rec aux acc n = if n < 2 then acc else aux I.(i <|> acc) (n - 1) in
  aux i n

let pipe = I.string A.(fg black) "┃"

let disk_of_n ?(error = false) n =
  let attr = if error then A.fg A.red else A.empty in
  let c = I.string attr "\u{2588}" in
  I.(
    string attr "\u{1FB6E}" <|> hmul c ((2 * n) + 1) <|> string attr "\u{1FB6C}")

let img_of_col ?(error = false) (w, h) l =
  let rec aux acc first = function
    | [] -> acc
    | n :: l ->
        aux
          I.(acc <-> hsnap (w / 3) (disk_of_n ~error:(first && error) n))
          false l
  in
  let disks = aux I.empty true l in
  let pole = I.hsnap (w / 3) (vmul pipe (h - I.height disks - 1)) in
  I.(pole <-> disks)

let img_of_state ((w, _) as dims)
    ({ holding; moves; left; middle; right } : Model.t) =
  let move_str = I.strf "Moves: %d" moves in
  let top_line =
    I.(
      move_str
      </> hsnap w
            (match holding with
            | None -> I.empty
            | Some (n, _, None) ->
                I.(string A.empty "Holding: " <|> disk_of_n n)
            | Some (n, _, Some _) ->
                I.(string A.empty "Holding: " <|> disk_of_n ~error:true n)))
  in
  let l, m, r =
    (img_of_col dims left, img_of_col dims middle, img_of_col dims right)
  in
  let l, m, r =
    match holding with
    | Some (_, _, Some Left) -> (img_of_col ~error:true dims left, m, r)
    | Some (_, _, Some Middle) -> (l, img_of_col ~error:true dims middle, r)
    | Some (_, _, Some Right) -> (l, m, img_of_col ~error:true dims right)
    | None | Some (_, _, None) -> (l, m, r)
  in
  let cols = I.(l <|> m <|> r) in
  I.(top_line <-> cols)

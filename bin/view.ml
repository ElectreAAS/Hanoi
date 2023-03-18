open Notty

let vmul i n =
  let rec aux acc n = if n < 2 then acc else aux I.(i <-> acc) (n - 1) in
  aux i n

let hmul i n =
  let rec aux acc n = if n < 2 then acc else aux I.(i <|> acc) (n - 1) in
  aux i n

let pipe = I.string A.empty "┃"

let disk_of_n ?(error = false) ?(holding = false) n =
  let attr = if error then A.fg A.red else A.empty in
  let char =
    if n < 10 then I.strf ~attr "%d" n
    else I.char attr (Char.chr @@ (Char.code 'A' + n - 10)) 1 1
  in
  let disk = hmul char ((2 * n) + 1) in
  I.(hsnap (width disk) (if holding then empty else pipe) </> disk)

let img_of_col (w, h) l =
  let rec aux acc = function
    | [] -> acc
    | n :: l -> aux I.(acc <-> hsnap (w / 3) (disk_of_n n)) l
  in
  let disks = aux I.empty l in
  let pole = I.hsnap (w / 3) (vmul pipe (h - I.height disks - 1)) in
  I.(pole <-> disks)

let img_of_state (w, h)
    ({ holding; moves; left; middle; right; error } : Model.t) =
  let cols =
    I.(
      img_of_col (w, h) left
      <|> img_of_col (w, h) middle
      <|> img_of_col (w, h) right)
  in
  match (holding, error) with
  | None, None -> I.(strf "Moves: %d" moves <-> cols)
  | Some (n, _), None ->
      I.(
        strf "Moves: %d" moves
        </> hsnap w (string A.empty "Holding: " <|> disk_of_n ~holding:true n)
        <-> cols)
  | Some (n, _), Some _ ->
      I.(
        strf "Moves: %d" moves
        </> hsnap w
              (string A.empty "Holding: "
              <|> disk_of_n ~error:true ~holding:true n)
        <-> cols)
  | None, Some _ -> failwith "Impossible case!"

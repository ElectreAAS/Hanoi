(* open Notty *)
open Notty_unix
module M = Model
module V = View

let () =
  let rec update t state =
    Term.image t (V.img_of_state (Term.size t) state);
    loop t state
  and loop t state =
    if M.is_win state then true
    else
      match Term.event t with
      | `Key (`Arrow a, _) -> update t (M.move (M.col_of_arrow a) state)
      | `Key (`ASCII 'q', _) -> false
      | `Resize _ -> update t state
      | _ -> loop t state
  in
  let t = Term.create () in
  let result = update t (M.init @@ int_of_string Sys.argv.(1)) in
  Term.release t;
  if result then print_endline "Bravo !"

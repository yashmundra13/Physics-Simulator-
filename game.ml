open ANSITerminal
open Simulator
open Input

(* [boost] is the change in velocity in m/s caused by hitting the thrust key. *)
let boost = 30000.0

(** [welcome ()] displays the load screen and a prompt for the user to enter a
    save game. *)
let welcome () =
  resize 100 50;
  erase Screen;
  set_cursor 1 1;
  print_string [] "

                       :
                       :
                       :
        .              :
         '.            :           .'
           '.          :         .'
             '.    ........    .'                                  .'':
               '.'          '.'                                ..... '          .---.
                :            :                _    _        .'     .' '.    ...'     '...
        .........            .........    o  (_)  (_)  ()   :    .'    :   '..:.......:..'
                :            :                              :  .'      :       '.....'
                 :          :                             .'.'.      .'
               .' '........' '.                           ''   ``````
             .'        :       '.         \\        /  ___   |   ___    ___    _  _    ___
           .'          :         '.        \\  /\\  /  |___|  |  |      |   |  | \\/ |  |___|
         .'            :           '.       \\/  \\/   |___   |  |___   |___|  |    |  |___
                       :
                       :                        enter the level you would like to play
                       :

>>> "

(** [key] is the type of keypress registered on the keyboard. *)
type key = CW | ACW | T

(** [ship] is the list of icons that represent a ship in each orientation. *)
let ship = [">"; "v"; "<"; "^"]

(** [next_icon i] is the icon of a ship with icon [i] after rotating one quarter
    turn clockwise. *)
let next_icon i = match i with
  | ">" -> "v"
  | "v" -> "<"
  | "<" -> "^"
  | "^" -> ">"
  | s -> raise (Invalid_argument s)

(** [prev_icon i] is the icon of a ship with icon [i] after rotating one quarter
    turn anticlockwise. *)
let prev_icon i = match i with
  | ">" -> "^"
  | "v" -> ">"
  | "<" -> "v"
  | "^" -> "<"
  | s -> raise (Invalid_argument s)

(** [ship_exists i u] is "there is a body in [u] with a name in list [i]." *)
let rec ship_exists i = function
  | [] -> false
  | h::t -> if List.mem h.name i then true else ship_exists i t

(** [winning_colisions] is the list of bodies that could be created by a
    collision between the ship and the target. *)
let winning_colisions = ["^Target";"Target^";"<Target";"Target<"
                        ;">Target";"Target>";"vTarget";"Targetv"]

(** [win u] is [0] if the game represented by [u] is still in progress,
    [1] if the ship has crashed into the target, and 2 if the ship has crashed
    into another body or if the target has crashed into another body. *)
let win u =
  if ship_exists ship u then 0
  else if ship_exists winning_colisions u then 1
  else 2

(** [thrust' u key] is the universe [u] with the action represented by keys
    applied to all ships in the universe. A key of [CW] rotates the ships
    clockwise, [ACW] rotates them anticlockwise, and [T] applies a thrust of
    magnitude [boost] in the direction they are pointing. *)
let thrust' u key =
  List.map (fun b ->
    if List.mem b.name ship then
      match key with
      | CW -> {b with name = next_icon b.name}
      | ACW -> {b with name = prev_icon b.name}
      | T -> let (boostx, boosty) = begin match b.name with
        | "^" -> (0., boost)
        | ">" -> (boost,0.)
        | "v" -> (0.,-1.*.boost)
        | "<" -> (-1.*.boost, 0.)
        | _   -> failwith "bad icon"
        end in
       {b with velocity = (match b.velocity with
            | x::y::t -> [x+.boostx; y+.boosty]
            | _ -> failwith "bad icon")
        }
    else b) u

(** [valid_save u] is "universe [u] contains a ship and a Target." *)
let valid_save u =
  List.fold_left (fun acc b -> acc || b.name = "Target") false u &&
  List.fold_left (fun acc b -> acc || List.mem b.name ship) false u

(** [thrust u] waits for user input for 0.1 seconds, then returns the universe
    resulting from that input, if any. *)
let rec thrust u =
  (* Keyboard capture based on https://stackoverflow.com/questions/4130048/recognizing-arrow-keys-with-stdin. *)
  let terminfo = Unix.tcgetattr Unix.stdin in
  let newterminfo = {terminfo with c_icanon = false; c_vmin = 0;
                                   c_vtime = 1; c_echo=false} in
  Unix.tcsetattr Unix.stdin TCSAFLUSH newterminfo; (* Capture keyboard *)
  let s =
    try
      input_line stdin
    with
    | End_of_file -> ""
  in
  Unix.tcsetattr Unix.stdin TCSAFLUSH terminfo; (* Reset stdin *)
  if Str.string_match (Str.regexp "[a+A]") s 0 then
    thrust' u ACW
  else if Str.string_match (Str.regexp "[d+D]") s 0 then
    thrust' u CW
  else if Str.string_match (Str.regexp "[w+W]") s 0 then
    thrust' u T
  else if Str.string_match (Str.regexp "q") s 0 then
    main ()
  else
    u

(** [main_run u] is the main loop that repeatedly calls the display and thrust
    functions on the universe [u]. *)
and main_run u =
  display ?play:(Some true) u;
  let x = run (thrust u) 1.0e5 1.0e4  in
  if win x = 1 then
    (resize 100 50; erase Screen; set_cursor 46 25;
     print_string [] "You win!"; set_cursor 1 1;
     Unix.sleepf 2.0;
     main ())
  else if win x = 2 then
    (resize 100 50; erase Screen; set_cursor 44 25;
     print_string [] "You crashed!"; set_cursor 1 1;
     Unix.sleepf 2.0;
     main ())
  else  x |> main_run

(** [main ()] displays the welcome page, takes a user save file, and initiates
    the [main_run] loop on that save file. *)
and main () =
  welcome ();
  let rec load' () =
    try
      let u = match read_line () |> String.trim with
        | "quit" -> exit 0
        | s -> load s in
      if (valid_save u) then u else
        (print_string [] "Invalid save file.\n>>> "; load' ())
    with
    | Not_found ->
      print_string [] "No save with that name.\n>>> "; load' ()
    | Corrupted ->
      print_string [] "Save file has been corrupted.\n>>> "; load' ()
  in
  load' () |> main_run

let _ = main ()

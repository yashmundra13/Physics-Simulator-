open Input
open Simulator
open ANSITerminal

let disp = true
let runtime = 30.
let fps = 144.
let steps_per_frame = 100.
let steps_disp = runtime *. fps *. steps_per_frame
let steps_comp = 60000000.

(** runs the simulation and displays graphically*)
let main_run u t =
  if disp then
    let dt = t /. steps_disp in
    let rec main_run' u t' =
      display u;
      print_string [] ("Time is runing at " ^ string_of_float (t /. runtime) ^
                       "x speed.");
      set_cursor 1 50;
      Unix.sleepf (1.0 /. fps);
      if t' > 0. then
        main_run' (run u (dt *. steps_per_frame) dt)
                  (t' -. (dt *. steps_per_frame))
      else
        u
    in main_run' u t
  else
    let dt = t /. steps_disp in
    run u t dt

(**[help ()] displays a list of accepted commands *)
let help () =
  print_string [] "[quit] exits the program\n";
  print_string [] "[add name:string mass:int position:(px:float,py:float) velocity:(vx:float,vy:float) radius:float]
    adds a body of specified type to the universe\n";
  print_string [] "[remove name:string] removes a body of specified name from the universe\n";
  print_string [] "[run 0] displays the simulation at this instance.\n";
  print_string [] "[run t:int] runs the simulation for t amount of time \n";
  print_string [] "[help] prints this message\n";
  print_string [] "[print] prints the current state of the universe\n";
  print_string [] "[save savename:string] saves the current universe to savename\n";
  print_string [] "[load savename:string] loads the universe saved to savename\n";
  print_string [] "\n"

(**[welcome () ]displays a welcome message *)
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
                       :                    enter 'help' for the list of commands
                       :

"
(**[main ()] initializes the interface *)
let main () =
  welcome ();
  let rec main' u =
    let u = try
        begin match print_string [] ">>> "; read_line () |> parse with
          | Quit -> exit 0
          | Add b -> add b u
          | Remove b -> begin try remove b u with
              | Not_found ->
                print_string [] ("No body named '" ^ b ^ "' to remove.\n"); u
            end
          | Run i -> main_run u (float_of_int i)
          | Help -> help (); u
          | Print -> format_universe Format.std_formatter u; u
          | Save s -> save u s; u
          | Load s -> try load s with
            | Not_found -> print_string [] "No save with that name.\n"; u
            | Corrupted -> print_string [] "Save file has been corrupted.\n"; u
        end
      with
      | Malformed -> print_string []
        "Invalid command.\nUse 'help' to see a list of commands.\n"; u
      | Empty -> u
    in main' u in
  main' []

let _ = main ()

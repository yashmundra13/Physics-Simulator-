open ANSITerminal
open Input

let g = 6.674e-11

exception Corrupted

type vector = float list


(** Rep invariant: no two bodies in a universe can share a name and the bodies
                   are stored in ascending order by name. *)
type universe = body list

let remove n u =
  let rec remove_helper n u acc =
    match u with
    | [] -> raise Not_found
    | h::t ->
      if h.name = n then (acc@t)
      else (remove_helper n t [h]@acc)
  in
  remove_helper n u []

let add (b : body) (u : universe) =
  List.sort (fun a b ->
      (compare a.name b.name)
    ) (b :: (try remove b.name u with Not_found -> u))

(**[distance a b] is the distance from a to b *)
let distance a b = match a.position, b.position with
  | [x1;y1],[x2;y2] -> sqrt (((x1 -. x2) ** 2.0) +. ((y1 -. y2) ** 2.0))
  | _ -> failwith "bad bodies"

(**[dir a b] is a unit vector pointing in the direction of b from a *)
let dir a b = match a.position, b.position with
  | [x1; y1], [x2; y2] ->
    [(x2 -. x1) /. (distance a b); (y2 -. y1) /. (distance a b)]
  | _ -> failwith "bad bodies"

(** [force a b] is the magnitude of the force on a due to the gravity between a and b *)
let force a b  =
  List.map (fun x ->
      x *. (g *. a.mass *. b.mass) /. ((distance a b) ** 2.0)
    ) (dir a b)
(** [acceleration a b] is the acceleration vector for a due to b*)
let acceleration a b =
  List.map(fun x -> x/.a.mass) (force a b)
(** [addv a b] is the sum of vectors and b*)
let addv a b= match a, b with
  | [x1; y1], [x2; y2] -> [x1 +. x2; y1 +. y2]
  | _ -> failwith "bad vectors"
(** [a_field u] is a function that takes a body and returns an acceleration*)
let a_field u =
  List.fold_left (fun f b ->
      fun b' -> if b = b' then f b' else addv (f b') (acceleration b' b)
    ) (fun _ -> [0.0;0.0]) u

(**[v_after_collision b1 b2]
   gives the resultant velocity after b1 and b2 collide*)
let v_after_collision b1 b2 =
  match b1.mass, b1.velocity ,b2.mass, b2.velocity with
  |m1, [vx1;vy1], m2, [vx2;vy2] ->
    [(m1*.vx1 +. m2*.vx2)/.(m1 +. m2); (m1*.vy1 +. m2*.vy2)/.(m1 +. m2)]
  | _ -> failwith "bad inputs"

(**[subv]  is the vecotr subtracion of vectors a and b*)
let subv a b = match a, b with
  | [x1; y1], [x2; y2] -> [x1 -. x2; y1 -. y2]
  | _ -> failwith "bad vectors"


(**[collisison b1 b2 dt] is true if b1 and b2 collide in the timeframe dt
   Math based on https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect*)
let collision b1 b2 dt =
  let p1 = b1.position in
  let p2 = b2.position in
  let v1 = [dt *. (List.nth b1.velocity 0);dt *. (List.nth b1.velocity 1)] in
  let v2 = [dt *. (List.nth b2.velocity 0);dt *. (List.nth b2.velocity 1)] in
  let cross2D [x1;y1] [x2;y2] = x1*.y2 -. y1*.x2 in
  let dotv [x1;y1] [x2;y2] = x1*.x2 +. y1*.y2 in
  if v1 = [0.;0.] && v2 = [0.;0.] then false
  else if (cross2D v1 v2 = 0.) && cross2D (subv p2 p1) v1 = 0. then
    let t0 = dotv (subv p2 p1) v1 /. (dotv v1 v1) in
    let t1 = t0 +. (dotv v2 v1)/.(dotv v1 v1) in
    0. <= t0 && t0 <= 1. && 0. <=  t1 &&  t1 <= 1.
  else if (cross2D v1 v2 = 0.) then false
  else
    let t = cross2D (subv p2 p1) v2 /. cross2D v1 v2 in
    let u =  cross2D (subv p2 p1) v1 /. cross2D v1 v2 in
    (0. <= t && t <= 1. && 0. <= u && u <= 1.) ||
    ((distance b1 b2) <= b1.radius +. b2.radius)

(**[p_after_collision b1 b2]
   gives the resultant position after b1 and b2 collide*)
let p_after_collision b1 b2 dt =
  let p1 = b1.position in
  let p2 = b2.position in
  let v1 = [dt *. (List.nth b1.velocity 0);dt *. (List.nth b1.velocity 1)] in
  let v2 = [dt *. (List.nth b2.velocity 0);dt *. (List.nth b2.velocity 1)] in
  let dotv [x1;y1] [x2;y2] = x1*.x2 +. y1*.y2 in
  let cross2D [x1;y1] [x2;y2] = x1*.y2 -. y1*.x2 in
  if (cross2D v1 v2 = 0.) then
    let t0 = dotv (subv p2 p1) v1 /. (dotv v1 v1) in
    let t1 = t0 +. (dotv v2 v1)/.(dotv v1 v1) in
    let t = min t0 t1 in
    match p1 , v1 with |[x1;y1],[x2;y2] -> [x1 +. t*.x2; y1 +. t*.y2]
                       |_ -> [0.;0.]
  else
    let t = cross2D (subv p2 p1) v2 /. cross2D v1 v2 in
    match p1 , v1 with |[x1;y1],[x2;y2] -> [x1 +. t*.x2; y1 +. t*.y2]
                       |_ -> [0.;0.]

(**[extract k list] is the list of all k number combinations of list*)
let rec extract k list =
  if k <= 0 then [ [] ]
  else match list with
    | [] -> []
    | h :: tl ->
      let with_h = List.map (fun l -> h :: l) (extract (k-1) tl) in
      let without_h = extract k tl in
      with_h @ without_h;;
(** [collision_helper u dt] is (a,b) where a is the list of bodies 
    to be removed and b is the list of bodies to be added after collisions occur.*)
let collision_helper u dt =
  let l = extract 2 u in
  let rec helper list dt (acc : body list * body list) =
    match list with
    |[] -> acc
    |h::t ->
      let x = List.nth h 0 in
      let y = List.nth h 1 in
      let cond  = (fun x y ->
          if x.position = [0.;0.] || x.velocity = [0.;0.]then (collision y x dt)
          else (collision x y dt)) in
      if cond x y then
        let b = {
          name = x.name ^ y.name;
          mass = x.mass +. y.mass;
          position = p_after_collision x y dt;
          velocity = v_after_collision x y ;
          radius = max x.radius y.radius
        }
        in helper t dt (([x]@[y]@(fst acc)),([b]@(snd acc)))
      else helper t dt acc
  in
  helper l dt ([],[])

(** [remover l u] is u after elements of l are removed from it*)
let rec remover l u =
  match l with
  |[] -> u
  |h::t ->
    remover t (remove h.name u)

(** [adder l u] is u after elements of l are added to it*)
let rec adder l u =
  match l with
  |[] -> u
  |h::t -> adder t (add h u)


let step u dt =
  let acc = a_field u in
  let new_u = List.map (fun b -> {
        b with
        position = addv b.position (List.map (fun v ->
            v *. dt) b.velocity);
        velocity = addv b.velocity (List.map (fun a ->
            a *. dt) (acc b));
      }) u
  in
  let x = collision_helper u dt in
  remover (fst x) (adder (snd x) new_u )

let display ?play:(play=false) u=
  let max_rad = 2.0 ** (((List.fold_left (fun m {position = [x;y]} ->
      max (max m (int_of_float x |> abs)) (int_of_float y |> abs)
    ) 1 u) |> float_of_int |> ( *. ) 1.15 |> log) /. (log 2.0) |> ceil) in
  resize 100 50;
  set_cursor 1 1;
  erase Screen;
  List.iter (fun {name = name; position = x::y::t} ->
      let x = x /. max_rad *. 50. in
      let y = y /. max_rad *. 25. in
      set_cursor (int_of_float (50. +. x)) (int_of_float (25. -. y));
      print_string [] (if play then
                         match name with
                         | ">" | "<" | "^" | "v" -> name
                         | "Target" -> "O"
                         | _ -> "@"
                       else
                         "@ " ^ name)
    ) u;
  set_cursor 1 48;
  print_string [] ("Screen is 2^" ^ ((log max_rad) /. (log 2.0) |> int_of_float |> string_of_int)
                   ^ "m squared.");
  set_cursor 1 49

let rec run u t dt =
  if t <= 0. then u else run (step u dt) (t -. dt) dt

let save u s =
  let file_name = "saves/" ^ s in
  let continue () =
    print_string [] "A save file with that name exists. Would you like to overwrite it? (y/n)\n>>> ";
    let s = String.get (read_line ()) 0 in
    s = 'y' || s = 'Y'
  in
  if (not (Sys.file_exists (file_name))) || continue () then
    let f = open_out file_name in
    List.iter (fun b -> match b with
        | {name = n; mass = m; position = [px; py]; velocity = [vx; vy]; radius = r} ->
          Printf.fprintf f "add %s %f (%f, %f) (%f, %f) %f\n" n m px py vx vy r
        | _ -> ()
      ) u;
    flush f;
    close_out f
  else
    ()

let load s =
  let file_name = "saves/" ^ s in
  try if Sys.file_exists file_name then
      let f = open_in file_name in
      let rec read_lines u =
        try
          match input_line f |> parse with
          | Add b -> (add b u |> read_lines)
          | _ -> raise Corrupted
        with
        | End_of_file -> u
        | Empty -> read_lines u
        | Malformed -> raise Corrupted
      in
      read_lines []
    else
      raise Not_found
  with
  | Sys_error s -> raise Not_found

let format_body fmt b =
  let print_string = Format.pp_print_string fmt in
  let print_float = Format.pp_print_float fmt in
  let print_vector units v = List.iter (fun a ->
      Format.pp_print_float fmt a; print_string (units ^ " ")) v in
  print_string (b.name ^ ":\n");
  print_string "  mass: "; print_float b.mass; print_string " kg\n";
  print_string "  position: "; print_vector "m" b.position; print_string "\n";
  print_string "  velocity: "; print_vector "m/s" b.velocity; print_string "\n";
  print_string "  radius: "; print_float b.radius; print_string " m\n";
  print_string "\n"

let format_universe fmt u =
  List.iter (fun b -> format_body fmt b) u

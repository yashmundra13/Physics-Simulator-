open Str

exception Malformed
exception Empty

let r = regexp "[(), ]"

type body = {
  name : string;
  mass : float;
  position : float list;
  velocity : float list;
  radius : float;
}

type command =
  | Quit
  | Add of body
  | Remove of string
  | Run of int
  | Help
  | Print
  | Save of string
  | Load of string

let float_of_s s =
  try float_of_string s
  with Failure _ -> raise Malformed

let parse s =
  match s |> Str.split r |> List.filter (fun x -> x <> "") with
  | [] -> raise Empty
  | h::[] when (String.lowercase_ascii h) = "quit" -> Quit
  | h::[] when (String.lowercase_ascii h) = "help" -> Help
  | h::[] when (String.lowercase_ascii h) = "print" -> Print
  | h::t when (String.lowercase_ascii h) = "remove" ->
    if List.length t = 1 then Remove((List.nth t 0))
    else raise Malformed
  | h::t when (String.lowercase_ascii h) = "run" ->
    if List.length t = 1 then
      try Run(int_of_string(List.nth t 0))
      with Failure(x) -> raise Malformed
    else raise Malformed
  | h::t when (String.lowercase_ascii h) = "add"->
    if List.length t = 7 then
      try
        let vx = float_of_s(List.nth t 4) in
        let vy = float_of_s(List.nth t 5) in
        let px = float_of_s(List.nth t 2) in
        let py = float_of_s(List.nth t 3) in
        let (b : body) = {
          name =  List.nth t 0;
          mass = float_of_string(List.nth t 1);
          position = [px;py];
          velocity = [vx;vy];
          radius = float_of_string(List.nth t 6);
        }
        in
        Add(b)
      with Failure(_) -> raise Malformed
    else raise Malformed
  | h::t::[] when (String.lowercase_ascii h) = "save" ->
    Save t
  | h::t::[] when (String.lowercase_ascii h) = "load" ->
    Load t
  | _ -> raise Malformed

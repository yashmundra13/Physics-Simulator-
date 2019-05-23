(**
   Parses the user's commands.
*)

(** [Malformed] is raised when a malformed command is encountered. *)
exception Malformed

(** [Empty] is raised when an empty command is encountered. *)
exception Empty

type body = {
    name : string;
    mass : float;
    position : float list;
    velocity : float list;
    radius : float;
  }

(** The type [command] represents the user's command. *)
type command =
  | Quit
  | Add of body
  | Remove of string
  | Run of int
  | Help
  | Print
  | Save of string
  | Load of string

(** [parse str] parses a user's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. The rest of the words, if any, become the parameters of the
    command.

    Examples:
    - [parse "Add earth 10000 (0,0) (10,10) 100"] is
    [Add Body {
      name = "Earth";
      mass = 10000;
      position = (0, 0);
      velocity = (10, 10);
      radius = 100;
    }]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9), space
    characters (only ASCII character code 32; not tabs or newlines, etc.),
    parenthesis, commas, and periods.

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the verb is not a command or is not followed by the appropriate parameters.
*)
val parse : string -> command

(**
   Simulates a universe.
 *)

exception Corrupted

(** [g] is the gravitational constant = 6.674E-11 m^3/kg s^2. *)
val g : float

(** [vector] is the type representing a vector. *)
type vector = float list

(** [body] is the type representing a body in space.

    Rep invariant: the mas and radius must be non negative.
 *)

(** [universe] is the type representing a collection of bodies. *)
type universe = Input.body list

(** [add b u] is a copy of the universe [u] with the addition of the body [b].
    If a body with name the same name as [b] exists in [u], it is overwritten.
*)
val add : Input.body -> universe -> universe

(** [remove n u] is a copy of the universe [u] without the body named [n].

    Requires: There is a body in [u] named [n].

    Raises: [Not_found] if no body in [u] has the name [n].
 *)
val remove : string -> universe -> universe

(** [step u t] is the universe [u] after [t] time has elapsed. *)
val step : universe -> float -> universe

(** [diplay u] prints a visual representation of the universe to [stdout]. *)
val display : ?play:bool -> universe -> unit

(** [run u t dt] is the universe [u] after time [t] where each step of the
    simulation takes time [dt]. If [t] is zero or negative, the original
    universe is returned.
 *)
val run : universe -> float -> float -> universe

(** [save u s] writes the contents of the universe [u] to the file "saves/[s]".
    If the file already exists, it asks for confirmation to overwrite it.*)
val save : universe -> string -> unit

(** [load s] is the universe in the file "saves/s".

    Requires: [s] exists and contains a valid save file.

    Raises: [Not_found] if there is no file named "saves/s".
            [Corrupted] if the file "saves/s" does not contain valid save data.
*)
val load : string -> universe

(** A printing function for bodies suitable for the top level.*)
val format_body : Format.formatter -> Input.body -> unit

(** A printing function for universes suitable for the top level.*)
val format_universe : Format.formatter -> universe -> unit

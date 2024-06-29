
(** A stream of characters that is indexed by a position. *)

(** The characters in a character stream provided by this module are accessed
    based on their position in the stream. A position [pos] is valid in the
    stream [s] if it satisfies [0 <= pos < length s]. Character streams can
    be created from input channels and from strings.
*)


type t
(** A stream of characters. *)

type index = int
(** An index inside the stream. *)

type pos = {
  line: int;
  column: int;
}
(** A visual position in text, with lines and columns starting at 1. *)


(** {2 Peeking into the stream} *)

val read_char: t -> index -> char option
(** [read_char s i] returns the character in stream [s] indexed by [i],
    unless the index is invalid. *)

val match_char: char -> t -> index -> bool
(** [match_char c st i] is [true] if [c] is the character at index [i]
    in stream [st]. *)

val match_string: string -> t -> index -> bool
(** [match_string s st i] returns [true] if [s] is the string at index [i]
    in stream [st]. *)

val skip_newline: t -> index -> index option
(** [skip_newline st i] returns [Some i'] if stream [st] contains a
    well-formed newline at index [i], where [i'] is the index of the
    character that follows that newline. *)

val from_string: string -> t
(** [from_string s] creates a character stream that contains the characters
    of the string [s]. *)

val sub: t -> index -> len: int -> string
(** [sub s start len] is a string of length [len], containing the part
    of of stream [s] that starts at position [start] and has length [len]. *)

val length: t -> int
(** [length s] is the length (in characters) of stream [s]. *)


(** {2 Handling positions} *)

val get_pos: t -> index -> pos
(** [get_pos s i] returns the position in [s] indexed by [i]. If [i] is out
    of bounds, the function returns either starting or ending position. *)

val print_info: width: int -> indent: int -> t -> index -> string
(** [print_info ~width ~indent s] prints the position info corresponding
    to stream [s]. [width] determines the maximum width (in characters)
    of the resulting message. [indent] is the indentation offset.

    Warning: positions are calculated in bytes (with [get_pos]), which may
    cause invalid display of error locations on UTF-8-enabled terminals. *)

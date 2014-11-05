(** Read and write bencode files in OCaml *)

type t = 
  | Integer of int
  | String of string
  | List of t list
  | Dict of (string * t) list

type src = [ 
  | `Channel of in_channel 
  | `File_path of string
  | `String of string ]

type dst = [
  | `Channel of out_channel
  | `File_path of string
  | `Buffer of Buffer.t]

val eq : t -> t -> bool

val hash : t -> int

(** sort list and wrap it in [Dict]. The list should not contain
    the same key twice. *)
val dict_of_list : (string*t) list -> t

(** [pretty_print] is not tail recursive (or pretty) *)

val pretty_print : t -> string

val decode : [< src] -> t

(** [encode] is not tail recursive *)

val encode : [< dst] -> t -> unit

(** [encode_to_string] is not tail recursive *)

val encode_to_string : t -> string

type 'a sequence = ('a -> unit) -> unit
val decode_seq : [< src] -> t sequence
val encode_seq : [< dst] -> t sequence -> unit

(** {2 Helpers} *)

val as_string : t -> string option
val as_int : t -> int option
val as_list : t -> t list option
val as_dict : t -> (string * t) list option
val dict_get : t -> string -> t option

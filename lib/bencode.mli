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

(** [pretty_print] is not tail recursive (or pretty) *)

val pretty_print : t -> string

val decode : [< src] -> t

(** [encode] is not tail recursive *)

val encode : [< dst] -> t -> unit

(** [encode_to_string] is not tail recursive *)

val encode_to_string : t -> string

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

val pretty_print : t -> string

val decode : [< src] -> t

(** encoding is NOT tail recursive (for now) *)
val encode : [< dst] -> t -> unit

val encode_to_string : t -> string

(** {1 Non-Blocking IO} *)

type t = Bencode.t
type bencode = t

type 'a sequence = ('a -> unit) -> unit

(** {2 Serialization (encoding)} *)

module Encode : sig
  val size : bencode -> int
    (** Size needed for serialization, in bytes *)

  val write_in_string : t -> Bytes.t -> int -> unit
    (** [write_in_string v buf o] writes the value [v] in  the string,
        starting at offset [o]. The portion of the string starting from [o]
        must be big enough (ie >= [size v]) *)

  val to_buf : Buffer.t -> t -> unit
  val to_string : t -> string
  val to_bytes : t -> Bytes.t
  val to_chan : out_channel -> t -> unit
  val fmt : Format.formatter -> t -> unit

  val to_seq : bencode -> Bencode_token.t sequence
    (** Traverse the Bencode value as a sequence of tokens *)

  val to_list : bencode -> Bencode_token.t list

  val put : Bencode_token.Encode.t -> t -> unit
    (** Put the whole value on the given token encoder *)
end

(** {2 Pretty printing of values} *)

val pretty : Format.formatter -> t -> unit
  (** Print the tree itself, not its encoding *)

val pretty_to_str : t -> string
  (** Print the tree into a string *)

(** {2 Deserialization (decoding)}
Deserialization is based on the {! Bencode_token.Decode} module. Parsing can be
incremental, in which case the input is provided chunk by chunk and
the decoder contains the parsing state. Once a B-encoded value
has been parsed, other values can still be read.
*)

module Decode : sig
  type t

  val create : Bencode_token.Decode.t -> t

  val of_string : string -> t
  val of_bytes : Bytes.t -> t
  val of_chan : in_channel -> t
  val manual : unit -> t

  type parse_result =
    | ParseOk of bencode
    | ParseError of string
    | ParseEnd  (** end of input *)
    | ParsePartial (** Await more input *)

  val feed : t -> string -> int -> int -> unit
  (** Provide some more input (the subtstring). Only useful for
      manual, non-blocking parsing. *)

  val feed_bytes : t -> Bytes.t -> int -> int -> unit

  val next : t -> parse_result
  (** Parse next value *)

  val parse_string : string -> bencode option
  (** Parse the string. *)

  val parse_string_exn : string -> bencode
  (** @raise Failure if it fails to parse. *)
end

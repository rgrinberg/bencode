(** {1 Streaming Tokenization for Bencode} *)

type t =
  [
  | `I of int
  | `S of string
  | `BeginDict
  | `BeginList
  | `End
  ]

type token = t

type 'a sequence = ('a -> unit) -> unit

val to_string : t -> string

(** {2 Encode}
serializing a stream of tokens to a buffer or a channel, in text form *)

module Encode : sig
  type t

  val to_buf : Buffer.t -> t
    (** Encode directly to the given buffer *)

  val to_chan : out_channel -> t
    (** Write on the given channel *)

  val put : t -> token -> unit
  val put_many : t -> token sequence -> unit
end

(** {2 Decode}
read a stream of tokens from a channel or string *)

module Decode : sig
  type t

  type result =
    | Next of token
    | End
    | Error of string
    | Await (** more input, for non blocking-IO *)

  val of_string : string -> t

  val of_bytes : Bytes.t -> t

  val of_slice : string -> int -> int -> t

  val of_bytes_slice : Bytes.t -> int -> int -> t

  val of_chan : in_channel -> t

  val manual : unit -> t
    (** Input will have to provided by hand *)

  val feed : t -> string -> int -> int -> unit
    (** Feed a substring to the decoder *)

  val feed_bytes : t -> Bytes.t -> int -> int -> unit

  val next : t -> result
    (** Next token, or another result *)

  val iter : t -> (token -> unit) -> unit
    (** Iterate on tokens that can be read without errors
        nor starvation. *)

  val to_list : t -> token list option
    (** List of tokens that can be read without starvation or error *)
end

(** {2 Shortcuts} *)
module Easy : sig
  val to_string : token list -> string

  val output : out_channel -> token list -> unit

  val of_string : string -> token list option

  val of_bytes : Bytes.t -> token list option

  val of_string_exn : string -> token list
    (** @raise Failure if the string isn't valid *)
end

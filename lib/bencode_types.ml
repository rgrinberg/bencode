type t =
  | Integer of int64
  | String of string
  | List of t list
  | Dict of (string * t) list

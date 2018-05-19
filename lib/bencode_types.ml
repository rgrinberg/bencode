type t =
  | Integer of int
  | String of string
  | List of t list
  | Dict of (string * t) list

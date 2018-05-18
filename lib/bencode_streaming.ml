type t = Bencode.t
open Bencode

type bencode = t
type 'a sequence = ('a -> unit) -> unit

(** {2 Serialization (encoding)} *)

module Encode = struct
  let _len_min_int = String.length (string_of_int min_int)

  (* length of an encoded int, in bytes *)
  let _len_int i =
    match i with
    | 0 -> 1
    | _ when i=min_int -> _len_min_int
    | _ when i < 0 -> 2 + int_of_float (log10 (float_of_int ~-i))
    | _ -> 1 + int_of_float (log10 (float_of_int i))

  (* length of an encoded string, in bytes *)
  let _len_str s =
    _len_int (String.length s) + 1 + String.length s

  let rec size t = match t with
    | Integer i -> 2 + _len_int i
    | String s -> _len_str s
    | List l -> List.fold_left (fun acc i -> acc + size i) 2 l
    | Dict map -> List.fold_left (fun acc (k,v) -> acc + _len_str k + size v) 2 map

  let write_in_string t buf o =
    let pos = ref o in
    let rec append t = match t with
    | Integer i -> write_char 'i'; write_int i; write_char 'e'
    | String s -> write_str s
    | List l ->
      write_char 'l';
      List.iter append l;
      write_char 'e';
    | Dict m ->
      write_char 'd';
      List.iter (fun (key, t') -> write_str key; append t') m;
      write_char 'e'
    and write_int i =
      let s = string_of_int i in
      Bytes.blit_string s 0 buf !pos (String.length s);
      pos := !pos + String.length s
    and write_str s =
      write_int (String.length s);
      write_char ':';
      String.blit s 0 buf !pos (String.length s);
      pos := !pos + String.length s
    and write_char c =
      Bytes.set buf !pos c;
      incr pos
    in
    append t

  let to_bytes t =
    let len = size t in
    let s = Bytes.create len in
    write_in_string t s 0;
    s

  let to_string t =
    Bytes.unsafe_to_string (to_bytes t)

  let to_buf buf t =
    Buffer.add_string buf (to_string t)

  let to_chan ch t =
    let b = Buffer.create 25 in
    to_buf b t;
    Buffer.output_buffer ch b

  let fmt formatter t =
    let b = Buffer.create 25 in
    to_buf b t;
    Format.pp_print_string formatter (Buffer.contents b)

  let rec to_seq b k = match b with
    | Integer i -> k (`I i)
    | String s -> k (`S s)
    | Dict l ->
        k `BeginDict;
        List.iter (fun (key,v) -> k (`S key); to_seq v k) l;
        k `End
    | List l ->
        k `BeginList;
        List.iter (fun b' -> to_seq b' k) l;
        k `End

  let to_list b =
    let l = ref [] in
    to_seq b (fun x -> l := x :: !l);
    List.rev !l

  let put enc t =
    Bencode_token.Encode.put_many enc (to_seq t)
end

let rec pretty fmt t = match t with
  | Integer i -> Format.fprintf fmt "%d" i
  | String s -> Format.fprintf fmt "@[<h>\"%s\"@]" s
  | List l ->
    Format.fprintf fmt "@[<hov 2>[@,";
    List.iteri (fun i t' -> (if i > 0 then Format.pp_print_char fmt ' '); pretty fmt t') l;
    Format.fprintf fmt "]@]";
  | Dict d ->
    Format.fprintf fmt "@[<hov 2>{@,";
    List.iter
      (fun (k,t') -> Format.fprintf fmt "%a -> %a@ " pretty (String k) pretty t')
      d;
    Format.fprintf fmt "}@]";
    ()

let pretty_to_str t =
  let b = Buffer.create 15 in
  Format.fprintf (Format.formatter_of_buffer b) "%a@?" pretty t;
  Buffer.contents b

(** {2 Deserialization (decoding)} *)

module Decode = struct
  module BT = Bencode_token

  type state =
    | StateDoGoOn
    | StateError of string

  type parse_result =
    | ParseOk of t
    | ParseError of string
    | ParseEnd  (** end of input *)
    | ParsePartial

  type partial_state =
    | PS_L of bencode list
    | PS_D of (string*bencode)list  (* in dictionary *)
    | PS_D_key of string * (string*bencode)list (* parsed key, wait for value *)

  type t = {
    dec : Bencode_token.Decode.t;
    mutable state : state;
    mutable stack : partial_state list;
  }

  let create dec = {
    dec;
    state = StateDoGoOn;
    stack = [];
  }

  let of_string s = create (BT.Decode.of_string s)
  let of_bytes s = create (BT.Decode.of_bytes s)
  let of_chan oc = create (BT.Decode.of_chan oc)
  let manual () = create (BT.Decode.manual ())

  let feed dec s i len =
    BT.Decode.feed dec.dec s i len

  let feed_bytes dec s i len =
    BT.Decode.feed_bytes dec.dec s i len

  (* how to fail: set state to an error *)
  let _fail : t -> ('a, Buffer.t, unit, parse_result) format4 -> 'a = fun dec fmt ->
    let buf = Buffer.create 16 in
    Printf.kbprintf
      (fun buf ->
        let msg = Buffer.contents buf in
        dec.state <- StateError msg;
        ParseError msg)
      buf fmt

  (* atomic bencode value from token *)
  let _atom = function
    | `I i -> Integer i
    | `S s -> String s
    | _ -> assert false

  let rec next dec =
    match dec.state with
    | StateError e -> ParseError e
    | StateDoGoOn -> _next dec

  (* proper switch *)
  and _next dec =
    match BT.Decode.next dec.dec with
    | BT.Decode.Await -> ParsePartial
    | BT.Decode.End ->
        begin match dec.stack with
        | [] -> ParseEnd
        | _ -> _fail dec "unexpected end of input"
        end
    | BT.Decode.Error e -> _fail dec "lexing error: %s" e
    | BT.Decode.Next tok ->
        begin match tok, dec.stack with
        | `S key, PS_D l :: stack' ->
            dec.stack <- PS_D_key (key, l)::stack';
            _next dec
        | (`I _ | `S _), _ ->
            (* return a value (possibly into the top container) *)
            _return dec (_atom tok)
        | `BeginDict, stack' ->
            dec.stack <- (PS_D []) :: stack';
            _next dec
        | `BeginList, stack' ->
            dec.stack <- (PS_L []) :: stack';
            _next dec
        | `End, PS_D l :: stack' ->
            dec.stack <- stack';
            _return dec (Dict (List.rev l))
        | `End, PS_L l :: stack' ->
            dec.stack <- stack';
            _return dec (List (List.rev l))
        | `End, [] ->
            _fail dec "unexpected 'e' at top-level"
        | `End, PS_D_key _ :: _ ->
            _fail dec "unexpected 'e' when parsing dictionary entry"
        end

  (* push [v] as a completed value *)
  and _return dec v =
    assert (dec.state = StateDoGoOn);
    match dec.stack with
    | [] ->
        ParseOk v (* yield! *)
    | (PS_L l::stack') ->
        dec.stack <- PS_L (v :: l)::stack';
        next dec
    | (PS_D_key (key,l) :: stack') ->
        dec.stack <- PS_D ((key, v) :: l)::stack';
        next dec
    | _ -> _fail dec "unexpected stack state"

  let parse_string s =
    match next (of_string s) with
    | ParseOk b -> Some b
    | _ -> None

  let parse_string_exn s =
    match next (of_string s) with
    | ParseOk b -> b
    | ParseError msg -> failwith msg
    | ParseEnd -> failwith "unexpected EOF"
    | ParsePartial -> failwith "awaiting input"
end


type t = Bencode.t
open Bencode

(** {2 Serialization (encoding)} *)

(* length of an encoded int, in bytes *)
let _len_int i =
  match i with
  | 0 -> 1
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
    String.blit s 0 buf !pos (String.length s);
    pos := !pos + String.length s
  and write_str s =
    write_int (String.length s);
    write_char ':';
    String.blit s 0 buf !pos (String.length s);
    pos := !pos + String.length s
  and write_char c =
    buf.[!pos] <- c;
    incr pos
  in
  append t

let to_string t =
  let len = size t in
  let s = String.create len in
  write_in_string t s 0;
  s

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

(** Deserialization is based on the {! decoder} type. Parsing can be
    incremental, in which case the input is provided chunk by chunk and
    the decoder contains the parsing state. Once a B-encoded value
    has been parsed, other values can still be read. *)

type decoder = {
  mutable buf : string;  (* buffer *)
  mutable i : int;  (* index in buf *)
  mutable len : int;  (* length of substring to read *)
  mutable c : int;  (* line *)
  mutable l : int;  (* column *)
  mutable state : parse_result;
  mutable stack : partial_state list;
}

(** Result of parsing *)
and parse_result =
  | ParseOk of t
  | ParseError of string
  | ParsePartial

(** Partial state of the parser *)
and partial_state =
  | PS_I of bool * int (* sign and integer *)
  | PS_S of int ref * string   (* index in string, plus string *)
  | PS_L of t list
  | PS_D of (string*t)list  (* in dictionary *)
  | PS_D_key of string * (string*t)list (* parsed key, wait for value *) 
  | PS_return of t  (* bottom of stack *)
  | PS_error of string (* error *)

let mk_decoder () =
  let dec = {
    buf = "";
    i = 0;
    len = 0;
    c = 0;
    l = 0;
    state = ParsePartial;
    stack = [];
  } in
  dec

let is_empty dec = dec.len = 0
let cur dec = dec.buf.[dec.i]

let junk dec =
  (* update line/column *)
  (if cur dec = '\n'
    then (dec.c <- 0; dec.l <- dec.l + 1)
    else dec.c <- dec.c + 1);
  dec.i <- dec.i + 1;
  dec.len <- dec.len - 1

let next dec =
  let c = cur dec in
  junk dec;
  c

(* parse value *)
let rec parse_rec dec =
  match dec.stack with
  | [PS_return v] ->  (* return value *)
    dec.stack <- [];
    dec.state <- ParseOk v;
    dec.state
  | [PS_error s] -> (* failure *)
    dec.stack <- [];
    dec.state <- ParseError s;
    dec.state
  | _ ->
    if is_empty dec then ParsePartial (* wait *)
    else begin
      let c = next dec in
      (match dec.stack, c with
      | (PS_I (sign, i)) :: stack, '0' .. '9' ->
        dec.stack <- PS_I (sign, (Char.code c - Char.code '0') + 10 * i) :: stack;
      | (PS_I (_, 0)) :: stack, '-' ->
        dec.stack <- PS_I (false, 0) :: stack  (* negative number *)
      | (PS_I (sign, i)) :: stack, 'e' ->
        dec.stack <- stack;
        push_value dec (Integer (if sign then i else ~- i))
      | ((PS_D _ | PS_D_key _ | PS_L _) :: _ | []), '0' .. '9' ->
        (* initial length of string *)
        dec.stack <- (PS_I (true, Char.code c - Char.code '0')) :: dec.stack
      | (PS_I (sign, i)) :: stack, ':' ->
        if i < 0
          then error dec "string length cannot be negative"
        else if i = 0 then  (* empty string *)
          let _ = dec.stack <- stack in
          push_value dec (String "")
        else (* prepare to parse a string *)
          dec.stack <- (PS_S (ref 0, String.create i)) :: stack;
      | (PS_S (n, s)) :: stack, _ ->
        s.[!n] <- c;
        incr n;
        (* value completed *)
        (if !n = String.length s
          then
            let _ = dec.stack <- stack in
            push_value dec (String s));
      | stack, 'i' ->
        dec.stack <- (PS_I (true, 0)) :: stack
      | stack, 'l' ->
        dec.stack <- PS_L [] :: stack;
      | stack, 'd' ->
        dec.stack <- PS_D [] :: stack
      | (PS_L l) :: stack, 'e' -> (* end of list *)
        dec.stack <- stack;
        push_value dec (List (List.rev l))
      | (PS_D d) :: stack, 'e' -> (* end of dict *)
        dec.stack <- stack;
        push_value dec (Dict (List.rev d))
      | (PS_D_key _) :: _, 'e' -> (* error *)
        error dec "missing value in dict"
      | _ -> (* generic error *)
        error dec (Printf.sprintf "expected value, got %c" c));
      parse_rec dec
    end
(* When a value is parsed, push it on the stack (possibly collapsing it) *)
and push_value dec v =
  match v, dec.stack with
  | _, [] ->
    dec.stack <- [PS_return v] (* finished *)
  | _, (PS_L l) :: stack ->
    (* add to list *)
    dec.stack <- (PS_L (v :: l)) :: stack;
  | String key, ((PS_D d) :: stack) ->
    (* new key for the map *)
    dec.stack <- (PS_D_key (key, d)) :: stack;
  | _, ((PS_D d) :: _) ->
    (* error: key must be string *)
    error dec "dict keys must be strings"
  | _, (PS_D_key (key, d)) :: stack ->
    (* new binding for the map *)
    dec.stack <- (PS_D ((key,v) :: d)) :: stack;
  | _ -> assert false
(* signal error *)
and error dec msg =
  let msg = Printf.sprintf "Bencode: error at line %d, column %d: %s"
    dec.l dec.c msg in
  dec.stack <- [PS_error msg]

(* exported parse function *)
let parse dec s i len =
  (if i < 0 || i+len > String.length s
    then invalid_arg "Bencode.parse: not a valid substring");
  (* add the input to [dec] *)
  if dec.len = 0
    then begin
      dec.buf <- String.copy s;
      dec.i <- i;
      dec.len <- len;
    end else begin
      (* use a buffer to merge the stored input and the new input *)
      let buf' = String.create (dec.len + len - dec.i) in
      String.blit dec.buf dec.i buf' 0 dec.len;
      String.blit s i buf' dec.len len;
      dec.buf <- buf';
      dec.i <- 0;
      dec.len <- dec.len + len - dec.i;
    end;
  (* state machine *)
  parse_rec dec

let parse_resume d = parse_rec d

let reset dec =
  dec.l <- 0;
  dec.c <- 0;
  dec.i <- 0;
  dec.len <- 0;
  dec.state <- ParsePartial;
  dec.stack <- [];
  ()

let state dec = dec.state

let rest dec =
  String.sub dec.buf dec.i dec.len

let rest_size dec =
  dec.len

let parse_string s =
  let dec = mk_decoder () in
  parse dec s 0 (String.length s)

let of_string s =
  match parse_string s with
  | ParseOk t -> t
  | ParsePartial -> invalid_arg "Bencode: partial parse"
  | ParseError msg -> invalid_arg msg

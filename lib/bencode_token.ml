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

module Encode = struct
  type t = {
    str : string -> unit;
    chr : char -> unit;
  }

  let to_buf b = {
    str = Buffer.add_string b;
    chr = (fun c -> Buffer.add_char b c);
  }

  let to_chan oc = {
    str = output_string oc;
    chr = output_char oc;
  }

  let put enc (tok:token) = match tok with
    | `I i ->
        enc.chr 'i';
        enc.str (string_of_int i);
        enc.chr 'e'
    | `S s ->
        enc.str (string_of_int (String.length s));
        enc.chr ':';
        enc.str s
    | `BeginDict -> enc.chr 'd'
    | `BeginList -> enc.chr 'l'
    | `End -> enc.chr 'e'

  let put_many enc seq =
    seq (put enc)
end

module Decode = struct
  type result =
    | Next of token
    | End
    | Error of string
    | Await (** more input, for non blocking-IO *)

  (* reading an integer: which sign does it have? *)
  type int_read_state =
    | PosInt    (* positive int *)
    | NegInt    (* negative int *)
    | AnyInt    (* don't know yet which kind of integer *)
    | ZeroInt   (* only 0 *)

  type state =
    | Start
    | ReadInt of int_read_state
    | ReadStringLen
    | ReadString
    | StateError of string

  type refill_result =
    | Refill_eof
    | Refill_read of int
    | Refill_error of string
    | Refill_await

  type t = {
    mutable cur_i : int;  (* when reading int, or string length *)
    mutable cur_s : string; (* when reading string *)
    mutable buf : string; (* input buffer *)
    mutable i : int;
    mutable len : int;
    mutable state : state;
    mutable refill : unit -> refill_result;
  }

  let _refill_stop () = Refill_eof
  
  let _default = {
    cur_i = 0;
    cur_s = "";
    buf = "";
    i = 0;
    len = 0;
    state = Start;
    refill = _refill_stop;
  }

  let of_string s = {
    _default with
    buf = s;
    len = String.length s;
  }

  let of_slice s i len = {
    _default with
    buf = s; i; len;
  }

  (* move the active slice of buffer to the beginning.
    postcondition: dec.i = 0 *)
  let _move_beginning dec =
    String.blit dec.buf dec.i dec.buf 0 dec.len;
    dec.i <- 0

  let of_chan ic =
    let len = 256 in
    let buf = String.make len ' ' in
    let dec = { _default with buf; len; } in
    let refill () =
      assert (dec.len >= 0);
      (* shift the partial content to the beginning, if any *)
      if dec.len > 0 then _move_beginning dec;
      dec.i <- 0;
      try
        let size = String.length dec.buf - dec.len in
        let n = input ic dec.buf dec.len size in
        Refill_read n
      with
      | End_of_file ->
        dec.refill <- _refill_stop;
        if dec.len = 0 then Refill_eof else Refill_read 0
      | e ->
          let err = Printexc.to_string e in
          dec.state <- StateError err;
          Refill_error err
    in
    dec.refill <- refill;
    dec

  let manual = { _default with refill = (fun () -> Refill_await); }

  let feed dec s j len' =
    match dec.state with
    | StateError _ -> ()
    | Start
    | ReadInt _
    | ReadStringLen
    | ReadString ->
      _move_beginning dec;
      dec.i <- 0;
      (* resize if needed *)
      if len' + dec.len > String.length dec.buf
        then begin
          let buf' = String.make (2*(len' + dec.len)) ' ' in
          String.blit dec.buf 0 buf' 0 dec.len;
          dec.buf <- buf';
        end;
      String.blit s j dec.buf dec.len len';
      ()

  (* how to fail: set state to an error *)
  let _fail : t -> ('a, Buffer.t, unit, result) format4 -> 'a = fun dec fmt ->
    let buf = Buffer.create 16 in
    Printf.kbprintf
      (fun buf ->
        let msg = Buffer.contents buf in
        dec.state <- StateError msg;
        Error msg)
      buf fmt

  let _is_digit_nonzero c =
    Char.code '1' <= Char.code c && Char.code c <= Char.code '9'
  let _is_digit c =
    Char.code '0' <= Char.code c && Char.code c <= Char.code '9'

  let _yield_int dec i =
    dec.state <- Start;
    dec.cur_i <- 0;
    Next (`I i)

  let rec next dec =
    match dec.state with
    | StateError e -> Error e
    | _ when dec.len = 0 -> _refill dec
    | ReadString ->
      (* bulk transfer. [n] is how many bytes we can transfer right now *)
      let n = min (String.length dec.cur_s - dec.cur_i) dec.len in
      String.blit dec.buf dec.i dec.cur_s dec.cur_i n;
      dec.i <- dec.i + n;
      dec.cur_i <- dec.cur_i + n;
      dec.len <- dec.len - n;
      if String.length dec.cur_s = dec.cur_i
        then begin
          let s = dec.cur_s in
          dec.cur_s <- "";
          dec.state <- Start;
          Next (`S s)  (* done! *)
        end
        else next dec
    | _ ->
      (* consume one char *)
      let c = dec.buf.[dec.i] in
      dec.len <- dec.len - 1;
      dec.i <- dec.i + 1;
      begin match dec.state, c with
      | StateError _, _ -> assert false
      | Start, 'd' -> Next `BeginDict
      | Start, 'l' -> Next `BeginList
      | Start, 'e' -> Next `End
      | Start, 'i' ->
        dec.state <- ReadInt AnyInt;
        dec.cur_i <- 0;
        next dec
      | Start, '0' ->
        dec.state <- ReadStringLen;
        dec.cur_i <- 0;
        next dec
      | Start, c when _is_digit_nonzero c ->
        dec.state <- ReadStringLen;
        dec.cur_i <- Char.code c - Char.code '0';
        next dec
      | Start, c ->
        _fail dec "unexpected char for B-encode expr: %c" c
      | ReadInt AnyInt, '-' ->
        dec.state <- ReadInt NegInt;
        next dec
      | ReadInt AnyInt, '0' ->
        (* allow exactly one leading 0 *)
        dec.state <- ReadInt ZeroInt;
        next dec
      | (ReadInt _ | ReadStringLen), '0' when dec.cur_i = 0 ->
        _fail dec "forbidden leading 0 while reading integer"
      | ReadInt AnyInt, c when _is_digit c ->
        (* case where we start reading a positive int *)
        assert (dec.cur_i = 0);
        dec.cur_i <- Char.code c - Char.code '0';
        dec.state <- ReadInt PosInt;
        next dec
      | (ReadInt (PosInt | NegInt) | ReadStringLen), c when _is_digit c ->
        (* add a digit to the integer *)
        dec.cur_i <- dec.cur_i * 10 + Char.code c - Char.code '0';
        next dec
      | ReadInt (PosInt | ZeroInt), 'e' ->
        (* finish reading an int *)
        _yield_int dec dec.cur_i
      | ReadInt NegInt, 'e' ->
        _yield_int dec (- dec.cur_i)
      | ReadInt AnyInt, 'e' ->
        _yield_int dec 0
      | ReadStringLen, ':' when dec.cur_i = 0 ->
        dec.state <- Start;
        Next (`S "")
      | ReadStringLen, ':' ->
        (* allocate buffer of the correct size *)
        dec.state <- ReadString;
        dec.cur_s <- String.make dec.cur_i ' ';
        dec.cur_i <- 0;
        next dec
      | ReadInt _, c ->
        _fail dec "expected digit or 'e', got %c" c
      | ReadStringLen , c ->
        _fail dec "expected digit or ':', got %c" c
      | ReadString, _ -> assert false
      end
      
  and _refill dec =
    match dec.refill () with
    | Refill_error e -> _fail dec "error during refill: %s" e
    | Refill_read n when n = 0 -> _refill dec  (* XXX: caution... *)
    | Refill_read n -> next dec (* available input, next *)
    | Refill_await -> Await
    | Refill_eof ->
        begin match dec.state with
        | Start -> End  (* ok, no leftover *)
        | ReadStringLen
        | ReadString -> _fail dec "unexpected EOF (was reading a string)"
        | ReadInt _ -> _fail dec "unexpected EOF (was reading an int)"
        | StateError e -> Error e
        end

  let rec iter dec k =
    match next dec with
    | Next tok -> k tok; iter dec k
    | _ -> ()

  let to_list dec =
    let rec iter acc = match next dec with
      | Next tok -> iter (tok::acc)
      | Await
      | End -> Some (List.rev acc)
      | Error _ -> None
    in
    iter []
end

module Easy = struct
  let to_string l =
    let buf = Buffer.create 24 in
    let enc = Encode.to_buf buf in
    Encode.put_many enc (fun k -> List.iter k l);
    Buffer.contents buf

  let output oc l =
    let enc = Encode.to_chan oc in
    Encode.put_many enc (fun k -> List.iter k l)

  let of_string s =
    let dec = Decode.of_string s in
    Decode.to_list dec

  let of_string_exn s =
    let dec = Decode.of_string s in
    match Decode.to_list dec with
    | Some l -> l
    | None ->
        begin match dec.Decode.state with
        | Decode.StateError e -> failwith e
        | _ -> failwith "invalid decoder state"
        end
end

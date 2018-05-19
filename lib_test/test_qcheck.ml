
module B = Bencode
module BS = Bencode_streaming

module Q = QCheck

let (|>) x f = f x

let arb_bencode =
  let gen =
    let open QCheck.Gen in
    let base =
      frequency
        [ 2, (ui64 >|= fun i -> B.Integer i);
          2, (small_int >|= fun i -> B.Integer (Int64.of_int i));
          1, (oneofl [Int64.min_int; Int64.max_int] >|= fun i -> B.Integer i);
          5, (string >|= fun s -> B.String s);
        ]
    in
    1--3 >>= fun n ->
    fix (fun sub size ->
      if size<=0 then base
      else frequency
          [ 1, map B.dict_of_list
              (small_list (pair string (sub (size-1))));
            1, map (fun l -> B.List l) (small_list (sub(size-1)));
            2, base;
          ]) n
  in
  let rec shrink_int64 : int64 Q.Shrink.t =
    fun x ->
      let open Q.Iter in
      ( ( return @@ Int64.div x 2_L )
        <+> (return @@ Int64.(sub x one))
      ) >>= shrink_int64
  in
  let rec shrink b =
    let open Q.Iter in
    match b with
      | B.List l -> Q.Shrink.list ~shrink l >|= fun l->B.List l
      | B.Integer i -> shrink_int64 i >|= fun i -> B.Integer i
      | _ -> Q.Iter.empty
  in
  Q.make
    ~print:BS.Encode.to_string
    ~small:BS.Encode.size
    ~shrink
    gen

let check_decode_encode =
  let prop b = B.eq (B.decode (`String (B.encode_to_string b))) b in
  let name = "bencode_decode_encode_bij" in
  Q.Test.make ~name arb_bencode prop

let check_decode_encode_streaming =
  let prop b =
    B.eq (BS.Decode.parse_string_exn (BS.Encode.to_string b)) b in
  let name = "bencode_streaming_decode_encode_bij" in
  Q.Test.make ~name arb_bencode prop

let check_decode_encode_token =
  let (arb_token_l : Bencode_token.t list Q.arbitrary) =
    let gen = Q.Gen.(
        small_list (oneof
            [ map (fun i -> `I (Int64.of_int i)) small_int
            ; map (fun s -> `S s) string
            ; oneofl [ `BeginDict; `BeginList; `End ] ]
        )
      )
    and shrink = Q.Shrink.list
    and print =
      Q.Print.list (fun b -> Bencode_token.Easy.to_string [b])
    in
    Q.make ~shrink ~print gen
  in
  let prop l =
    let s = Bencode_token.Easy.to_string l in
    let l' = Bencode_token.Easy.(of_string_exn s) in
    l = l'
  in
  let name = "bencode_token_decode_encode_bij" in
  Q.Test.make ~name ~count:2_000 arb_token_l prop

let props =
  [ check_decode_encode
  ; check_decode_encode_streaming
  ; check_decode_encode_token
  ]

let () =
  let _ = QCheck_runner.run_tests props in
  ()

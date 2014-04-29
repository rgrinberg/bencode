
module B = Bencode
module BS = Bencode_streaming

open QCheck

let check_decode_encode =
  let gen = Arbitrary.(
    let base = choose
      [ lift (fun i -> B.Integer i) small_int
      ; lift (fun s -> B.String s) string
      ]
    in
    fix ~max:3 ~base (fun sub ->
      choose
        [ lift B.dict_of_list (list (pair string sub))
        ; lift (fun l -> B.List l) (list sub)
        ; sub
        ]))
  in
  let prop b = B.eq (B.decode (`String (B.encode_to_string b))) b in
  let name = "bencode_decode_encode_bij" in
  mk_test ~name ~pp:BS.Encode.to_string ~size:BS.Encode.size gen prop

let check_decode_encode_streaming =
  let gen = Arbitrary.(
    let base = choose
      [ lift (fun i -> B.Integer i) small_int
      ; lift (fun s -> B.String s) string
      ]
    in
    fix ~max:3 ~base (fun sub ->
      choose
        [ lift B.dict_of_list (list (pair string sub))
        ; lift (fun l -> B.List l) (list sub)
        ; sub
        ]))
  in
  let prop b = B.eq (BS.Decode.parse_string_exn (BS.Encode.to_string b)) b in
  let name = "bencode_streaming_decode_encode_bij" in
  mk_test ~name ~pp:BS.Encode.to_string ~n:2_000 ~size:BS.Encode.size gen prop

let check_decode_encode_token =
  let (gen : Bencode_token.t list Arbitrary.t) = Arbitrary.(
    list (choose
      [ lift (fun i -> `I i) small_int
      ; lift (fun s -> `S s) string
      ; among [ `BeginDict; `BeginList; `End ] ]
    )
  ) in
  let prop l =
    let s = Bencode_token.Easy.to_string l in
    let l' = Bencode_token.Easy.(of_string_exn s) in
    l = l'
  in
  let name = "bencode_token_decode_encode_bij" in
  let pp = PP.list (fun b -> Bencode_token.Easy.to_string [b]) in
  mk_test ~name ~n:2_000 ~pp ~size:List.length gen prop

let props =
  [ check_decode_encode
  ; check_decode_encode_streaming
  ; check_decode_encode_token
  ]

let () =
  let _ = QCheck.run_tests props in
  ()

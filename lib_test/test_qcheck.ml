
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
  mk_test ~name ~pp:BS.to_string ~size:BS.size gen prop

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
  let prop b = B.eq (BS.of_string (BS.to_string b)) b in
  let name = "bencode_streaming_decode_encode_bij" in
  mk_test ~name ~pp:BS.to_string ~size:BS.size gen prop

let props =
  [ check_decode_encode
  ; check_decode_encode_streaming
  ]

let () =
  let _ = QCheck.run_tests props in
  ()

open OUnit2
module OUnit = OUnit2

module B = Bencode
module BS = Bencode_streaming

let test1 _ =
  let s = "li42ei0ei-200ee" in
  match BS.Decode.parse_string s with
  | Some b ->
    OUnit.assert_equal (B.List [B.Integer 42; B.Integer 0; B.Integer ~-200]) b
  | None ->
    OUnit.assert_failure "should parse"

let test2 _ =
  let b =
    B.dict_of_list [
      "foo", B.Integer 42;
      "bar", B.List [B.Integer 0; B.String "caramba si"];
      "", B.String "";
    ]
  in
  let s = BS.Encode.to_string b in
  (* Printf.printf "serialized to %s\n" s; *)
  let b' = BS.Decode.parse_string_exn s in
  OUnit.assert_equal ~cmp:B.eq ~printer:BS.Encode.to_string b b'

let test3 _ =
  let b = B.dict_of_list [
    "a", B.Integer 1;
    "b", B.String "bbbb";
    "l", B.List [B.Integer 0; B.Integer 0; B.String "zero\n\t \x00"];
    "d", B.dict_of_list ["foo", B.String "bar"];
  ] in
  let s = BS.Encode.to_string b in
  (* Printf.printf "serialized to %s\n" s; *)
  let b' = BS.Decode.parse_string_exn s in
  OUnit.assert_equal ~cmp:B.eq ~printer:BS.Encode.to_string b b'

let suite =
  "test_bencode" >:::
    [ "test1" >:: test1;
      "test2" >:: test2;
      "test3" >:: test3;
    ]

let () =
  let _ = OUnit.run_test_tt_main suite in
  ()

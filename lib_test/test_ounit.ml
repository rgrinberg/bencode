
open OUnit

module B = Bencode
module BS = Bencode_streaming

let test1 () =
  let s = "li42ei0ei-200ee" in
  match BS.parse_string s with
  | BS.ParseError msg ->
    OUnit.assert_failure (Printf.sprintf "should parse, got %s" msg)
  | BS.ParsePartial ->
    OUnit.assert_failure "should parse, got partial"
  | BS.ParseOk b ->
    OUnit.assert_equal (B.List [B.Integer 42; B.Integer 0; B.Integer ~-200]) b

let test2 () =
  let b =
    B.dict_of_list [
      "foo", B.Integer 42;
      "bar", B.List [B.Integer 0; B.String "caramba si"];
      "", B.String "";
    ]
  in
  let s = BS.to_string b in
  (* Printf.printf "serialized to %s\n" s; *)
  let b' = BS.of_string s in
  OUnit.assert_equal ~cmp:B.eq ~printer:BS.to_string b b'

let test3 () =
  let b = B.dict_of_list [
    "a", B.Integer 1;
    "b", B.String "bbbb";
    "l", B.List [B.Integer 0; B.Integer 0; B.String "zero\n\t \x00"];
    "d", B.dict_of_list ["foo", B.String "bar"];
  ] in
  let s = BS.to_string b in
  (* Printf.printf "serialized to %s\n" s; *)
  let b' = BS.of_string s in
  OUnit.assert_equal ~cmp:B.eq ~printer:BS.to_string b b'

let suite =
  "test_bencode" >:::
    [ "test1" >:: test1;
      "test2" >:: test2;
      "test3" >:: test3;
    ]

let () =
  let _ = OUnit.run_test_tt_main suite in
  ()

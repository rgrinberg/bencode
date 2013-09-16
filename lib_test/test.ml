let torrent_file = "test.torrent"

let () =
  let bencode = Bencode.decode (`File_path torrent_file) in
  let encoded = Bencode.encode_to_string bencode in
  Printf.printf "Encoded to: %d bytes\n" (String.length encoded);
  if bencode = (Bencode.decode (`String encoded)) then
    print_endline "Pass"
  else print_endline "Failed"

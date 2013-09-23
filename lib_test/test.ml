let torrent_file = "test.torrent"

let () =
  let bencode = Bencode.decode (`File_path torrent_file) in
  let pretty_printed = Bencode.pretty_print bencode in
  print_endline pretty_printed;
  let encoded = Bencode.encode_to_string bencode in
  Printf.printf "Encoded to: %d bytes\n" (String.length encoded);
  if bencode = (Bencode.decode (`String encoded)) then
    print_endline "Pass"
  else print_endline "Failed"

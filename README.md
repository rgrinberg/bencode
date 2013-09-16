bencode
=======

Bencode (.torrent file format) reader/writer in OCaml
```
make
make install
```

Then in the top level:
```
#require "bencode"
Bencode.decode (`File_path "test.torrent")
```

bencode [![Build Status](https://travis-ci.org/rgrinberg/bencode.svg?branch=master)](https://travis-ci.org/rgrinberg/bencode)
=======

Bencode (`.torrent` [file format](https://en.wikipedia.org/wiki/Bencode)) reader/writer in OCaml without any
external dependencies.

See `lib/bencode.mli`. Usage is straightforward. There is also a streaming
API in modules `Bencode_streaming` and `Bencode_token`.

[online documentation here](https://rgrinberg.github.io/bencode/)

## Installation
For a findlib based install
```sh
make
make install
```

## Example
In the top level:
```ocaml
#require "bencode"
Bencode.decode (`File_path "test.torrent")
```

Will return a data structure representing the bencoded form of the following
type:
```ocaml
type t =
  | Integer of int64
  | String of string
  | List of t list
  | Dict of (string * t) list
```

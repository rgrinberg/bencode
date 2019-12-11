# CHANGELOG

## 2.0

- **breaking*:: make Integer backed by int64 to support 32-bit platforms

- doc: add link to wikipedia page
- implement `pp_hum`, a human readable pretty printer
- handle empty string keys which are required for BEP-0052
- doc: add syntax coloring to readme
- move to opam 2 and dune
- adjust minimal ocaml version required to 4.02.0
- parser: fail with error msg instead of writing directly to stdout
- special fix for `min_int` (close #4)
- readme: add travis build status

## 1.0.2

- fixes: bencode_token

## 1.0.1

- fixes: META file, bencode_token

## 1.0

- license: WTFPL -> MIT
- accept some intersticial whitespace in parser (especially \n)
- parse and print sequences of Bencode values
- make the lib depend on `bytes`; now compliant with -safe-string
- updated the interface of Bencode_streaming, now using
  Bencode_token to make the parser much simpler and the API
  less coupled
- Bencode_token, for reading/printing efficiently streams of single tokens

## 0.2
- added test deps to travis
- fixed bug where empty dict/list would not be parsed
- use ounit2 in unit tests
- make target runs all tests
- non-blocking IO using Bencode_streaming
- more tests
- travis support

## 0.1

- docs
- README.md
- added test bencode file (torrent)
- unhardcoded path and fixed typo
- initial commit


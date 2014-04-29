# CHANGELOG


## master

- updated the interface of Bencode_streaming, now using
  Bencode_token to make the parser much simpler and the API
  less coupled
- Bencode_token, for reading/printing efficiently streams of single tokens

## 2.0
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


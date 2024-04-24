{
  open Lexing
  open Bencode_parse
}

rule read_fixed buf i n = parse
  | _ as c {
    Bytes.set buf i c;
    if i+1 = n then Bytes.unsafe_to_string buf
    else read_fixed buf (i+1) n lexbuf
  }
  | eof { failwith "not enough input" }

and bencode = parse
  | "0:" { STRING "" }
  | ['1'-'9'] ['0'-'9']* ':' {
    let str = lexeme lexbuf in
    let len = int_of_string (String.sub str 0 (String.length str - 1)) in
    STRING (read_fixed (Bytes.make len ' ') 0 len lexbuf)
  }
  | "i0e" { INT Int64.zero }
  | 'i' '-'? ['1'-'9'] ['0'-'9']* 'e' {
      let str = lexeme lexbuf in
      INT (Int64.of_string (
          String.sub str 1 (String.length str - 2)
        ))
    }
  | 'l' { LIST_START }
  | 'd' { DICT_START }
  | 'e' { END }
  | _ as c {
      failwith (
        Printf.sprintf "Unrecognized char: %c. Pos: %d \n" c
          (lexeme_start lexbuf))
  }
  | eof { EOF }

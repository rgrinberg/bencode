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
  | ['0'-'9']+ ':' { 
    let str = lexeme lexbuf in
    let len = int_of_string (String.sub str 0 (String.length str - 1)) in
    STRING (
      if len = 0 then ""
      else read_fixed (Bytes.make len ' ') 0 len lexbuf
      )
  }
  | 'i' '-'? ['0'-'9']+ 'e' { 
      let str = lexeme lexbuf in
      INT (int_of_string (
          String.sub str 1 (String.length str - 2)
        ))
    }
  | 'l' { LIST_START }
  | 'd' { DICT_START }
  | 'e' { END }
  | [' ' '\t' '\n'] { bencode lexbuf }  (* whitespace *)
  | _ as c {
    Printf.printf "Urecognized char: %c. Pos: %d \n" c (lexeme_start lexbuf);
    failwith "" 
  }
  | eof { EOF }

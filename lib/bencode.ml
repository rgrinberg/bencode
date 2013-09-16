external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
include Bencode_types

type src = [ 
  | `Channel of in_channel 
  | `File_path of string
  | `String of string ]

type dst = [
  | `Channel of out_channel
  | `File_path of string
  | `Buffer of Buffer.t] 

let format_list l ~f = 
  let buf = Buffer.create 10 in
  Buffer.add_string buf "[\n";
  l |> List.iter (fun e -> 
      f buf e;
      Buffer.add_char buf ';';
    );
  Buffer.add_char buf ']';
  Buffer.contents buf

let rec pretty_print = function
  | Integer x -> string_of_int x
  | String x -> Printf.sprintf "<string:%d>" (String.length x)
  | List l ->
    format_list l ~f:(fun buf e -> 
        Buffer.add_string buf (pretty_print e))
  | Dict t ->
    let format_tuple s t =
      Printf.sprintf "(\"%s\", %s)" s (pretty_print t) in
    format_list t ~f:(fun buf (s, t) ->
        Buffer.add_string buf (format_tuple s t))

module Str_conv = struct
  open Printf
  let of_int i = sprintf "i%de" i
  let of_string s = sprintf "%d:%s" (String.length s) s
  let of_list ?(c='l') s ~f = 
    let buf = Buffer.create 10 in
    Buffer.add_char buf c;
    s |> List.iter (fun x -> Buffer.add_string buf (f x));
    Buffer.add_char buf 'e';
    Buffer.contents buf
  let of_dict s ~f =
    of_list ~c:'d' (List.sort (fun x y -> compare (fst x) (fst y)) s)
      ~f:(fun (s, e) -> (of_string s) ^ (f e))
end

let rec encode_to_string t =
  let open Str_conv in
  match t with
  | Integer x -> of_int x
  | String s -> of_string s
  | List l -> of_list l ~f:encode_to_string
  | Dict d -> of_dict d ~f:encode_to_string

let decode = function
  | `Channel ch -> 
      Bencode_parse.bencode Lex.bencode (Lexing.from_channel ch)
  | `File_path path ->
      let ch = open_in path in
      let t = Bencode_parse.bencode Lex.bencode (Lexing.from_channel ch) in
      close_in ch;
      t
  | `String s -> Bencode_parse.bencode Lex.bencode (Lexing.from_string s)
  
let encode dst t =
  let encoded = encode_to_string t in
  match dst with
  | `Channel out -> output_string out encoded
  | `File_path path ->
      let ch = open_out path in
      output_string ch encoded;
      close_out ch
  | `Buffer buf -> Buffer.add_string buf encoded


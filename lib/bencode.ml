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

let rec eq t1 t2 = match t1, t2 with
  | Integer i1, Integer i2 -> i1 = i2
  | String s1, String s2 -> s1 = s2
  | List l1, List l2 ->
    (try List.for_all2 eq l1 l2 with Invalid_argument _ -> false)
  | Dict d1, Dict d2 ->
    begin try
      (* lists are sorted *)
      List.for_all2 (fun (s1,t1)(s2,t2) -> s1=s2 && eq t1 t2) d1 d2
    with Invalid_argument _ -> false
    end
  | _ -> false

let hash t = Hashtbl.hash t

let dict_of_list l =
  let l = List.sort (fun (s1,_)(s2,_) -> String.compare s1 s2) l in
  Dict l

let format_list l ~f =
  let buf = Buffer.create 10 in
  Buffer.add_string buf "[\n";
  l |> List.iter (fun e ->
      f buf e;
    );
  Buffer.add_char buf ']';
  Buffer.contents buf

let empty_string ~len = String.make len ' '

let spaces level = empty_string ~len:(level * 2)

let pretty_print =
  let rec loop level = function
    | Integer x -> Int64.to_string x
    | String x -> Printf.sprintf "<string:%d>" (String.length x)
    | List l ->
      format_list l ~f:(fun buf e ->
          Buffer.add_string buf (spaces level);
          Buffer.add_string buf (loop (succ level) e);
          Buffer.add_string buf ";\n"
        )
    | Dict t ->
      let format_tuple s t =
        Printf.sprintf "(\"%s\", %s)" s (loop (succ level) t) in
      format_list t ~f:(fun buf (s, t) ->
          Buffer.add_string buf (spaces level);
          Buffer.add_string buf (format_tuple s t);
          Buffer.add_string buf ";\n")
  in loop 1

let pp_hum (fmt:Format.formatter) (benc:t) =
  let pp_nest fmt nestlvl = (* prints a comment explaining tree context *)
    if nestlvl = [] then ()
    else Format.fprintf fmt " (* %a *)"
        Format.(pp_print_list
                  ~pp_sep:(fun fmt () -> pp_print_string fmt " -> ")
                  (fun fmt str -> pp_print_string fmt (String.escaped str))
               ) ("^--" :: List.rev nestlvl)
  in
  let rec pp_val (nestlvl:string list) fmt = function
    | Integer x -> Format.fprintf fmt "%Ld" x
    | String x when String.length x <= 40 ->
      Format.fprintf fmt "<string:%d:%S>" (String.length x) x
    | String x ->
      Format.fprintf fmt "<string:len %d>" (String.length x)
    | List (( [ String _ | Integer _ ]
            | [ String _ | Integer _ ; String _ | Integer _ ]
            ) as lst) ->
      Format.fprintf fmt "@[<v>[ @[<v>%a @]];@]"
        Format.(pp_print_list @@ pp_val nestlvl) lst
    | List lst ->
      Format.fprintf fmt "@[<v>[  @[<v>%a@]@ ] ;@]"
        Format.(pp_print_list ~pp_sep:Format.pp_print_cut @@ pp_val nestlvl) lst
    | Dict lst ->
      let last_key = ref None in
      let annotate_order key = (* alert the user if keys are invalid: *)
        let annot = match key, !last_key with
          | _, None -> ""
          | _, Some prev when key = prev -> " (* error: duplicate key *)"
          | _, Some prev when key > prev -> ""
          | _ -> " (* error: out of order, BEP-003 needs ascending key order *)"
        in last_key := Some key ; annot
      in
      Format.fprintf fmt "@[<v>{  @[<v>%a@]@ } ;@]"
        Format.(pp_print_list ~pp_sep:Format.pp_print_cut (fun fmt (key,x) ->
            let order_annotation = annotate_order key in
            begin match x with
              | ( String _ (* don't print newline before if: *)
                | List (_::_::[] | _::[] | [])
                | Integer _  ) when order_annotation = "" ->
                Format.fprintf fmt "( %S, @[<v>%a@] );" key
                  (pp_val []) x
              | _ -> Format.fprintf fmt "@[<v>( \"@[<v>%a\":%s@ %a@]@ @]) ; %a"
                       Format.(fun x -> pp_print_as x 2) (String.escaped key)
                       order_annotation
                       (pp_val ((match x with | List _ -> "[]"
                                              | _ -> "")::key::nestlvl)) x
                       pp_nest (key::nestlvl)
            end
          )) lst
  in
  (pp_val []) fmt benc

module Str_conv = struct
  open Printf
  let of_int i = sprintf "i%Lde" i
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
    Bencode_parse.bencode Bencode_lex.bencode (Lexing.from_channel ch)
  | `File_path path ->
    let ch = open_in path in
    let t = Bencode_parse.bencode Bencode_lex.bencode (Lexing.from_channel ch) in
    close_in ch;
    t
  | `String s -> Bencode_parse.bencode Bencode_lex.bencode (Lexing.from_string s)

let encode dst t =
  let encoded = encode_to_string t in
  match dst with
  | `Channel out -> output_string out encoded
  | `File_path path ->
    let ch = open_out path in
    output_string ch encoded;
    close_out ch
  | `Buffer buf -> Buffer.add_string buf encoded

type 'a sequence = ('a -> unit) -> unit

let decode_seq src k = match src with
  | `Channel ch ->
    let l = Bencode_parse.bencodes Bencode_lex.bencode (Lexing.from_channel ch)
    in List.iter k l
  | `File_path path ->
    let ch = open_in path in
    let l = Bencode_parse.bencodes Bencode_lex.bencode (Lexing.from_channel ch) in
    close_in ch;
    List.iter k l
  | `String s ->
    let l = Bencode_parse.bencodes Bencode_lex.bencode (Lexing.from_string s) in
    List.iter k l

let encode_seq dst seq =
  match dst with
  | `Channel out ->
    seq (fun d -> output_string out (encode_to_string d))
  | `File_path path ->
    let ch = open_out path in
    seq (fun d -> output_string ch (encode_to_string d));
    close_out ch
  | `Buffer buf ->
    seq (fun d -> Buffer.add_string buf (encode_to_string d))

let as_string = function
  | String s -> Some s
  | _ -> None

let as_int = function
  | Integer i -> Some i
  | _ -> None

let as_list = function
  | List l -> Some l
  | _ -> None

let as_dict = function
  | Dict l -> Some l
  | _ -> None

let dict_get b key = match b with
  | Dict l ->
      begin try
        Some (List.assoc key l)
      with Not_found -> None
      end
  | _ -> None

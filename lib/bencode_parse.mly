%{
  open Bencode_types
%}
%token <int> INT
%token <string> STRING
%token LIST_START
%token DICT_START
%token END
%token EOF

%start bencode
%type <Bencode_types.t> bencode

%%

bencodes:
  | bencode bencodes { $1::$2 }
  | bencode { [$1] }

dict:
  | STRING bencode dict { ($1, $2)::$3 }
  | STRING bencode { [($1, $2)] }

bencode:
  | INT { Integer $1 }
  | STRING { String $1 }
  | LIST_START END { List [] }
  | DICT_START END { Dict [] }
  | LIST_START bencodes END { List $2 }
  | DICT_START dict END { Dict $2 }


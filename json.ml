type token =
  | NULL
  | TRUE
  | FALSE
  | STRING of string
  | INT of int
  | FLOAT of float
  | ID of string
  | LEFT_BRACK
  | LEFT_BRACE
  | RIGHT_BRACE
  | RIGHT_BRACK
  | COMMA
  | COLON
  | EOF

type value = [
  | `Assoc of (string * value) list
  | `Bool of Bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `NULL
  | `String of string
]

open Core
open Out_channel

let rec output_value outc = function
  | `Assoc obj -> print_assoc outc obj
  | `List l    -> print_list outc l
  | `String s  -> printf "\"%s\"" s
  | `Int i     -> printf "%d" i
  | `Float x   -> printf "%f" x
  | `Bool true -> output_string outc "true"
  | `Bool false -> output_string outc "false"
  | `Null       -> output_string outc "null"

and print_assoc outc obj =
  output_string outc "{";
  let sep = ref "" in
  List.iter ~f:(fun (key, value) ->
    printf "%s\"%s\": %a" !sep key output_value value;
    sep := ",\n ") obj;
  output_string outc " }"

and print_list outc arr =
  output_string outc "[";
  List.iteri ~f:(fun i v ->
    if i > 0 then
      output_string outc ", ";
    output_value outc v) arr;
  output_string outc "]"
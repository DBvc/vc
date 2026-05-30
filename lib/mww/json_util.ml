open Resultx

type json = Yojson.Safe.t

let assoc = function `Assoc fields -> Ok fields | _ -> Error "expected JSON object"
let member name json = match json with `Assoc fields -> List.assoc_opt name fields | _ -> None

let required name json =
  match member name json with
  | Some value -> Ok value
  | None -> Error ("missing required field: " ^ name)

let required_string name json =
  let* value = required name json in
  match value with `String s -> Ok s | _ -> Error ("field " ^ name ^ " must be a string")

let optional_string ?default name json =
  match member name json with
  | None | Some `Null -> Ok default
  | Some (`String s) -> Ok (Some s)
  | Some _ -> Error ("field " ^ name ^ " must be a string or null")

let string_with_default ~default name json =
  match member name json with
  | None | Some `Null -> Ok default
  | Some (`String s) -> Ok s
  | Some _ -> Error ("field " ^ name ^ " must be a string")

let int_with_default ~default name json =
  match member name json with
  | None | Some `Null -> Ok default
  | Some (`Int i) -> Ok i
  | Some _ -> Error ("field " ^ name ^ " must be an int")

let bool_with_default ~default name json =
  match member name json with
  | None | Some `Null -> Ok default
  | Some (`Bool b) -> Ok b
  | Some _ -> Error ("field " ^ name ^ " must be a bool")

let list_with_default ~default name json parse =
  match member name json with
  | None | Some `Null -> Ok default
  | Some (`List values) ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* parsed = parse value in
            loop (parsed :: acc) rest
      in
      loop [] values
  | Some _ -> Error ("field " ^ name ^ " must be a list")

let string_list_with_default ~default name json =
  list_with_default ~default name json (function
    | `String s -> Ok s
    | _ -> Error ("field " ^ name ^ " must be a list of strings"))

let option_to_yojson f = function Some x -> f x | None -> `Null
let string_opt_to_yojson = function Some s -> `String s | None -> `Null

let parse_file path parse =
  let* contents = Fs.read_file path in
  let* json = protect ~where:("parse json " ^ path) (fun () -> Yojson.Safe.from_string contents) in
  parse json

let write_file path json = Fs.write_file path (Yojson.Safe.pretty_to_string json ^ "\n")

type 'a t = ('a, string) result

let ok x = Ok x
let error message = Error message
let ( let* ) value f = match value with Ok x -> f x | Error _ as e -> e
let ( let+ ) value f = match value with Ok x -> Ok (f x) | Error _ as e -> e
let map_error f = function Ok x -> Ok x | Error e -> Error (f e)
let of_option ~error = function Some x -> Ok x | None -> Error error

let protect ?(where = "operation") f =
  try Ok (f ()) with
  | Sys_error message -> Error (Printf.sprintf "%s failed: %s" where message)
  | Unix.Unix_error (err, fn, arg) ->
      Error
        (Printf.sprintf "%s failed: %s%s%s" where (Unix.error_message err)
           (if fn = "" then "" else " in " ^ fn)
           (if arg = "" then "" else " " ^ arg))
  | Yojson.Json_error message -> Error (Printf.sprintf "%s failed: %s" where message)

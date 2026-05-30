let replace_all ~pattern ~with_ text =
  let pattern_len = String.length pattern in
  if pattern_len = 0 then text
  else
    let text_len = String.length text in
    let buf = Buffer.create text_len in
    let rec loop i =
      if i >= text_len then Buffer.contents buf
      else if i + pattern_len <= text_len && String.sub text i pattern_len = pattern then (
        Buffer.add_string buf with_;
        loop (i + pattern_len))
      else (
        Buffer.add_char buf text.[i];
        loop (i + 1))
    in
    loop 0

let trim_trailing_slash value =
  let rec loop i =
    if i <= 0 then String.sub value 0 (i + 1)
    else if value.[i] = '/' then loop (i - 1)
    else String.sub value 0 (i + 1)
  in
  if value = "" then value else loop (String.length value - 1)

let non_empty_lines text =
  text |> String.split_on_char '\n' |> List.filter (fun line -> String.trim line <> "")

let take n xs =
  let rec loop i acc = function
    | [] -> List.rev acc
    | _ when i <= 0 -> List.rev acc
    | x :: rest -> loop (i - 1) (x :: acc) rest
  in
  loop n [] xs

let first_url text =
  let tokens =
    String.split_on_char ' ' (String.map (function '\n' | '\t' -> ' ' | c -> c) text)
  in
  let looks_like_url token =
    String.length token >= 8
    && (String.sub token 0 7 = "http://"
       || (String.length token >= 9 && String.sub token 0 8 = "https://"))
  in
  tokens |> List.map String.trim |> List.find_opt looks_like_url

let md5_file filename =
  try Ok (filename |> Digest.file |> Digest.to_hex) with Sys_error message -> Error message

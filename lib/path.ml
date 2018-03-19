type t = string list

let to_string (path:t) = sprintf "/%s"
    (String.concat ~sep:"/" path)

let to_summary ~has_subnames (path:t) =
  sprintf "Gemini %s Command%s"
    (String.concat ~sep:" " path)
    (match has_subnames with
     | true -> "s"
     | false -> ""
    )





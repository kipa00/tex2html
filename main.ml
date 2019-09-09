open Type

exception DoNotUnderstandException of string
exception MathInMathException
exception LatexException
exception BufferException

exception SuccessBreak

let main () =
  let lexbuf = Lexing.from_channel stdin in
  let e = Parser.parser_main Lexer.lexing_token lexbuf in
  (*
  let rec stringify : latex list -> string =
    let rec aux ll =  match ll with
    | LDesc s -> (
      match s with
      | "<" -> "&lt;" | ">" -> "&gt;"
      | _ -> s
    )
    | LCommand (cmd, argument) -> cmd
    | LMath _ -> raise LatexException
    in
    fun e -> String.concat "" (List.map aux e)
  in
  *)
  let rec to_string_math : latex list -> string =
    let rec aux : latex list -> string list = fun e -> match e with
    | [] -> []
    | hd::tl ->
      let res = ref [] in
      try (
        let u = (
        match hd with
          | LDesc s ->
            let flag = Char.code (String.get s 0) in
            if 48 <= flag && flag < 58 then s
            else if flag == 32 then ""
            else if s = "," then ", "
            else if s = "=" then " = "
            else if s = "<" then " &lt; "
            else if s = ">" then " &gt; "
            else if s = "\\pi" then "&pi;"
            else if s = "_" || s = "^" then (
              let tag_name = if s = "_" then "sub" else "sup" in
              match tl with
              | [] -> raise LatexException
              | hd2::tl2 ->
                let _ : unit = res := (
                  "<" ^ tag_name ^ ">" ^ (
                    match hd2 with
                    | LMath u -> to_string_math u
                    | _ -> List.nth (aux [hd2]) 0
                  ) ^ "</" ^ tag_name ^ ">"
                )::(aux tl2) in
                raise SuccessBreak
            ) else if (65 <= flag && flag < 91) || (97 <= flag && flag < 123) then (
              "<span style=\"font-style: italic;\">" ^ s ^ "</span>"
            ) else s
          | LCommand (cmd, argument) -> (
              match cmd with
              | "leq" | "le" -> " &le; "
              | "geq" | "ge" -> " &ge; "
              | "neq" -> " &ne; "
              | "," -> ","
              | "cdots" -> "&hellip;"
              | "cdot" -> "&middot;"
              | "times" -> " &times; "
              | "blacksquare" -> "&#x25A0;"
              | "texttt" -> "<code>" ^ (List.nth argument 0) ^ "</code>"
              | "sign" -> (
                  match argument with
                  | [hd2] ->
                    let cnv2 = to_string_math [LDesc hd2] in
                    let cnv3 = to_string_math [LDesc hd2] in
                    "<span style=\"display: inline-block; position: relative; vertical-align: middle; letter-spacing: 0.001em; text-align: center;\">" ^
                    "<span style=\"display: block; padding: 0.1em;\">|" ^ cnv2 ^ "|</span>" ^
                    "<span style=\"display: none; padding: 0.1em;\">/</span>" ^
                    "<span style=\"display: block; padding: 0.1em; border-top: thin solid black;\">" ^ cnv3 ^ "</span>" ^
                    "</span>"
                  | _ -> raise LatexException
                )
              | "frac" -> (
                  match argument with
                  | hd2::(hd3::[]) ->
                    let cnv2 = to_string_math [LDesc hd2] in
                    let cnv3 = to_string_math [LDesc hd3] in
                    "<span style=\"display: inline-block; position: relative; vertical-align: middle; letter-spacing: 0.001em; text-align: center;\">" ^
                    "<span style=\"display: block; padding: 0.1em;\">" ^ cnv2 ^ "</span>" ^
                    "<span style=\"display: none; padding: 0.1em;\">/</span>" ^
                    "<span style=\"display: block; padding: 0.1em; border-top: thin solid black;\">" ^ cnv3 ^ "</span>" ^
                    "</span>"
                  | _ -> raise LatexException
                )
              | "pi" -> "&pi;"
              | _ -> raise (DoNotUnderstandException cmd)
            )
          | LMath _ -> raise MathInMathException
        ) in
        let tl' = aux tl in
        if u = "" then tl' else u::tl'
      ) with SuccessBreak -> !res
    in
    fun e -> String.concat "" (aux e)
  in
  let rec to_string : latex -> string = fun e ->
    match e with
    | LDesc s ->
      if (String.get s 0) == '\n' then (
        if (String.length s) = 1 then " "
        else "</p>\n<p>"
      ) else s
    | LMath m -> to_string_math m
    | LCommand (cmd, argument) -> (
      match cmd with
      | "begin" -> (
          match argument with
          | [] -> raise LatexException
          | hd::tl -> match hd with
            | "example" -> "예제는 다른 곳에서 입력합니다."
            | "itemize" -> "<ul>\n<li>"
            | "problem" ->
              let title = List.nth tl 0 in
              "<h1>" ^ title ^ "</h1>\n<p>"
            | "quote" -> "<blockquote>"
            | _ -> raise (DoNotUnderstandException hd)
        )
      | "end" -> (
          match argument with
          | [] -> raise LatexException
          | hd::tl -> match hd with
            | "example" -> ""
            | "itemize" -> "</li>\n</ul>"
            | "problem" -> ""
            | "quote" -> "</blockquote>"
            | _ -> raise (DoNotUnderstandException hd)
        )
      | "ldots" -> "&hellip;"
      | "exmpfile" -> ""
      | "item" -> "</li>\n<li>"
      | "textbf" ->
        let u = List.nth argument 0 in
        "<strong>" ^ u ^ "</strong>"
      | "pagebreak" -> ""
      | "textit" ->
        let u = List.nth argument 0 in
        "<span style=\"font-style: italic;\">" ^ u ^ "</span>"
      | "texttt" ->
        let u = List.nth argument 0 in
        "<code>" ^ u ^ "</code>"
      (* Problem Grammar *)
      | "InputFile" -> "</p>\n<h3>입력 형식</h3>\n<p>"
      | "OutputFile" -> "</p>\n<h3>출력 형식</h3>\n<p>"
      | "Examples" -> "</p>\n<h3>예제</h3>\n<p>"
      | "Note" | "Notes" -> "</p>\n<h3>노트</h3>\n<p>"
      | _ -> raise (DoNotUnderstandException cmd)
      )
    | LComment _ -> ""
  in
  let s = (String.concat "" (List.map to_string e)) ^ "</p>" in
  let postprocess : string -> string = fun s ->
    let no_sp = Str.global_replace (Str.regexp "  ") " " s in
    let no_bli = Str.global_replace (Str.regexp "\n?<li> *</li>") "" no_sp in
    let no_bp = Str.global_replace (Str.regexp "\n?<p> *</p>") "" no_bli in
    let quote = Str.global_replace (Str.regexp "``") "\"" no_bp in
    let no_absp = Str.global_replace (Str.regexp " *\\(<p>\\|</p>\\|<li>\\|</li>\\) *") "\\1" quote in
    let nosp_btwn0 = Str.global_replace (Str.regexp "0, +0") "0,0" no_absp in
    nosp_btwn0
  in print_endline (postprocess s)

let _ : unit = main ()

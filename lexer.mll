{
  open Parser
}

rule lexing_token = parse
| '{' { BOPEN }
| '}' { BCLOSE }
| '\\' { BACKSLASH }
| '%' { PERCENT }
| '\n'+ as s { WRAP (String.length s) }
| '$' { DOLLAR }
| ['A' - 'Z' 'a' - 'z']+ as s { DESC s }
| ['0' - '9']+ as s { DESC s }
| [^ 'A' - 'Z' 'a' - 'z' '0' - '9' '{' '}' '\\' '$' '%' '\n'] as s { DESC (String.make 1 s) }
| eof { EOF }

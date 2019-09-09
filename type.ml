type latex =
| LCommand of string * string list
| LDesc of string
| LMath of latex list
| LComment of string

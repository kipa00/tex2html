%token <string> DESC
%token <int> WRAP
%token BOPEN BCLOSE BACKSLASH EOF DOLLAR PERCENT

%start parser_main
%type <Type.latex list> parser_main

%%
parser_main:
| latexes EOF { $1 }
;
latexes:
| latex { [$1] }
| latex latexes { $1::$2 }
;
latex:
| BACKSLASH DESC { LCommand ($2, []) }
| BACKSLASH DESC cmdparam { LCommand ($2, $3) }
| special_char { LDesc $1 }
| BOPEN latex BCLOSE { $2 }
| DESC { LDesc $1 }
| DOLLAR math DOLLAR { LMath $2 }
| PERCENT str WRAP { LComment (String.concat "" $2) }
| PERCENT WRAP { LComment "" }
;
math:
| math_one { [$1] }
| math_one math { $1::$2 }
;
math_one:
| BACKSLASH DESC { LCommand ($2, []) }
| BACKSLASH DESC cmdparam { LCommand ($2, $3) }
| special_char { LDesc $1 }
| DESC { LDesc $1 }
| BOPEN math BCLOSE { LMath $2 }
;
cmdparam:
| BOPEN str BCLOSE { [String.concat "" $2] }
| BOPEN str BCLOSE cmdparam { (String.concat "" $2)::$4 }
;
str:
| str_one { [$1] }
| str_one str { $1::$2 }
;
str_one:
| DESC { $1 }
| BACKSLASH DESC { $2 }
| special_char { $1 }
;
special_char:
| BACKSLASH BOPEN { "{" }
| BACKSLASH BCLOSE { "}" }
| BACKSLASH DOLLAR { "$" }
| BACKSLASH PERCENT { "%" }
| WRAP { String.make $1 '\n' }
;

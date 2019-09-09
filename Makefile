all: main

main: type.cmo parser.cmo lexer.cmo main.cmo
	ocamlc str.cma type.cmo parser.cmo lexer.cmo main.cmo -o main

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml: parser.mly
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

type.cmo: type.ml
	ocamlc -c type.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo: main.ml
	ocamlc -c main.ml

clean:
	rm -f *.cmo *.cmi parser.ml parser.mli lexer.ml main

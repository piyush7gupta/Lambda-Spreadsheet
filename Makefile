all:
	ocamllex real_lexer.mll
	ocamlyacc real_parser.mly
	ocamlc -c mainfile.ml
	ocamlc -c real_parser.mli
	ocamlc -c real_parser.ml
	ocamlc -c real_lexer.ml
	ocamlc -c yomain.ml
	ocamlc -o assignment4 str.cma mainfile.cmo real_lexer.cmo real_parser.cmo yomain.cmo 
	

all: clean
	
	# Generating the lexer!
	ocamllex Lexer.mll
	ocamlc -c Lexer.ml
	ocamlc -c main.ml
	ocamlc -o main Lexer.cmo main.cmo


clean:
	# Removes pre-existing files.
	rm Lexer.cmo -f
	rm Lexer.cmi -f
	rm main.cmo -f
	rm main.cmi -f
	rm Lexer.ml -f
	rm main -f

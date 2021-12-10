## General Description:

The Lexer.mll file contains rules to generate the tokens from the source code. 
The Lexer.mll file is compiled to generate the Lexer.ml file. The Lexer.ml contains the transition tables.
The file "main.ml" takes in the source code as input and generates the tokens using the rules present in the 
Lexer.mll file. Each token is matched with its respective token type and the output is printed to the terminal. 

At this stage of the compiler, the invalid tokens are detected which results in a compilation error. If there are no
invalid tokens, the lexing phase is successful and the tokens are passed on to the parser.

## Follow the below steps to run the Lexer (On Linux): (Open the terminal in the directory containing the required files)

1) The first step is to compile the OCamllex file, i.e Lexer.mll file. To do this run the following command on the terminal:
```
 $ ocamllex Lexer.mll
```
2) The Lexer.ml file is generated. The next step is to compile the Lexer.ml file, which generates the Ocaml Object file(.cmo file). 
```
$ ocamlc -c Lexer.ml
```
3) Compile the main.ml file
```
$ ocamlc -c main.ml
```
4) To generate the Lexical analyser run the following command:
```
$ ocamlc -o main Lexer.cmo main.cmo
```
This command generates the Lexical analyzer as an executable file named "main".

All the above instructions have been included in the Makefile. Therefore, it is enough to run the Makefile.
It is important to remove the pre-existing executables and the object files. The Makefile removes the pre-existing executables and the object files.

To run the Makefile enter the following command:
```
$ make
```
5) To run the main executable run the following command
```
$ ./main test.miche
```
In the above command, "test.miche" is the source code that is passed to the Lexical analyser. If the file name is not given an error is generated 
and the execution is terminated.  

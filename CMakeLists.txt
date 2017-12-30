CMAKE_MINIMUM_REQUIRED(VERSION 3.0)
PROJECT(CIM)


add_executable( cim
                src/main.c
                src/parser.c
                src/tokenizer.c
                src/dbuffer.c
                src/ast_printer.c)
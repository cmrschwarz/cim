#include "parser.h"
#include "ast.h"
#include <stdio.h>
#include <string.h>

int main(){
	cunit cu;
	init(&cu);
    char* input = "int y=0; heureka(a+b, 123, \"test\");";
    printf("input:\n%s\n\n", input);
    cu.str = input;
	cu.pos = input;
    parse(&cu, input);
    printf("\noutput:\n");
    print_ast(&cu);
	return 0;
}

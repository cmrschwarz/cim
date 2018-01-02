#include "parser.h"
#include "ast.h"
#include <stdio.h>
#include <string.h>

int main(){
    printf("sizeof(expr_elem): %i\n", sizeof(expr_elem));
	cunit cu;
	init(&cu);
    char* input = "x = heureka[x](test);";
    printf("input:\n%s\n\n", input);
    cu.str = input;
	cu.pos = input;
    parse(&cu, input);
    printf("sizeof(ast): %i\n", dbuffer_get_size(&cu.ast));
    printf("\noutput:\n");
    print_ast(&cu);
	return 0;
}

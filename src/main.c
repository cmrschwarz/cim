#include "parser.h"
#include "ast.h"
#include <stdio.h>
#include <string.h>

int main(){
    printf("sizeof(expr_elem): %i\n", sizeof(expr_elem));
	cunit cu;
	cunit_init(&cu);
    char* input = "x = heureka[x](1, \"test\") + y;";
    printf("input:\n%s\n\n", input);
    cu.str = input;
	cu.pos = input;
    parse(&cu, input);
    printf("sizeof(ast): %i\n", dbuffer_get_size(&cu.ast));
    printf("//TODO: change expr_elem union so we only need 8 bytes per elem\n");
    printf("\noutput:\n");
    print_ast(&cu);
    cunit_fin(&cu);
	return 0;
}

#include "parser.h"
#include "ast.h"
#include <stdio.h>
#include <string.h>

int main(){
    printf("sizeof(expr_elem): %i\n", sizeof(expr_elem));
	cunit cu;
	cunit_init(&cu);
    char* input = "x = heureka[y](1, \"test\") + z;";
    printf("input:\n%s\n\n", input);
    cu.str = input;
	cu.pos = input;
    parse(&cu, input);
    printf("sizeof(ast): %i[%i]\n",
           dbuffer_get_size(&cu.ast),
           dbuffer_get_size(&cu.ast) / sizeof(expr_elem));
    printf("\noutput:\n");
    print_ast(&cu);
    cunit_fin(&cu);
	return 0;
}

#include "parser.h"
#include "ast.h"
#include <stdio.h>
#include <string.h>

int main(){
	cunit cu;
	init(&cu);
    char* input = "int y=0; heureka![int](1+2, 3*4+1); int x = a[4 + 4];";
    printf("input:\n%s\n\n", input);
    cu.str = input;
	cu.pos = input;
    parse(&cu, input);
    printf("\noutput:\n");
    print_ast(&cu);
	return 0;
}

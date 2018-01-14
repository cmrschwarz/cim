#include "parser.h"
#include "ast.h"
#include <stdio.h>
#include <string.h>

int main(){
    printf("sizeof(expr_elem): %i\n", sizeof(expr_elem));
	cunit cu;
	cunit_init(&cu);
    char* input = "typedef vec_int vector[int, float];";
    printf("input:\n%s\n\n", input);
    for(int i=0;i<1;i++){
        parse(&cu, input);
    }
    printf("sizeof(ast): %llu[%llu]\n",
           dbuffer_get_size(&cu.ast),
           dbuffer_get_size(&cu.ast) / sizeof(expr_elem));
    printf("\noutput:\n");
    print_ast(&cu);
    cunit_fin(&cu);
	return 0;
}

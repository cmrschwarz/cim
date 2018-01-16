#include "parser.h"
#include "ast.h"
#include <stdio.h>

int main(){
    printf("sizeof(ast_node): %llu\n", sizeof(ast_node));
	cunit cu;
	cunit_init(&cu);
    char* input = "vecctor[int]() * x; typedef f scope:vector[scope:int];";
    printf("input:\n%s\n\n", input);
    for(int i=0;i<1;i++){
        parse(&cu, input);
    }
    printf("sizeof(ast): %llu[%llu]\n",
           dbuffer_get_size(&cu.ast),
           dbuffer_get_size(&cu.ast) / sizeof(ast_node));
    printf("\noutput:\n");
    print_ast(&cu);
    cunit_fin(&cu);
	return 0;
}

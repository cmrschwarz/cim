#include "parser.h"
#include "ast.h"
#include <stdio.h>

int main(){
    printf("sizeof(ast_node): %llu\n", sizeof(ast_node));
	cunit cu;
	cunit_init(&cu);

     char* input2 =
        "fn_call();"
        "fn_call_with_params(4, var);"
        "fn_call_with_params(4, nested_fn_call());"
        "random * expression + 4 * op_prec * (op - prec);"
        "generic_fn_call[int]() * x;"
        "typedef td x;"
        "typedef td scope:x;"
        "typedef td generic[x];"
        "typedef td scope:generic[x];"
        "typedef td scope:generic[scope:nested];"
        "typedef td scope:generic[nested_generic[x]];"
        "typedef td scope:generic[scoped:nested_generic[x]];";
    char* input = "random * expression + 4 * op_prec * (op - prec);";
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

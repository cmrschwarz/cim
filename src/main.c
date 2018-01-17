#include "parser.h"
#include "ast.h"
#include <stdio.h>

int main(){
    printf("sizeof(ast_node): %llu\n", sizeof(ast_node));
	cunit cu;
	cunit_init(&cu);

     char* input =
        "fn_call();"
        "fn_call_with_params(4, var);"
        "fn_call_nested(4, nested_fn_call(4, 1, nest_3()));"
       // "generic_fn_call[int](4, nested_fn_call(2), empty_generic_nest[]());"
        "random * expression + 4 * op_prec && (parenthesis - prec);"
        //"generic_fn_call[int]() * x;"
        "typedef td x;"
        "typedef sc x:td;"
        "typedef td generic[x];"
        "typedef td scope:generic[x];"
        "typedef td scope:generic[scope:nested];"
        "typedef td scope:generic[nested_generic[x]];"
        "typedef td scope:generic[scoped:nested_generic[x]];"
        "typedef td void(*)(char*) (*)(int);";
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

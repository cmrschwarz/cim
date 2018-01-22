#include "parser.h"
#include "ast.h"
#include <stdio.h>
int main(){
    printf("sizeof(ast_node): %llu\n", sizeof(ast_node));
	cunit cu;
	cunit_init(&cu);

     char* input2 =
         "int* foo(int* x, int + 2);"
         "int* foo(int(*)(char*) fn_ptr){bar();}"
        "int* foo{int x, int* y}(heureka * p){baz();};"
        "char* foo(+a, int ************b);"
        "int foo(int x){}"
        "int**[y]**[x] x;"
        "fn_call();"
        "fn_call_with_params(4, var);"
        "generic_fn_call{int}(x);"
        "fn_call_nested(4, nested_fn_call(4, 1, nest_3()));"
        "generic_fn_call{int}(4, nested_fn_call(2), empty_generic_fn{}());"
        "typedef td x;"
        "typedef sc x:td;"
        "typedef td generic{x};"
        "typedef td scope:generic{x};"
        "typedef td scope:generic{scope:nested};"
        "typedef td scope:generic{nested_generic{x}};"
        "typedef td scope:generic{scoped:nested_generic{x}};"
        "typedef td void(*)(char*) (*)(int);"
        "int[]**[]**[]*[]****[]*[x] x;"
        "ret_type ***** foo(a1* x, a2* x2, +int) + 3;";
    char* input =  "int* foo(int(*)(char*) fn_ptr){bar();}";
    printf("input:\n%s\n\n", input);
    for(int i=0;i<1;i++){
        parse(&cu, input);
    }
    printf("ast size: %llu bytes (%llu regs)\n",
           dbuffer_get_size(&cu.ast),
           dbuffer_get_size(&cu.ast) / sizeof(ast_node));
    printf("string store size: %llu bytes (%llu strings, 19 keywords)\n",
           sbuffer_get_size(&cu.data_store),
           dbuffer_get_size(&cu.string_ptrs) / sizeof(ureg) - 19);
    printf("\noutput:\n");
    print_ast(&cu);
    cunit_fin(&cu);
	return 0;
}

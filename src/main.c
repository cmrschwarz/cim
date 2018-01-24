#include "parser.h"
#include "ast.h"
#include <stdio.h>
int main(){
    printf("sizeof(ast_node): %llu\n", sizeof(ast_node));
	cunit cu;
	cunit_init(&cu);
    parse_file(&cu, "../test/dummy_src.cim");
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

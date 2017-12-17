#include "dbuffer.c"
#include "parser.c"
#include <stdio.h>
int main(){
	compilation_unit cu;
	compilation_unit_init(&cu);
	char* str = "int a = 4";
	cu.str = str;
	cu.pos = str;
	compilation_unit_consume_token(&cu);
	while(cu.current_token.type != TOKEN_TYPE_EOF){
		compilation_unit_print_token(&cu, &cu.current_token);
		printf(" (%i)", cu.current_token.str_rel);
		putchar('\n');
		compilation_unit_consume_token(&cu);
	}	
	printf("Hello gcc, my love");
	return 0;
}

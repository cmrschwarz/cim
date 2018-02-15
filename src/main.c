#include <stdio.h>

#include "parser.h"
#include "ast.h"
#include "compiler.h"
#include "hms.h"

void hms_dump(hms* h)
{
    printf("Dumping map of length %lu:\n", h->map_end - h->map);
    hms_node* n = h->map;
    while(n != h->map_end){
        if(n->key == NULL){
            printf("----\n");
        }
        else{
            printf("%s --> %s\n", n->key, n->value);
        }
        n++;
    }
}

int main(int argc, char **argv)
{
    cunit cu;
    cunit_init(&cu);
    for(int i = 1; i < argc; i++){
         parse_file(&cu, argv[i]);
    }

    if(argc == 1) {
        parse_file(&cu, "../test/dummy_src.cim");
    }

    ureg ast_size = dbuffer_get_size(&cu.ast);
    ureg regs = dbuffer_get_size(&cu.ast) / sizeof(astn);

    if(DEBUG_ENUMS){
        printf("ast size(debug_enums): %llu bytes (%llu regs)\n", ast_size, regs);
    }
    else{
        printf("ast size(release_enums): %llu bytes (%llu regs)\n", ast_size, regs);
    }
    printf("data store size: %llu byte\n", sbuffer_get_size(&cu.data_store));

    printf("\noutput:\n");
    print_ast(&cu);
    cunit_fin(&cu);


    return 0;

}

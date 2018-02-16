#include <stdio.h>

#include "parser.h"
#include "ast.h"
#include "compiler.h"
#include "hms.h"
#include "ast_printer.h"

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
    parser p;
    parser_init(&p);
    for(int i = 1; i < argc; i++){
         cim_file f;
         dbuffer_init(&f.ast);
         f.path = argv[i];
         parse_file(&p, &f);
    }

    if(argc == 1) {
        cim_file f;
        dbuffer_init(&f.ast);
        f.path = "../test/dummy_src.cim";
        parse_file(&p, &f);
        print_ast(&f);
    }

    ureg ast_size = dbuffer_get_size(p.ast);
    ureg regs = dbuffer_get_size(p.ast) / sizeof(astn);
    putchar('\n');
    if(DEBUG_ENUMS){
        printf("ast size(debug_enums): %llu bytes (%llu regs)\n", ast_size, regs);
    }
    else{
        printf("ast size(release_enums): %llu bytes (%llu regs)\n", ast_size, regs);
    }
    printf("data store size: %llu byte\n", sbuffer_get_size(&p.tk.string_store));

    parser_fin(&p);


    return 0;

}

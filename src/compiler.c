#include "compiler.h"
#include "parser.h"
#include "ast_printer.h"
void cimc_init(cimc* c, ureg thread_count){
    c->thread_count = thread_count;
    dbuffer_init_with_capacity(&c->files, sizeof(cim_file) * 4);
    ureg parser_size = sizeof(parser) * thread_count;
    dbuffer_init_with_capacity(&c->parsers, parser_size);
    parser* p = dbuffer_claim_space(&c->parsers,  parser_size);
    parser*  pe = p + thread_count;
    while(p != pe){
        parser_init(p);
        p++;
    }
}
void cimc_fin(cimc* c){

}
void cimc_add_file(cimc* c, char* path){
    cim_file* f = dbuffer_claim_small_space(&c->files, sizeof(cim_file));
    f->path = path;
    dbuffer_init_with_capacity(&f->ast, sizeof(ureg) * 1024);
}
void cimc_compile(cimc* c){
    //no MT yet, srr
    parser* p = (void*)c->parsers.start;
    cim_file* f = (void*)c->files.start;
    cim_file* fe = (void*)c->files.head;
    while(f != fe){
        parse_file(p, f);
        f++;
    }
}
void cimc_dump_asts(cimc* c){
    cim_file* f = (void*)c->files.start;
    cim_file* fe = (void*)c->files.head;
    while(f != fe){
        printf("ast dump of %s:\n", f->path);
        print_ast(f);
        f++;
    }
}
void cimc_print_stats(cimc* c){

}
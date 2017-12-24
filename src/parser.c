#include "parser.h"
#include "tokenizer.h"
#include <stdio.h>
#include <memory.h>
#include "ast.h"
#include <assert.h>
#define POS(x)(((u8*)(x)) - cu->ast.start)
#define UNEXPECTED_TOKEN() do{                                         \
    printf("%s(%d): Unexpected token", __FILE__, __LINE__); exit(-1); \
}while(0)
#define OP_RANGE 255
static u8 prec_table [OP_RANGE];
static u8 assoc_table [OP_RANGE];
enum assocs{
    RIGHT_ASSOCIATIVE = 0,
    LEFT_ASSOCIATIVE = 1,   //this cant be zero or our initialization flag won't work
};
void init(cunit* cu){
    if(prec_table[0] == 0){
        prec_table[0] = 1; //initialization flag
        prec_table['('] = 0;   // '(' uniquely has this precedence level so it is not removed during flush
        prec_table['+'] = 1;
        prec_table['-'] = 1;
        prec_table ['%'] = 2;
        prec_table['/'] = 3;
        prec_table['*'] = 3;
        prec_table[OPERATOR_DOUBLE('-')] = 99;
        prec_table[OPERATOR_DOUBLE('+')] = 99;
        prec_table[OPS_DEREF] = 99;
        prec_table[OPS_UNYRY_PLUS] = 99;
        prec_table[OPS_UNARY_MINUS] = 99;
    }
    if(assoc_table[0] == 0){
        memset(assoc_table, LEFT_ASSOCIATIVE, OP_RANGE);
    }
	dbuffer_init(&cu->string_store);
	dbuffer_init(&cu->string_ptrs);
	dbuffer_init(&cu->ast);
    dbuffer_init(&cu->shy_ops);
}

//we do the mask because a cast to an int of lower width is implementation defined for values
//outside of its range. this will be optimized away anyway


void print_indent(ureg indent){
    for(ureg i=0;i<indent; i++)fputs("    ", stdout);
}
static expr_elem* print_expr_elem(cunit* cu, expr_elem* e);
static inline expr_elem* print_expr(cunit* cu, expr_elem* expr){
    expr_elem* e = (void*) cu->ast.start + expr->val - sizeof(expr_elem);
    print_expr_elem(cu, e);
    return (void*) cu->ast.start + expr->val;
}
static expr_elem* print_expr_elem(cunit* cu, expr_elem* e){
    switch(e->type){
        case EXPR_ELEM_TYPE_NUMBER:
        case EXPR_ELEM_TYPE_VARIABLE:
            print_rel_str(cu, e->val);
            break;
        case EXPR_ELEM_TYPE_OP_LR:{
            putchar('(');
            expr_elem* r = (void*)(e-1);
            expr_elem* l;
            if(r->type == EXPR_ELEM_TYPE_NUMBER || r->type == EXPR_ELEM_TYPE_VARIABLE){
                l = r-1;
            }
            else{
                l = (void*)(cu->ast.start + r->val);
            }
            expr_elem* end_op = print_expr_elem(cu, l);
            putchar(' ');
            token t;
            t.type = TOKEN_TYPE_OPERATOR_LR;
            t.str = (ureg)e->op;
            print_token(cu, &t);
            putchar(' ');
            print_expr_elem(cu, r);
            putchar(')');
            return end_op;
        }
        case EXPR_ELEM_TYPE_OP_R:{
            putchar('(');
            expr_elem *u = (void *) (e - 1);
            print_expr_elem(cu, u);
            token t;
            t.type = TOKEN_TYPE_OPERATOR_R;
            t.str = (ureg) e->op;
            print_token(cu, &t);
            putchar(')');
            break;
        }
        case EXPR_ELEM_TYPE_OP_L:
        case EXPR_ELEM_TYPE_UNARY: {
            putchar('(');
            expr_elem *u = (void *) (e - 1);
            token t;
            t.type = (e->type == EXPR_ELEM_TYPE_UNARY)? TOKEN_TYPE_POSSIBLY_UNARY : TOKEN_TYPE_OPERATOR_L;
            t.str = (ureg) e->op;
            print_token(cu, &t);
            print_expr_elem(cu, u);
            putchar(')');
            break;
        }
        case EXPR_ELEM_TYPE_FN_CALL:{
            expr_elem* end = (void*)(cu->ast.start  + e->val);
            e--;
            print_rel_str(cu, e->val);
            e--;
            putchar('(');
            if(e!= end){
                e = print_expr_elem(cu, e);
                while(e!= end){
                    putchar(',');
                    e = print_expr_elem(cu, e);
                }
            }
            putchar(')');
            return e;
        }
        default: printf("Unknown expr type"); exit(-1);
    }
    return e-1;
}

void print_ast(cunit* cu){
    u8* astn = (void*)cu->ast.start;
    u8* end = (void*)cu->ast.head;
    ureg indent = 0;
    while(astn!=end){
        switch(*astn){
            case ASTNT_DECLARATION:{
                print_indent(indent);
                astn_declaration* d = (void*)astn;
                astn+= sizeof(*d);
                print_rel_str(cu, d->type);
                putchar(' ');
                print_rel_str(cu, d->name);
                if(d->assigning == false){
                    putchar(';');
                    putchar('\n');
                    break;
                }
                fputs(" = ", stdout);
            }break;
            case ASTNT_EXPRESSION:{
                astn = print_expr(cu, (void*)astn);
                puts(";");
            }break;
            case ASTNT_NUMBER:{
                expr_elem* n = (void*)astn;
                astn+= sizeof(*n);
                print_rel_str(cu, n->val);
                putchar(';');
                putchar('\n');
            }break;
            case ASTNT_VARIABLE:{
                expr_elem* v = (void*)astn;
                astn+= sizeof(*v);
                print_rel_str(cu, v->val);
                putchar(';');
                putchar('\n');
            }break;
            default:{
                 UNEXPECTED_TOKEN();
            }return;
        } 
    }
}
typedef struct shy_op_t{
    u8 type;
    u8 op;
}shy_op;
static void parse_meta(cunit* cu, token* t1){
    
}
static inline void flush_shy_op(cunit* cu, shy_op* s, ureg expr_start){
    expr_elem* expr_rend = (void*)(cu->ast.start + expr_start + sizeof(astn_expression) - sizeof(expr_elem));
    expr_elem* expr_rit =  (void*)(cu->ast.head - sizeof(expr_elem));
    int its;
    switch (s->type){
        case EXPR_ELEM_TYPE_BRACE: its = 0;break;
        case EXPR_ELEM_TYPE_OP_LR: its = 2;break;
        default: its = 1; break;
    }
    for(int i = 0; i != its; i++){
        if(expr_rit->type == EXPR_ELEM_TYPE_NUMBER){
            expr_rit--;
        }
        else{
            expr_rit = (void*)cu->ast.start + expr_rit->val;
        }
    }
    expr_elem* e = dbuffer_claim_small_space(&cu->ast, sizeof(expr_elem));
    e->type = s->type;
    e->op = s->op;
    e->val = (u8*)expr_rit - cu->ast.start;
    cu->shy_ops.head -= sizeof(shy_op); //this will be inlined and brought out of the loop
}
static inline void push_shy_op(cunit* cu, shy_op* sop, shy_op** sho_ri, shy_op** sho_re, ureg shy_ops_start){
    if(dbuffer_has_space(&cu->shy_ops, sizeof(shy_op))){
        *((shy_op*)cu->shy_ops.head) = *sop;
        cu->shy_ops.head += sizeof(shy_op);
        (*sho_ri)++;
    }
    else{
        dbuffer_grow(&cu->shy_ops);
        *((shy_op*)cu->shy_ops.head) = *sop;
        cu->shy_ops.head += sizeof(shy_op);
        *sho_re = (void*)(cu->shy_ops.start + shy_ops_start - sizeof(shy_op));
        *sho_ri = (void*)(cu->shy_ops.head - sizeof(shy_op));
    }
}
static int parse_expr(cunit *cu, char term1,char term2, token *t1, token *t2){
    //printf("parsing expr (%llu)\n", POS(cu->ast.head));
    if(t2->type == term1 || t2->type == term2){
        //short expression optimization
        if(t1->type == TOKEN_TYPE_NUMBER){
            expr_elem* n = dbuffer_claim_small_space(&cu->ast, sizeof(expr_elem));
            n->type = ASTNT_NUMBER;
            n->val = t1->str;
        }
		if(t1->type == TOKEN_TYPE_STRING){
            expr_elem* v = dbuffer_claim_small_space(&cu->ast, sizeof(expr_elem));
            v->type = ASTNT_VARIABLE;
            v->val = t1->str;
        }
        return (t2->type == term1) ? 0 : 1;
    }
    expr_elem* expr = dbuffer_claim_small_space(&cu->ast, sizeof(expr_elem));
    expr->type = ASTNT_EXPRESSION;
    ureg expr_start = (u8*)expr - cu->ast.start;
    ureg expr_elems_start = expr_start + sizeof(astn_expression);
    expr_elem* e;
    shy_op sop;
    bool second_available = true;
    ureg shy_ops_start = cu->shy_ops.head - cu->shy_ops.start;
    u8 last_prec = 0;
    bool expecting_op = false;
    u8 prec;
    shy_op* sho_re = (void*)(cu->shy_ops.start + shy_ops_start - sizeof(shy_op));
    shy_op* sho_ri = (void*)(cu->shy_ops.head - sizeof(shy_op));
    while(true){
        if(t1->type == term1 || t1->type == term2){
                for(;sho_ri != sho_re; sho_ri--){
                    flush_shy_op(cu, sho_ri, expr_start);
                }
                expr = (astn_expression*)(cu->ast.start + expr_start);
                expr->val = dbuffer_get_size(&cu->ast);
                return (t1->type == term1) ? 0 : 1;
        }
        switch(t1->type){
            case TOKEN_TYPE_OPERATOR_L_OR_R:{
                sop.type = (expecting_op)? EXPR_ELEM_TYPE_OP_R : EXPR_ELEM_TYPE_OP_L;
                sop.op = TO_U8(t1->str);
                prec =  prec_table[sop.op];
                if(last_prec > prec){
                    for(; sho_ri != sho_re && prec_table[sho_ri->op] > prec;sho_ri--){
                        flush_shy_op(cu, sho_ri, expr_start);
                    }
                }
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                last_prec = prec;
                break;
            }
            case TOKEN_TYPE_POSSIBLY_UNARY:{
                if(!expecting_op){ //--> unary
                    sop.type = EXPR_ELEM_TYPE_UNARY;

                    switch(TO_U8(t1->str)){
                        case '+': sop.op = OPS_UNYRY_PLUS;break;
                        case '-': sop.op = OPS_UNARY_MINUS;break;
                        case '*': sop.op = OPS_DEREF;break;
                        default: printf("Unexpected unary op"); exit(-1);
                    }
                    prec =  prec_table[sop.op];
                    if(last_prec > prec){
                        for(; sho_ri != sho_re && prec_table[sho_ri->op] > prec;sho_ri--){
                            flush_shy_op(cu, sho_ri, expr_start);
                        }
                    }
                    push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                    last_prec = prec;
                    //expecting_op stays on false
                    break;
                }
                //in this case we fall through to LR
            }
            case TOKEN_TYPE_OPERATOR_LR:{
                sop.type = EXPR_ELEM_TYPE_OP_LR;
                sop.op = TO_U8(t1->str);
                prec = prec_table[sop.op];
                if(last_prec >= prec){
                    if(assoc_table[sop.op] == LEFT_ASSOCIATIVE){
                        for(; sho_ri != sho_re && prec_table[sho_ri->op] >= prec;sho_ri--){
                            flush_shy_op(cu, sho_ri, expr_start);
                        }
                    }
                    else{
                        for(; sho_ri != sho_re && prec_table[sho_ri->op] > prec;sho_ri--){
                            flush_shy_op(cu, sho_ri, expr_start);
                        }
                    }
                }
                last_prec = prec;
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                expecting_op = false;
            }break;
            case TOKEN_TYPE_NUMBER:
                assert(!expecting_op);
                e = dbuffer_claim_small_space(&cu->ast, sizeof(*e));
                e->type = EXPR_ELEM_TYPE_NUMBER;
                e->val = t1->str;
                expecting_op = true;
                break;
            case TOKEN_TYPE_STRING:
                assert(!expecting_op);
                if(!second_available){
                    get_token(cu, t2);
                    second_available = true;
                }
                if(t2->type == '('){
                    ureg fn_name = t1->str;
                    ureg fn_end = dbuffer_get_size(&cu->ast) - sizeof(expr_elem);
                    do{
                        get_token(cu, t1);
                        get_token(cu, t2);
                    }while(parse_expr(cu, ',', ')', t1, t2) == 0);
                    e = dbuffer_claim_small_space(&cu->ast, 2*sizeof(*e));
                    e->type = EXPR_ELEM_TYPE_FN_NAME;
                    e->val = fn_name;
                    e++;
                    e->type = EXPR_ELEM_TYPE_FN_CALL;
                    e->val = fn_end;
                    second_available = false;
                }
                else{
                    e = dbuffer_claim_small_space(&cu->ast, sizeof(*e));
                    e->type = EXPR_ELEM_TYPE_VARIABLE;
                    e->val = t1->str;
                }
                expecting_op = true;
                break;
            case '(': {
                sop.type = EXPR_ELEM_TYPE_BRACE;
                sop.op = '(';
                push_shy_op(cu, &sop, &sho_ri, &sho_re, shy_ops_start);
                expecting_op = false;
                //last_prec = 0;
            }break;
            case ')':{
                for(;sho_ri != sho_re && sho_ri->op != '('; sho_ri--){
                    flush_shy_op(cu, sho_ri, expr_start);
                }
                //removing the (
                dbuffer_pop_back(&cu->shy_ops, sizeof(shy_op));
                sho_ri--;
                expecting_op = true;
            }break;
            case TOKEN_TYPE_EOF:
                printf("Unexpected eof.");exit(-1);
        }
        if(!second_available){
            get_token(cu, t1);
        }
        else {
            second_available = false;
            *t1 = *t2;
        }
    }
}
static int parse_normal_declaration(cunit* cu, token* t1, token* t2){
    astn_declaration* d =
         dbuffer_claim_small_space(&cu->ast, sizeof(astn_declaration));
    d->astnt = ASTNT_DECLARATION;
    d->type = t1->str;
    d->name = t2->str;
    get_token(cu, t1);
    if(t1->type == ';'){
        d->assigning = false;
    }
    else if(t1->type == '='){
        d->assigning = true;
        get_token(cu, t1);
        get_token(cu, t2);
        return parse_expr(cu, ';',';', t1, t2);
    }
    else{
        UNEXPECTED_TOKEN();    
    }
    return 0;
    printf("parsed declaration\n"); 
}
static int parse_next(cunit* cu){
    token t1;
    token t2;
    get_token(cu, &t1);
    switch (t1.type){
        case TOKEN_TYPE_STRING:
            get_token(cu, &t2);
            if (t2.type == TOKEN_TYPE_STRING) 
                return parse_normal_declaration(cu, &t1, &t2);
            else return parse_expr(cu, ';', ';', &t1, &t2);
        case TOKEN_TYPE_NUMBER:
            get_token(cu, &t2);
            return parse_expr(cu, ';',';', &t1, &t2);
        case '#':
        case TOKEN_TYPE_DOUBLE_HASH:
            parse_meta(cu, &t1);
            break;
        case TOKEN_TYPE_EOF:
            return -1;
        default:
            printf("parse_next: unexpected token\n");
            return -1;
    }
    return 0;
}
void parse(cunit* cu, char* str){
    cu->str = str;
    /*
    token t;
	get_token(cu, &t);
	while(t.type != TOKEN_TYPE_EOF){
		print_token(cu, &t);
		printf(" (%i)", t.rel_str);
		putchar('\n');
		get_token(cu, &t);
	}	
    */
    // actual parsing
    while(parse_next(cu)!=-1); 
}

#include "parser.h"

#include <memory.h>
#include "ast.h"
#define POS(x)(((u8*)(x)) - cu->ast.start)
static u8 prec_table [255];
//all ops not mentioned here represent themselves
enum ops{
    OPS_UNARY_PLUS='p',
    OPS_UNARY_MINUS='m',
};
void init(cunit* cu){
    if(prec_table[0] == 0){
        prec_table[0] = 1; //initialization flag
        prec_table ['%'] = 2;
        prec_table['/'] = 3;
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
static void print_expr_elem(cunit* cu, expr_elem* e){
    switch(e->type){
        case EXPR_ELEM_TYPE_NUMBER:
            print_rel_str(cu, e->val.number_str);
            break;
        case EXPR_ELEM_TYPE_OP_LR:{
            putchar('(');
            expr_elem* r = (void*)(e-1);
            expr_elem* l;
            if(r->type == EXPR_ELEM_TYPE_NUMBER){
                l = r-1;
            }
            else{
                l = (void*)(cu->ast.start + r->val.op_rend);
            }
            print_expr_elem(cu, l);
            putchar(' ');
            token t;
            t.type = TOKEN_TYPE_OPERATOR_LR;
            t.str = (ureg)e->op;
            print_token(cu, &t);
            putchar(' ');
            print_expr_elem(cu, r);
            putchar(')');
        }break;
    }
}
static inline u8* print_expr(cunit* cu, astn_expression* expr){
    expr_elem* e = (void*) cu->ast.start + expr->end - sizeof(expr_elem);
    print_expr_elem(cu, e);
    return cu->ast.start + expr->end;
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
                astn_number* n = (void*)astn;
                astn+= sizeof(*n);
                print_rel_str(cu, n->number_str);
                putchar(';');
                putchar('\n');
            }break;
            case ASTNT_VARIABLE:{
                astn_variable* v = (void*)astn;
                astn+= sizeof(*v);
                print_rel_str(cu, v->name);
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
//returns last prec
static inline u8 flush_shy_ops(cunit* cu, ureg expr_elems_start, ureg shy_ops_head){
	expr_elem* elems_rend = (void*)(cu->ast.start + expr_elems_start - sizeof(expr_elem));
	expr_elem* elems_rit =  (void*)(cu->ast.head - sizeof(expr_elem));
    if(elems_rit->type == EXPR_ELEM_TYPE_NUMBER){
        elems_rit--;
    }
    else{
        elems_rit = (void*)cu->ast.start + elems_rit->val.op_rend;
    }
    ureg last_end = (u8*)elems_rit - cu->ast.start;
    shy_op* it = (void*)(cu->shy_ops.head - sizeof(shy_op));
    //technically this can be a pointer outside of the allocated range, but 2B srsly
    shy_op* rend = (void*)(cu->shy_ops.start + shy_ops_head );
    expr_elem* e;
    while(it >= rend){
        if(it->type == EXPR_ELEM_TYPE_BRACE){
            cu->shy_ops.head = (void*)it;
            it--;
            if(it == rend)return 0;
            return prec_table[it->op];
        }
        if(dbuffer_has_space(&cu->ast, sizeof(expr_elem))){
            e = (void*)cu->ast.head;
            cu->ast.head+=sizeof(expr_elem);
        }
        else{
            ureg elems_rel_rit = (u8*)elems_rit - cu->ast.start;
            ureg elems_rel_rend = (u8*)elems_rend - cu->ast.start;
            dbuffer_grow(&cu->ast);
            e = (void*)cu->ast.head;
            cu->ast.head+=sizeof(expr_elem);
            elems_rit = (void*)(cu->ast.start + elems_rel_rit);
            elems_rend = (void*)(cu->ast.start + elems_rel_rend);
        }
        e->type = it->type;    
        e->op = it->op;
        //if op has two parameters, only one so far is LR
        if(it->type == EXPR_ELEM_TYPE_OP_LR){
            if(elems_rit->type == EXPR_ELEM_TYPE_NUMBER){
                elems_rit--;
            }
            else{
                elems_rit = (void*)cu->ast.start + elems_rit->val.op_rend;
            }
            e->val.op_rend = (u8*)elems_rit - cu->ast.start;
            last_end = e->val.op_rend;
        }
        else{
            e->val.op_rend = last_end;
        }
       // printf("%c (%llu)[-->%llu]\n", e->op,(u8*)e - cu->ast.start, e->val.op_rend);
        it--;
    }
    cu->shy_ops.head = cu->shy_ops.start + shy_ops_head;
    return 0;
}
static int parse_expr(cunit *cu, char term, token *t1, token *t2){
    //printf("parsing expr (%llu)\n", POS(cu->ast.head));
    dbuffer* sho = &cu->shy_ops;
    ureg shy_ops_head = sho->head - sho->start;
    u8 last_prec = 0;
    bool expecting_op = false;
    u8 prec;
    if(t2->type == term){
        //short expression optimization
        if(t1->type == TOKEN_TYPE_NUMBER){
            astn_number* n = dbuffer_claim_small_space(&cu->ast, sizeof(astn_number));
            n->astnt = ASTNT_NUMBER;
            n->number_str = t1->str;
        }
		if(t1->type == TOKEN_TYPE_STRING){
            astn_variable* v = dbuffer_claim_small_space(&cu->ast, sizeof(astn_variable));
            v->astnt = ASTNT_VARIABLE;
            v->name = t1->str;
        }
        return 0;
    }
    astn_expression* expr = dbuffer_claim_small_space(&cu->ast, sizeof(astn_expression));
    expr->astnt = ASTNT_EXPRESSION;
    ureg expr_start = (u8*)expr - cu->ast.start;
    ureg expr_elems_start = expr_start + sizeof(astn_expression);
    expr_elem* e;
    shy_op sop;
    bool handled_first = false;
    while(true){
        if(t1->type == term){
                flush_shy_ops(cu, expr_elems_start, shy_ops_head);
                expr = (astn_expression*)(cu->ast.start + expr_start);
                expr->end = dbuffer_get_size(&cu->ast);
                return 0;
        }
        switch(t1->type){
            case TOKEN_TYPE_OPERATOR_LR:{
                if(prec < last_prec){
                    last_prec = flush_shy_ops(cu, expr_elems_start, shy_ops_head);
                }
                else{
                    last_prec = prec;
                }
                sop.type = EXPR_ELEM_TYPE_OP_LR;
                sop.op = TO_U8(t1->str);
                prec = prec_table[sop.op];
                dbuffer_push_back_val(sho, sop);
            }break;
            case TOKEN_TYPE_NUMBER:
                e = dbuffer_claim_small_space(&cu->ast, sizeof(*e));
                e->type = EXPR_ELEM_TYPE_NUMBER;
                e->val.number_str = t1->str;
               // print_rel_str(cu, t1->str);
              //  printf(" (%i)[%i]\n", (u8*)e - cu->ast.start, cu->ast.head - cu->ast.start);
                break;
            case '(': {
                sop.type = EXPR_ELEM_TYPE_BRACE;
                dbuffer_push_back_val(sho, sop);
            }break;
            case ')':{
                last_prec = flush_shy_ops(cu, expr_elems_start, shy_ops_head);
            }break;
            case TOKEN_TYPE_EOF:
                flush_shy_ops(cu, expr_elems_start, shy_ops_head);
                expr = (astn_expression*)(cu->ast.start + expr_start);
                expr->end = dbuffer_get_size(&cu->ast);
                return -1;
        }
        if(handled_first){
            get_token(cu, t1);
        }
        else {
            handled_first = true;
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
        return parse_expr(cu, ';', t1, t2);
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
            else return parse_expr(cu, ';', &t1, &t2);
        case TOKEN_TYPE_NUMBER:
            get_token(cu, &t2);
            return parse_expr(cu, ';', &t1, &t2);
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

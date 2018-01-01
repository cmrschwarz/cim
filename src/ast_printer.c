#include "ast_printer.h"
#include "parser.h"
#include "tokenizer.h"
#include <stdio.h>
#include <string.h>

static expr_elem* print_expr_elem(cunit* cu, expr_elem* e);
void reverse_print_func_args(cunit* cu, expr_elem* elem, expr_elem* end){
    if(elem == end)return;
    expr_elem* nxt;
    if(elem->type == EXPR_ELEM_TYPE_EXPR){
        nxt = (void*)(cu->ast.start + elem->val);
    }
    else{
        nxt = elem-1;
    }
    if(nxt != end){
        reverse_print_func_args(cu, nxt, end);
        putchar(',');
        putchar(' ');
    }
    print_expr_elem(cu, elem);
}
void print_literal(cunit* cu, ureg str){
    putchar('\"');
    print_rel_str(cu, str);
    putchar('\"');
}
void print_number(cunit* cu, ureg str){
    print_rel_str(cu, str);
}
void print_binary_literal(cunit* cu, ureg str){
    putchar('\'');
    print_rel_str(cu, str);
    putchar('\'');
}
void print_op(u8 op){
	switch(op){
        case OP_ADD:putchar('+');return;
        case OP_PREINCREMENT:
        case OP_POSTINCREMENT: putchar('+');putchar('+'); return;
        case OP_ADD_ASSIGN: putchar('+');putchar('='); return;
        case OP_SUBTRACT:putchar('-'); return;
        case OP_PREDECREMENT:
        case OP_POSTDECREMENT: putchar('-');putchar('-'); return;
        case OP_SUBTRACT_ASSIGN:putchar('-');putchar('='); return;
        case OP_LOGICAL_NOT: putchar('!');return;
        case OP_NOT_EQUAL: putchar('!');putchar('='); return;
        case OP_MULTIPLY:
        case OP_DEREFERENCE: putchar('*'); return;
        case OP_MULTIPLY_ASSIGN:putchar('*');putchar('='); return;
        case OP_ASSIGN:putchar('='); return;
        case OP_EQUALS:putchar('=');putchar('='); return;
        case OP_MODULO: putchar('%'); return;
        case OP_MODULO_ASSIGN: putchar('%');putchar('='); return;
        case OP_DIVIDE: putchar('/'); return;
        case OP_DIVIDE_ASSIGN: putchar('/');putchar('='); return;
        case OP_BITWISE_AND: putchar('&'); return;
        case OP_LOGICAL_AND: putchar('&');putchar('&'); return;
        case OP_BITWISE_AND_ASSIGN: putchar('&');putchar('='); return;
        case OP_BITWISE_OR: putchar('|'); return;
        case OP_LOGICAL_OR: putchar('|');putchar('|'); return;
        case OP_BITWISE_OR_ASSIGN: putchar('|');putchar('='); return;
        case OP_BITWISE_NOT: putchar('~'); return;
        case OP_BITWISE_NOT_ASSIGN: putchar('~');putchar('='); return;
        case OP_LESS_THAN: putchar('<'); return;
        case OP_LSHIFT: putchar('<');putchar('<'); return;
        case OP_GREATER_THAN: putchar('>'); return;
        case OP_RSHIFT: putchar('>');putchar('>'); return;
        case OP_ACCESS_MEMBER: putchar('.'); return;
        case OP_DEREFERENCE_ACCESS_MEMBER: putchar('-');putchar('>'); return;
        case OP_LSHIFT_ASSIGN: putchar('<');putchar('<');putchar('=');return;
        case OP_RSHIFT_ASSIGN: putchar('>');putchar('>');putchar('=');return;
        case OP_BITWISE_XOR: putchar('^');return;
        case OP_BITWISE_XOR_ASSIGN: putchar('^');putchar('=');return;
        default:CIM_ERROR("Unknown token");
	}
}
static expr_elem* print_expr_elem(cunit* cu, expr_elem* e){
    switch(e->type){
        case EXPR_ELEM_TYPE_NUMBER:
        case EXPR_ELEM_TYPE_VARIABLE:
            print_rel_str(cu, e->val);
            break;
        case EXPR_ELEM_TYPE_LITERAL:
            print_literal(cu, e->val);
            break;
        case EXPR_ELEM_TYPE_BINARY_LITERAL:
            print_binary_literal(cu, e->val);
            break;
        case EXPR_ELEM_TYPE_OP_LR:{
            putchar('(');
            expr_elem* r = (void*)(e-1);
            expr_elem* l;
            if( r->type == EXPR_ELEM_TYPE_NUMBER ||
                r->type == EXPR_ELEM_TYPE_VARIABLE ||
                r->type == EXPR_ELEM_TYPE_LITERAL ||
                r->type == EXPR_ELEM_TYPE_BINARY_LITERAL)
            {
                l = r-1;
            }
            else{
                l = (void*)(cu->ast.start + r->val);
            }
            expr_elem* end_op = print_expr_elem(cu, l);
            putchar(' ');
            print_op(e->op);
            putchar(' ');
            print_expr_elem(cu, r);
            putchar(')');
            return end_op;
        }
        case EXPR_ELEM_TYPE_EXPR:{
            return print_expr_elem(cu, e-1);
        }
        case EXPR_ELEM_TYPE_OP_R:{
            putchar('(');
            expr_elem *u = (void *) (e - 1);
            expr_elem* end_op = print_expr_elem(cu, u);
            print_op(e->op);
            putchar(')');
            return end_op;
        }
        case EXPR_ELEM_TYPE_OP_L:
        case EXPR_ELEM_TYPE_UNARY: {
            putchar('(');
            expr_elem *u = (void *) (e - 1);
            print_op(e->op);
            expr_elem* end_op = print_expr_elem(cu, u);
            putchar(')');
            return end_op;
        }
        case EXPR_ELEM_TYPE_FN_CALL:{
            expr_elem* end = (void*)(cu->ast.start  + e->val);
            e--;
            print_rel_str(cu, e->val);
            e--;
            putchar('(');
            reverse_print_func_args(cu, e, end);
            putchar(')');
            return end;
        }
        case EXPR_ELEM_TYPE_GENERIC_FN_CALL:{
            expr_elem* end = (void*)(cu->ast.start  + e->val);
            e--;
            print_rel_str(cu, e->val);
            e--;
            expr_elem* args_end = (void*)(cu->ast.start  + e->val);
            e--;
            putchar('!');putchar('[');
            reverse_print_func_args(cu, args_end, end);
            putchar(']');putchar('(');
            reverse_print_func_args(cu, e, args_end);
            putchar(')');
            return end;
        }
        case EXPR_ELEM_TYPE_ARRAY_ACCESS:{
            print_rel_str(cu, e->val);
            e--;
            putchar('[');
            e = print_expr_elem(cu, e);
            putchar(']');
            return e;
        }
        default:CIM_ERROR("Unknown expression type");
    }
    return e-1;
}
static inline expr_elem* print_expr(cunit* cu, expr_elem* expr){
    expr_elem* e = (void*) cu->ast.start + expr->val - sizeof(expr_elem);
    print_expr_elem(cu, e);
    return (void*) (cu->ast.start + expr->val);
}

void print_indent(ureg indent){
    for(ureg i=0;i<indent; i++)fputs("    ", stdout);
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
                astn = (void*)print_expr(cu, (void*)astn);
                puts(";");
            }break;
            case ASTNT_VARIABLE:
            case ASTNT_NUMBER:{
                expr_elem* n = (void*)astn;
                astn+= sizeof(*n);
                print_rel_str(cu, n->val);
                putchar(';');
                putchar('\n');
            }break;
            case ASTNT_LITERAL:{
                expr_elem* e = (void*)astn;
                print_literal(cu, e->val);
                putchar(';');
                putchar('\n');
            }
            case ASTNT_BINARY_LITERAL:{
                expr_elem* e = (void*)astn;
                print_binary_literal(cu, e->val);
                putchar(';');
                putchar('\n');
            }
            default:CIM_ERROR("Unexpected ASTN");
        }
    }
}
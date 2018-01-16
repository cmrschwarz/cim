#include "ast_printer.h"
#include <stdio.h>
#include "error.h"
#include "ast.h"

static ast_node* print_ast_node(cunit* cu, ast_node* e);
void reverse_print_func_args(cunit* cu, ast_node* elem, ast_node* end){
    if(elem == end)return;
    ast_node* nxt = elem - elem->expr.size;
    if(nxt != end){
        reverse_print_func_args(cu, nxt, end);
        putchar(',');
        putchar(' ');
    }
    print_ast_node(cu, elem);
}
void print_literal(cunit* cu, char* str){
    putchar('\"');
    printf(str);
    putchar('\"');
}
void print_number(cunit* cu, char* str){
    printf(str);
}
void print_binary_literal(cunit* cu, char* str){
    putchar('\'');
    printf(str);
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
static ast_node* print_ast_node(cunit* cu, ast_node* e){
    switch(e->expr.type){
        case EXPR_NODE_TYPE_NUMBER:
        case EXPR_NODE_TYPE_VARIABLE:
            printf((e-1)->str);
            break;
        case EXPR_NODE_TYPE_LITERAL:
            print_literal(cu, (e-1)->str);
            break;
        case EXPR_NODE_TYPE_BINARY_LITERAL:
            print_binary_literal(cu, (e-1)->str);
            break;
        case EXPR_NODE_TYPE_OP_LR:{
            putchar('(');
            ast_node* r = (void*)(e-1);
            ast_node* l;
            if( r->expr.type == EXPR_NODE_TYPE_NUMBER ||
                r->expr.type == EXPR_NODE_TYPE_VARIABLE ||
                r->expr.type == EXPR_NODE_TYPE_LITERAL ||
                r->expr.type == EXPR_NODE_TYPE_BINARY_LITERAL)
            {
                l = r-2;
            }
            else {
                l= r - r->expr.size;
            }

            ast_node* end_op = print_ast_node(cu, l);
            putchar(' ');
            print_op(e->op.opcode);
            putchar(' ');
            print_ast_node(cu, r);
            putchar(')');
            return end_op;
        }
        case EXPR_NODE_TYPE_EXPR:{
            return print_ast_node(cu, e-1);
        }
        case EXPR_NODE_TYPE_OP_R:{
            putchar('(');
            ast_node *u = e - 1;
            ast_node* end_op = print_ast_node(cu, u);
            print_op(e->op.opcode);
            putchar(')');
            return end_op;
        }
        case EXPR_NODE_TYPE_OP_L:
        case EXPR_NODE_TYPE_UNARY: {
            putchar('(');
            ast_node *u = e - 1;
            print_op(e->op.opcode);
            ast_node* end_op = print_ast_node(cu, u);
            putchar(')');
            return end_op;
        }
        case EXPR_NODE_TYPE_FN_CALL:{
            ast_node* end = e - e->expr.size;
            e--;
            printf(e->str);
            e--;
            putchar('(');
            reverse_print_func_args(cu, e, end);
            putchar(')');
            return end;
        }
        case EXPR_NODE_TYPE_GENERIC_FN_CALL:{
            ast_node* end = e - e->expr.size;
            e--;
            printf(e->str);
            e--;
            ast_node* generic_args_rstart = e - e->expr.size - 1;
            e--;
            putchar('[');
            reverse_print_func_args(cu, generic_args_rstart, end);
            putchar(']');putchar('(');
            reverse_print_func_args(cu, e, generic_args_rstart);
            putchar(')');
            return end;
        }
        case EXPR_NODE_TYPE_ARRAY_ACCESS:{
            e--;
            printf(e->str);
            e--;
            putchar('[');
            e = print_ast_node(cu, e);
            putchar(']');
            return e;
        }
        default:CIM_ERROR("Unknown expression type");
    }
    return e-2;
}
static inline ast_node* print_expr(cunit* cu, ast_node* expr){
    ast_node* e = expr + expr->expr.size - 1;
    print_ast_node(cu, e);
    return e+1;
}

void print_indent(ureg indent){
    for(ureg i=0;i<indent; i++)fputs("    ", stdout);
}
void print_ptrs(u8 ptrs){
    for(u8 i = 0;i!=ptrs;i++){
        putchar('*');
    }
}
ast_node* print_type(cunit* cu, ast_node* t);
void reverse_print_type_list(cunit* cu, ast_node* start, ast_node* end){
    ast_node* next = start - start->type.size ;
    if(next != end){
        reverse_print_type_list(cu, next, end);
        putchar(',');putchar(' ');
    }
    print_type(cu, start);
}
ast_node* print_type(cunit* cu, ast_node* t){
    if(t->type.type == AST_TYPE_TYPE_SIMPLE){
        fputs((t-1)->str, stdout);
        print_ptrs(t->type.ptrs);
        return t + 1;
    }
    ast_node* last = t - t->type.size  + 1;
    ast_node* tn = t-1;
    switch (t->type.type){
        case AST_TYPE_TYPE_SCOPED:{
            for(ast_node* i = last ; i != tn; i++){
                fputs(i->str, stdout);
                putchar(':');
            }
            fputs(tn->str, stdout);
        }break;
        case AST_TYPE_TYPE_GENERIC_STRUCT:{
            fputs(tn->str, stdout);
            putchar('[');
            reverse_print_type_list(cu, t - 2, last-1);
            putchar(']');
        }break;
        case AST_TYPE_TYPE_SCOPED_GENERIC_STRUCT:{
            ast_node* scopes_end = last + tn->type.size ;
            for(ast_node* i = last; i!= scopes_end; i++){
                fputs(i->str, stdout);
                putchar(':');
            }
            fputs((t-2)->str, stdout);
            putchar('[');
            reverse_print_type_list(cu, t - 3, scopes_end - 1);
            putchar(']');
        }break;
        case AST_TYPE_TYPE_FN_PTR:{
            ast_node* ret = last + tn->type.size -1;
            putchar('(');
            print_type(cu, ret);
             putchar(' ');putchar('(');
            reverse_print_type_list(cu, (t-2), ret);
            putchar(')');
            putchar(')');
        }break;
        default:CIM_ERROR("Unknown AST_TYPE_TYPE");
    }
    print_ptrs(t->type.ptrs);
    return t + 1;
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
                printf(d->type);
                putchar(' ');
                printf(d->name);
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
                ast_node* n = (void*)astn;
                astn+= sizeof(*n) * 2;
                printf((n+1)->str);
                putchar(';');
                putchar('\n');
            }break;
            case ASTNT_LITERAL:{
                ast_node* e = (void*)astn;
                print_literal(cu, (e+1)->str);
                putchar(';');
                putchar('\n');
            }break;
            case ASTNT_BINARY_LITERAL:{
                ast_node* e = (void*)astn;
                print_binary_literal(cu, (e+1)->str);
                putchar(';');
                putchar('\n');
            }break;
            case ASTNT_TYPEDEF:{
                astn_typedef* t = (void*)astn;
                printf("typedef %s ", t->tgt_type.str);
                ast_node* tn = (ast_node*)(t+1) + t->size;
                astn = (void*)print_type(cu, tn);
                putchar(';');
                putchar('\n');
            }break;
            default:CIM_ERROR("Unexpected ASTN");
        }
    }
}
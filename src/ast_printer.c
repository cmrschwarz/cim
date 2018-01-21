#include "ast_printer.h"
#include <stdio.h>
#include "error.h"
#include "ast.h"
void reverse_print_type_list(cunit* cu, ast_node* start, ast_node* end);
static ast_node* print_expr_node(cunit *cu, ast_node *e);

void reverse_print_func_args(cunit* cu, ast_node* elem, ast_node* end){
    if(elem == end)return;
    ast_node* nxt = elem - elem->sub_expr.size;
    if(nxt != end){
        reverse_print_func_args(cu, nxt, end);
        putchar(',');
        putchar(' ');
    }
    print_expr_node(cu, elem);
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
        case OP_UNARY_PLUS:
        case OP_ADD:putchar('+');return;
        case OP_PREINCREMENT:
        case OP_POSTINCREMENT: putchar('+');putchar('+'); return;
        case OP_ADD_ASSIGN: putchar('+');putchar('='); return;
        case OP_UNARY_MINUS:
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
static ast_node* print_expr_node(cunit *cu, ast_node *e){
    switch(e->sub_expr.type){
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
            if( r->sub_expr.type == EXPR_NODE_TYPE_NUMBER ||
                r->sub_expr.type == EXPR_NODE_TYPE_VARIABLE ||
                r->sub_expr.type == EXPR_NODE_TYPE_LITERAL ||
                r->sub_expr.type == EXPR_NODE_TYPE_BINARY_LITERAL)
            {
                l = r-2;
            }
            else {
                l= r - r->sub_expr.size;
            }

            ast_node* end_op = print_expr_node(cu, l);
            putchar(' ');
            print_op(e->op.opcode);
            putchar(' ');
            print_expr_node(cu, r);
            putchar(')');
            return end_op;
        }
        case EXPR_NODE_TYPE_EXPR:{
            if(e->sub_expr.size != 1)return print_expr_node(cu, e - 1);
            return e-1;

        }
        case EXPR_NODE_TYPE_OP_R:{
            putchar('(');
            ast_node *u = e - 1;
            ast_node* end_op = print_expr_node(cu, u);
            print_op(e->op.opcode);
            putchar(')');
            return end_op;
        }
        case EXPR_NODE_TYPE_OP_L:
        case EXPR_NODE_TYPE_UNARY: {
            putchar('(');
            ast_node *u = e - 1;
            print_op(e->op.opcode);
            ast_node* end_op = print_expr_node(cu, u);
            putchar(')');
            return end_op;
        }
        case EXPR_NODE_TYPE_FN_CALL:{
            ast_node* end = e - e->sub_expr.size;
            e--;
            printf(e->str);
            e--;
            putchar('(');
            reverse_print_func_args(cu, e, end);
            putchar(')');
            return end;
        }
        case EXPR_NODE_TYPE_GENERIC_FN_CALL:{
            ast_node* end = e - e->sub_expr.size;
            e--;
            printf(e->str);
            e--;
            ast_node* generic_args_rstart = e - e->sub_expr.size;
            e--;
            putchar('{');
            reverse_print_type_list(cu, generic_args_rstart, end);
            putchar('}');putchar('(');
            reverse_print_func_args(cu, e, generic_args_rstart);
            putchar(')');
            return end;
        }
        case EXPR_NODE_TYPE_ARRAY_ACCESS:{
            e--;
            printf(e->str);
            e--;
            putchar('[');
            e = print_expr_node(cu, e);
            putchar(']');
            return e;
        }
        default:CIM_ERROR("Unknown expression type");
    }
    return e-2;
}
static inline ast_node* print_expr(cunit* cu, ast_node* expr){
    ast_node* e = expr + expr->sub_expr.size - 1;
    if(e != expr) print_expr_node(cu, e);
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
    if(start == end)return;
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
            print_ptrs(t->type.ptrs);
        }break;
        case AST_TYPE_TYPE_GENERIC_STRUCT:{
            fputs(tn->str, stdout);
            putchar('{');
            reverse_print_type_list(cu, t - 2, last-1);
            putchar('}');
            print_ptrs(t->type.ptrs);
        }break;
        case AST_TYPE_TYPE_SCOPED_GENERIC_STRUCT:{
            ast_node* gen_args_rstart = t - 3;
            ast_node* scopes_end = gen_args_rstart - tn->type.size;
            for(ast_node* i = last; i!= scopes_end; i++){
                fputs(i->str, stdout);
                putchar(':');
            }
            fputs((t-2)->str, stdout);
            putchar('{');
            reverse_print_type_list(cu, gen_args_rstart, scopes_end);
            putchar('}');
            print_ptrs(t->type.ptrs);
        }break;
        case AST_TYPE_TYPE_FN_PTR:{
            ast_node* args_start = (t-2);
            ast_node* ret = args_start - tn->type.size + 1;
            print_type(cu, ret);
            putchar(' ');
            putchar('(');
            print_ptrs(t->type.ptrs);
            putchar(')');
            putchar('(');
            reverse_print_type_list(cu, args_start, ret);
            putchar(')');
        }break;
        case AST_TYPE_TYPE_ARRAY:{
            ast_node* expr = t-1;
            print_type(cu, expr - expr->sub_expr.size);
            putchar('[');
            if(expr->sub_expr.size != 1)print_expr_node(cu, expr-1);
            putchar(']');
            print_ptrs(t->type.ptrs);
        }break;
        default:CIM_ERROR("Unknown AST_TYPE_TYPE");
    }
    return t + 1;
}
void reverse_print_func_params(cunit* cu, ast_node* elem, ast_node* end){
    if(elem == end)return;
    elem-=1;
    ast_node* nxt = elem - elem->type.size;
    if(nxt != end){
        reverse_print_func_params(cu, nxt, end);
        putchar(',');
        putchar(' ');
    }
    print_type(cu, elem);
    putchar(' ');
    printf((elem+1)->str);
}
void print_ast_within(cunit* cu, ureg indent, ast_node* astn, ast_node* end){
    while(astn!=end){
        print_indent(indent);
        switch(astn->ast_expr.type){
            case ASTNT_VARIABLE_DECLARATION:{
                ast_node* decl = (void*)astn;
                ast_node* name =  decl + decl->ast_expr.size -1;
                astn = name + 1;
                print_type(cu, name-1);
                putchar(' ');
                printf(name->str);
                putchar(';');putchar('\n');
            }break;
            case ASTNT_FUNCTION_DECLARATION:{
                ast_node* decl = (void*)astn;
                ast_node* fn_name =  decl + decl->ast_expr.size -1;
                ast_node* params_size = fn_name-1;
                ast_node* ret_type = params_size - params_size->ast_expr.size;
                print_type(cu, ret_type);
                putchar(' ');
                printf(fn_name->str);
                putchar('(');
                reverse_print_func_params(cu, params_size - 1, ret_type);
                putchar(')');putchar('{');putchar('\n');
                ast_node* block = fn_name+1;
                void* block_end = (u8*)block + block->size;
                print_ast_within(cu, indent + 1, block + 1, block_end);
                putchar('}');putchar('\n');
                astn = block_end;
            }break;
            case ASTNT_GENERIC_FUNCTION_DECLARATION:{
                ast_node* decl = (void*)astn;
                ast_node* fn_name =  decl + decl->ast_expr.size -1;
                ast_node* params_size = fn_name-1;
                ast_node* generic_params_size = params_size - params_size->ast_expr.size;
                ast_node* ret_type = generic_params_size - generic_params_size->ast_expr.size;
                print_type(cu, ret_type);
                putchar(' ');
                printf(fn_name->str);
                putchar('{');
                reverse_print_func_params(cu, generic_params_size - 1, ret_type);
                putchar('}');
                putchar('(');
                reverse_print_func_params(cu, params_size - 1, generic_params_size);
                putchar(')');putchar('{');putchar('\n');
                ast_node* block = fn_name+1;
                void* block_end = (u8*)block + block->size;
                print_ast_within(cu, indent + 1, block + 1, block_end);
                putchar('}');putchar('\n');
                astn = block_end;
            }break;
            case ASTNT_EXPRESSION:{
                astn = (void*)print_expr(cu, (void*)astn);
                puts(";");
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
void print_ast(cunit* cu){
    print_ast_within(cu, 0, (void*)cu->ast.start, (void*)cu->ast.head);
}
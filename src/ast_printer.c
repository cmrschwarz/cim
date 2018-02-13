#include "ast_printer.h"
#include <stdio.h>
#include "error.h"
#include "ast.h"
static void print_type(astn* t);
static void print_sub_expr(astn *e, bool allow_types);
void write(const char* str){
    fputs(str, stdout);
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
        case OP_ADDRESS_OF:
        case OP_BITWISE_AND: putchar('&'); return;
        case OP_LOGICAL_AND: putchar('&');putchar('&'); return;
        case OP_BITWISE_AND_ASSIGN: putchar('&');putchar('='); return;
        case OP_BITWISE_OR: putchar('|'); return;
        case OP_LOGICAL_OR: putchar('|');putchar('|'); return;
        case OP_BITWISE_OR_ASSIGN: putchar('|');putchar('='); return;
        case OP_BITWISE_NOT: putchar('~'); return;
        case OP_BITWISE_NOT_ASSIGN: putchar('~');putchar('='); return;
        case OP_LESS_THAN: putchar('<'); return;
        case OP_LESS_THAN_OR_EQUAL: putchar('<');putchar('='); return;
        case OP_LSHIFT: putchar('<');putchar('<'); return;
        case OP_GREATER_THAN: putchar('>'); return;
        case OP_GREATER_THAN_OR_EQUAL: putchar('>');putchar('='); return;
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
void print_indent(ureg indent){
    for(ureg i=0;i<indent; i++)fputs("    ", stdout);
}
void print_ptrs(ureg ptrs){
    for(ureg i = 0; i != ptrs; i++){
        putchar('*');
    }
}
void reverse_print_func_args(astn* elem, astn* end){
    if(elem == end)return;
    astn* nxt = elem - elem->expr.size;
    if(nxt != end){
        reverse_print_func_args(nxt, end);
        putchar(',');
        putchar(' ');
    }
    print_sub_expr(elem, false);
}
void reverse_print_generic_arg_list(astn* start, astn* end){
    if(start == end)return;
    astn* next = start - start->type.size ;
    if(next != end){
        reverse_print_generic_arg_list(next, end);
        putchar(',');putchar(' ');
    }
    print_sub_expr(start, true);
}
void reverse_print_generic_param_list(astn* start, astn* end){
    if(start == end)return;
    astn* next = start - start->type.size ;
    if(next != end){
        reverse_print_generic_param_list(next, end);
        putchar(',');putchar(' ');
    }
    print_type(start - 2);
    putchar(' ');
    write((start-1)->str);
}
void reverse_print_type_list(astn* start, astn* end){
    if(start == end)return;
    astn* next = start - start->type.size ;
    if(next != end){
        reverse_print_type_list(next, end);
        putchar(',');putchar(' ');
    }
    print_type(start);
}

void reverse_print_func_params(astn* elem, astn* end){
    if(elem == end)return;
    astn* nxt = elem - elem->type.size;
    if(nxt != end){
        reverse_print_func_params(nxt, end);
        putchar(',');putchar(' ');
    }
    print_type(elem-2);
    putchar(' ');
    write((elem-1)->str);
}
static void print_type(astn* t){
    bool cnst = false;
    if((t->type.mods & MOD_CONST) != 0){
        write("const(");
        cnst = true;
    }
    astn* tn = t-1;
    switch (t->type.type){
        case EXPR_NODE_TYPE_SIMPLE:{
            write((t-1)->str);
        }break;
        case EXPR_NODE_TYPE_PTR:{
            print_type(tn);
            putchar('*');
        }break;
        case EXPR_NODE_TYPE_SCOPED:{
            print_type(t - 2);
            putchar(':');
            write(tn->str);
        }break;
        case EXPR_NODE_TYPE_GENERIC_STRUCT_INST:{
            print_type(tn - tn->type.size);
            putchar('{');
            reverse_print_generic_arg_list(t - 2,  tn - tn->type.size);
            putchar('}');
        }break;
        case EXPR_NODE_TYPE_GENERIC_STRUCT_DECL:{
        case EXPR_NODE_TYPE_GENERIC_STRUCT_AMBIGUOUS:
            print_type(tn - tn->type.size);
            putchar('{');
            reverse_print_generic_param_list(t - 2, tn - tn->type.size);
            putchar('}');
        }break;
        case EXPR_NODE_TYPE_FN_PTR:{
            astn* args_start = (t-2);
            astn* ret = args_start - tn->type.size + 1;
            print_type(ret);
            write(token_strings[TOKEN_LEFT_ARROW]);
            putchar('(');
            reverse_print_type_list(args_start, ret);
            putchar(')');
        }break;
        case EXPR_NODE_TYPE_ARRAY:{
            astn* expr = t-1;
            print_type(expr - expr->expr.size);
            putchar('[');
            if(expr->expr.size != 1)print_sub_expr(expr, false);
            putchar(']');
        }break;
        default:CIM_ERROR("Unknown AST_TYPE_TYPE");
    }
    if(cnst)putchar(')');
}
static void print_sub_expr(astn *e, bool allow_types){
    astn* e2 = e-1;
    switch(e->expr.type){
        case EXPR_NODE_TYPE_SCOPED:{
            print_type(e - 2);
            putchar(':');
            write(e2->str);
        }return;
        case EXPR_NODE_NUMBER:
        case EXPR_NODE_VARIABLE:{
            write(e2->str);
        } return;
        case EXPR_NODE_LITERAL:{
            putchar('\"');
            write(e2->str);
            putchar('\"');
        } return;
        case EXPR_NODE_BINARY_LITERAL: {
            putchar('\'');
            write(e2->str);
            putchar('\'');
        }return;
        case EXPR_NODE_OP_L:{
            putchar('(');
            print_op(e->expr.special.opcode);
            print_sub_expr(e2, false);
            putchar(')');
        }return;
        case EXPR_NODE_OP_R:{
            putchar('(');
            print_sub_expr(e2, false);
            print_op(e->expr.special.opcode);
            putchar(')');
        }return;
        case EXPR_NODE_OP_LR:{
            putchar('(');
            astn* r = e2;
            astn* l;
            l= r - r->expr.size;
            print_sub_expr(l, false);
            putchar(' ');
            print_op(e->expr.special.opcode);
            putchar(' ');
            print_sub_expr(r, false);
            putchar(')');
        }return;
        case EXPR_NODE_CAST:{
            putchar('(');
            astn* r = e2;
            astn* l;
            l= r - r->expr.size;
            putchar('(');
            print_type(l);
            putchar(')');
            print_sub_expr(r, false);
            putchar(')');
        }return;
        case EXPR_NODE_FN_CALL:{
            astn* name = e2 - e2->expr.size;
            print_type(name);
            putchar('(');
            reverse_print_func_args(e-2, name);
            putchar(')');
        }return;
        case EXPR_NODE_GENERIC_FN_CALL:{
            astn* params_size = e-1;
            astn* generic_args_size = params_size - params_size->expr.size;
            astn* name = generic_args_size - generic_args_size->expr.size;
            print_type(name);
            putchar('{');
            reverse_print_generic_arg_list(generic_args_size - 1, name);
            putchar('}');putchar('(');
            reverse_print_func_args(params_size-1, generic_args_size);
            putchar(')');
        }return;
        case EXPR_NODE_ARRAY_ACCESS:{
            print_sub_expr(e2 - e2->expr.size, false);
            putchar('[');
            print_sub_expr(e2, false);
            putchar(']');
        }return;
        default:{
            if(allow_types){
                print_type(e);
            }
            else{
                 CIM_ERROR("Unknown expression type");
            }
        }
    }
}
astn* print_block(cunit* cu, ureg indent, astn* blk);
void print_ast_within(cunit* cu, ureg indent, astn* curr, astn* end, bool trailing_nl){
    while(curr!=end){
        print_indent(indent);
        switch(curr->common.type){
            case ASTNT_VARIABLE_DECLARATION:
            case ASTNT_VARIABLE_DECLARATION_AMBIGUOUS:{
                astn* name =  curr + curr->common.size -1;
                curr = name + 1;
                print_type(name - name->type.size);
                putchar(' ');
                print_type(name);
                putchar(';');
                if(trailing_nl)putchar('\n');
            }break;
            case ASTNT_ASSIGNING_VARIABLE_DECLARATION:
            case ASTNT_ASSIGNING_VARIABLE_DECLARATION_AMBIGUOUS: {
                astn* expr =  curr + curr->common.size -1;
                curr = expr + 1;
                astn* name = expr - expr->expr.size;
                print_type(name - name->type.size);
                putchar(' ');
                print_type(name);
                putchar(' ');putchar('=');putchar(' ');
                print_sub_expr(expr, false);
                putchar(';');
                if(trailing_nl)putchar('\n');
            }break;
            case ASTNT_FUNCTION_DECLARATION:{
                astn* decl = (void*)curr;
                astn* params_size =  decl + decl->common.size - 1;
                astn* fn_name = params_size - params_size->common.size;
                astn* ret_type = fn_name - fn_name->type.size;
                print_type(ret_type);
                putchar(' ');
                print_type(fn_name);
                putchar('(');
                reverse_print_func_params(params_size - 1, fn_name);
                putchar(')');putchar('{');putchar('\n');
                curr = print_block(cu, indent + 1, params_size + 1);
                print_indent(indent);putchar('}');
                if(trailing_nl)putchar('\n');
            }break;
            case ASTNT_GENERIC_FUNCTION_DECLARATION:{
                astn* decl = (void*)curr;
                astn* params_size =  decl + decl->common.size - 1;
                astn* generic_params_size = params_size - params_size->common.size;
                astn* fn_name = generic_params_size - generic_params_size->common.size;
                print_type(fn_name - fn_name->common.size);
                putchar(' ');
                print_type(fn_name);
                putchar('{');
                reverse_print_func_params(generic_params_size - 1, fn_name);
                putchar('}');
                putchar('(');
                reverse_print_func_params(params_size - 1, generic_params_size);
                putchar(')');putchar('{');putchar('\n');
                curr = print_block(cu, indent + 1, params_size + 1);
                print_indent(indent);putchar('}');putchar('\n');
            }break;
            case ASTNT_EXPRESSION:{
                astn* n = curr + curr->expr.size - 1;
                if(n != curr) print_sub_expr(n, false);
                curr = n + 1;
                puts(";");
            }break;
            case ASTNT_TYPEDEF:{
                write("typedef ");
                astn* tp2 = curr + curr->common.size - 1;
                print_type(tp2 - tp2->common.size);
                putchar(' ');
                print_type(tp2);
                curr = tp2+1;
                putchar(';');
                if(trailing_nl)putchar('\n');
            }break;
            case ASTNT_FOR:{
                write("for(");
                astn* st = curr + 1;
                print_ast_within(cu,0,st,  st + st->expr.size, false);
                st+= st->expr.size;
                astn* st_end = curr + curr->common.size;
                putchar(' ');
                st+= st->expr.size;
                print_sub_expr(st-1, false);
                while(st != st_end - 1){
                    putchar(';');putchar(' ');
                    st+= st->expr.size;
                    print_sub_expr(st-1, false);
                }
                putchar(')');putchar('{');putchar('\n');
                curr = print_block(cu, indent + 1, st_end);
                print_indent(indent); putchar('}');
                if(trailing_nl)putchar('\n');
            }break;
            case ASTNT_WHILE:{
                write("while(");
                print_sub_expr(curr + curr[1].expr.size, false);
                write("){\n");
                curr = print_block(cu, indent + 1, curr + curr->common.size);
                print_indent(indent); putchar('}');
                if(trailing_nl)putchar('\n');
            }break;
            case ASTNT_IF:{
                write("if(");
                print_sub_expr(curr+curr->common.size-1, false);
                write("){\n");
                curr = print_block(cu, indent + 1, curr + curr->common.size);
                print_indent(indent); putchar('}');
                if(trailing_nl)putchar('\n');
            }break;
            case ASTNT_IF_ELSE:{
                write("if(");
                print_sub_expr(curr+curr->common.size-1, false);
                write("){\n");
                curr = print_block(cu, indent + 1, curr + curr->common.size);
                print_indent(indent); putchar('}');
                write("\n");
                print_indent(indent);
                write("else{\n");
                curr = print_block(cu, indent + 1, curr);
                print_indent(indent); putchar('}');
                if(trailing_nl)putchar('\n');
            }break;
            case ASTNT_STRUCT_DECLARATION:{
                write("struct ");
                print_type(curr + curr->common.size - 1);
                putchar('{');putchar('\n');
                curr = print_block(cu, indent + 1, curr + curr->common.size);
                print_indent(indent); putchar('}');
                if(trailing_nl)putchar('\n');
            }break;
            case ASTNT_GENERIC_STRUCT_DECLARATION:{
                write("struct ");
                astn* generic_params_size = curr + curr->common.size - 1;
                print_type(generic_params_size - generic_params_size->common.size);
                putchar('{');
                reverse_print_generic_param_list(generic_params_size-1,
                     generic_params_size-generic_params_size->common.size);
                putchar('}');putchar('{');putchar('\n');
                curr = print_block(cu, indent + 1, curr + curr->common.size);
                print_indent(indent); putchar('}');
                if(trailing_nl)putchar('\n');
            }break;
            default:CIM_ERROR("Unexpected ASTN");
        }
    }
}
astn* print_block(cunit* cu, ureg indent, astn* blk)
{
    astn* blk_end = (astn*)((u8*)blk+blk->full_size);
    print_ast_within(cu, indent, blk+1, blk_end, true);
    return blk_end;
}
void print_ast(cunit* cu){
    print_ast_within(cu, 0, (void*)cu->ast.start, (void*)cu->ast.head, true);
}

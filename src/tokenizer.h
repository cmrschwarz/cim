#pragma once
void print_token(cunit* cu, token* t);
void print_rel_str(cunit* cu, ureg str);
char* store_string(cunit* cu, char* str, char* str_end, ureg* str_pos);
void print_token(cunit* cu, token* t);
void get_token(cunit* cu, token* tok);
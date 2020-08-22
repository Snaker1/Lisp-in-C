#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>

/* If we are compiling on Windows compile these functions */
#ifdef _WIN32
#include <string.h>

static char buffer[2048];

/* Fake readline function */
char* readline(char* prompt) {
  fputs(prompt, stdout);
  fgets(buffer, 2048, stdin);
  char* cpy = malloc(strlen(buffer)+1);
  strcpy(cpy, buffer);
  cpy[strlen(cpy)-1] = '\0';
  return cpy;
}

/* Fake add_history function */
void add_history(char* unused) {}

/* Otherwise include the editline headers */
#else
#include <editline/readline.h>
#include <editline/history.h>
#endif
#define LASSERT(args, cond, fmt, ...) \
  if (!(cond)) { \
      lval* err = lval_err(fmt, ##__VA_ARGS__); \
      lval_del(args); \
      return err; \
  }

#define LASSERT_TYPE(func, args, index, expect) \
  LASSERT(args, args->cell[index]->type == expect, \
    "Function '%s' passed incorrect type for argument %i. " \
    "Got %s, Expected %s.", \
    func, index, ltype_name(args->cell[index]->type), ltype_name(expect))

#define LASSERT_NUM(func, args, num) \
  LASSERT(args, args->count == num, \
    "Function '%s' passed incorrect number of arguments. " \
    "Got %i, Expected %i.", \
    func, args->count, num)

#define LASSERT_NOT_EMPTY(func, args, index) \
  LASSERT(args, args->cell[index]->count != 0, \
    "Function '%s' passed {} for argument %i.", func, index);

/* Function to unescape characters */
char lval_str_unescape(char x) {
    switch (x) {
        case 'a':  return '\a';
        case 'b':  return '\b';
        case 'f':  return '\f';
        case 'n':  return '\n';
        case 'r':  return '\r';
        case 't':  return '\t';
        case 'v':  return '\v';
        case '\\': return '\\';
        case '\'': return '\'';
        case '\"': return '\"';
    }
    return '\0';
}

/* Possible unescapable characters */
char* lval_str_unescapable = "abfnrtv\\\'\"";

/* List of possible escapable characters */
char* lval_str_escapable = "\a\b\f\n\r\t\v\\\'\"";

/* Function to escape characters */
char* lval_str_escape(char x) {
  switch (x) {
    case '\a': return "\\a";
    case '\b': return "\\b";
    case '\f': return "\\f";
    case '\n': return "\\n";
    case '\r': return "\\r";
    case '\t': return "\\t";
    case '\v': return "\\v";
    case '\\': return "\\\\";
    case '\'': return "\\\'";
    case '\"': return "\\\"";
  }
  return "";
}



struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

typedef lval*(*lbuiltin)(lenv*, lval*);

/* Declare new lval struct*/
typedef struct lval {
  int type;
  long num;
  /* Error and Symbol types have some string data */
  char* err;
  char* sym;
  char* str;
  lbuiltin builtin;
  lenv* env;
  lval *formals;
  lval* body;
  /* Count and Pointer to a list of "lval*"; */
  int count;
  struct lval** cell;
} lval;

struct lenv {
    lenv* par;
    int count;
    char** syms;
    lval** vals;
};

lenv* lenv_new(void) {
    lenv* e = malloc(sizeof(lenv));
    e->count = 0;
    e->syms = NULL;
    e->vals = NULL;
    e->par = NULL;
    return e;
}
void lval_del(lval* v);
lval* lval_copy(lval* v);
lval* lval_err(char*  fmt, ...);
lval* lval_eval(lenv* e, lval* v);
lval* lval_builtin_op(lenv* e, lval* a, char* op);
lval* lval_pop(lval* v, int i);
lval* lval_take(lval* v, int i);
int lval_eq(lval* x, lval* y);
lval* lval_read(char* s, int* i);
lval* lval_read_expr(char* s, int* i, char end);


void lenv_del(lenv* e) {
    for (int i = 0; i < e->count; i++) {
    free(e->syms[i]);
    lval_del(e->vals[i]);
    }
    free(e->syms);
    free(e->vals);
    free(e);
}
lval* lenv_get(lenv* e, lval* k) {
    for (int i = 0; i < e->count; i++) {
    if (strcmp(e->syms[i], k->sym) == 0 ) {
        return lval_copy(e->vals[i]);
    }
    }
    if (e->par) {
        return lenv_get(e->par, k);
    } else {
        return lval_err("Unbound symbol '%s'!", k->sym);
    }
}

lenv* lenv_copy(lenv* e) {
    lenv* n = malloc(sizeof(lenv));
    n->par = e->par;
    n->count = e->count;
    n->syms = malloc(sizeof(char*) * n->count);
    n->vals = malloc(sizeof(lval) * n->count);
    for (int i = 0; i < e->count; i++) {
        n->syms[i] = malloc(strlen(e->syms[i]) + 1);
        strcpy(n->syms[i], e->syms[i]);
        n->vals[i] = lval_copy(e->vals[i]);
    }
    return n;
}

void lenv_put(lenv* e, lval* k, lval* v) {
    for (int i = 0; i < e->count; i++) {
    if (strcmp(e->syms[i], k->sym) == 0 ) {
        lval_del(e->vals[i]);
        e->vals[i] = lval_copy(v);
        return;
    }
    }
    /* If no existing entry found allocate space for new entry */
    e->count++;
    e->vals = realloc(e->vals, sizeof(lval*) * e->count);
    e->syms = realloc(e->syms, sizeof(char*) * e->count);
    e->vals[e->count-1] = lval_copy(v);
    e->syms[e->count-1] = malloc(strlen(k->sym)+1);
    strcpy(e->syms[e->count-1], k->sym);
}
void lenv_def(lenv* e, lval* k, lval* v) {
    while (e->par) {
        e = e->par;
    }
    lenv_put(e, k, v);
}
/* Create enumeraton of possible lval types */
enum {LVAL_NUM, LVAL_BOOL, LVAL_ERR, LVAL_SYM, LVAL_STR, LVAL_FUN, LVAL_SEXPR, LVAL_QEXPR};

/* Create a pointer to a new number type lval */
lval* lval_num(long x) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_NUM;
    v->num = x;
    return v;
}

lval* lval_bool(long x) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_BOOL;
    v->num = x;
    return v;
}

/* Create error type lval */
lval* lval_err(char*  fmt, ...) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_ERR;
    va_list va;
    va_start(va, fmt);
    v->err = malloc(512);
    vsnprintf(v->err, 511, fmt, va);
    v->err = realloc(v->err, strlen(v->err) + 1);
    va_end(va);
    return v;
}
char* ltype_name(int t) {
  switch(t) {
    case LVAL_FUN: return "Function";
    case LVAL_NUM: return "Number";
    case LVAL_ERR: return "Error";
    case LVAL_SYM: return "Symbol";
    case LVAL_STR: return "String";
    case LVAL_BOOL: return "Boolean";
    case LVAL_SEXPR: return "S-Expression";
    case LVAL_QEXPR: return "Q-Expression";
    default: return "Unknown";
  }
}

/* Create symbol type lval */
lval* lval_sym(char*  s) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_SYM;
    v->sym = malloc(strlen(s) + 1);
    strcpy(v->sym, s);
    return v;
}

/* Create str type lval */
lval* lval_str(char*  s) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_STR;
    v->str = malloc(strlen(s) + 1);
    strcpy(v->str, s);
    return v;
}


/* Create a pointer to a new empty Sexpr type lval */
lval* lval_sexpr(void) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_SEXPR;
    v->count = 0;
    v->cell = NULL;
    return v;
}

lval* lval_qexpr(void) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_QEXPR;
    v->count = 0;
    v->cell = NULL;
    return v;
}

lval* lval_fun(lbuiltin func) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_FUN;
    v->builtin = func;
    return v;
}

lval* lval_lambda(lval* formals, lval* body) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_FUN;
    v->builtin = NULL;
    v->env = lenv_new();
    v->formals = formals;
    v->body = body;
    return v;
}

void lval_del(lval* v) {

    switch (v->type) {
    case LVAL_NUM: break;
    case LVAL_BOOL: break;
    case LVAL_ERR: free(v->err); break;
    case LVAL_SYM: free(v->sym); break;
    case LVAL_STR: free(v->str); break;
    case LVAL_FUN:
        if (!v->builtin) {
            lenv_del(v->env);
            lval_del(v->formals);
            lval_del(v->body);
        }
        break;

    /* If Qexpr or Sexpr then delete all elements inside */
    case LVAL_QEXPR:
    case LVAL_SEXPR:
      for (int i = 0; i < v->count; i++) {
        lval_del(v->cell[i]);
      }
      /* Also free the memory allocated to contain the pointers */
      free(v->cell);
    break;
  }

  free(v);
}

lval* lval_copy(lval* v) {
    lval* x = malloc(sizeof(lval));
    x->type = v->type;

    switch (v->type) {
    case LVAL_FUN:
        if (v->builtin) {
            x->builtin = v->builtin;
        } else {
            x->builtin = NULL;
            x->env = lenv_copy(v->env);
            x->formals = lval_copy(v->formals);
            x->body = lval_copy(v->body);
        }
          break;
        case LVAL_BOOL:
        case LVAL_NUM: x->num = v->num; break;
    case LVAL_ERR:
       x->err = malloc(strlen(v->err)+ 1);
       strcpy(x->err, v->err);
       break;
    case LVAL_SYM:
       x->sym = malloc(strlen(v->sym)+ 1);
       strcpy(x->sym, v->sym);
       break;
    case LVAL_STR:
       x->str = malloc(strlen(v->str)+ 1);
       strcpy(x->str, v->str);
       break;

    case LVAL_SEXPR:
    case LVAL_QEXPR:
       x->count = v->count;
       x->cell = malloc(sizeof(lval*) * x->count);
       for (int i = 0; i < x->count; i++) {
           x->cell[i] = lval_copy(v->cell[i]);
       }
       break;
    }

    return x;
}
void lval_print(lval* v);
void lval_expr_print(lval* v, char open, char close) {
    putchar(open);
    for (int i = 0; i < v->count; i++) {
    /* Print values contained within */
    lval_print(v->cell[i]);
    /* Don't put trailing space if last element */
    if (i != (v->count-1)) {
        putchar(' ');
    };
    }
    putchar(close);
}
void lval_print_str(lval* v) {
    putchar('"');
    /* Loop over the characters in the string */
    for (int i = 0; i < strlen(v->str); i++) {
        if (strchr(lval_str_escapable, v->str[i])) {
        /* If the character is escapable then escape it */
        printf("%s", lval_str_escape(v->str[i]));
        } else {
        /* Otherwise print character as it is */
        putchar(v->str[i]);
        }
    }
    putchar('"');
}
/* Print an lval */
void lval_print(lval* v) {
    switch(v->type) {
        // In case the type is a number print it
        // Then break out of the switch
        case LVAL_NUM: printf("%li", v->num); break;
        case LVAL_BOOL: if (v->num == 0) {printf("false");} else {printf("true");}; break;
        case LVAL_ERR: printf("Error: %s", v->err); break;
        case LVAL_SYM: printf("%s", v->sym); break;
        case LVAL_STR: lval_print_str(v); break;
        case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break;
        case LVAL_QEXPR: lval_expr_print(v, '{', '}'); break;
        case LVAL_FUN:
            if (v->builtin) {
                printf("<builtin>");
            } else {
                printf("\\ ");
                lval_print(v->formals);
                putchar(' ');
                lval_print(v->body);
                putchar(' ');
            } break;
    };
}
lval* lval_add(lval* v, lval* x) {
    v->count++;
    v->cell = realloc(v->cell, sizeof(lval*) * v->count);
    v->cell[v->count-1] = x;
    return v;
}

// Print lval followed by newline
void lval_println(lval* v) {
    lval_print(v);
    putchar('\n');
}

lval* lval_pop(lval* v, int i) {
    lval* x = v->cell[i];
    memmove(&v->cell[i], &v->cell[i+1],
    sizeof(lval*) * (v->count-i-1));
    v->count--;
    v->cell = realloc(v->cell, sizeof(lval*) * v->count);
    return x;
}
lval* lval_take(lval* v, int i) {
    lval* x = lval_pop(v, i);
    lval_del(v);
    return x;
}

lval* lval_builtin_op(lenv* e, lval* a, char* op) {
    for (int i = 0; i < a->count; i++) {
    if (a->cell[i]->type != LVAL_NUM) {
        lval_del(a);
        return lval_err("Cannot operate on non-number!");
    }
    }
    // Pop the first element
    lval* x = lval_pop(a, 0);
    // If no arguments and sub than perform unary negation
    if (strcmp(op, "-") == 0 && a->count == 0) {
    x->num = -x->num;
    }
    // while there are still elments remaining
    while (a->count > 0) {
    // pop the next elemen
    lval* y = lval_pop(a, 0);

    if (strcmp(op, "+") == 0) { x->num += y->num; }
    if (strcmp(op, "-") == 0) { x->num -= y->num; }
    if (strcmp(op, "*") == 0) { x->num *= y->num; }
    if (strcmp(op, "%") == 0) {
        if (y->num == 0) {
        lval_del(x); lval_del(y);
        x = lval_err("Division By Zero!"); break;
        }
        x->num %= y->num;
    }
    if (strcmp(op, "/") == 0) {
        if (y->num == 0) {
        lval_del(x); lval_del(y);
        x = lval_err("Division By Zero!"); break;
        }
        x->num /= y->num;
    }
    lval_del(y);
    }
    lval_del(a);
    return(x);
}
lval* builtin_plus(lenv* e, lval* a) {
    return lval_builtin_op(e, a, "+");
}
lval* builtin_minus(lenv* e, lval* a) {
    return lval_builtin_op(e, a, "-");
}
lval* builtin_mult(lenv* e, lval* a) {
    return lval_builtin_op(e, a, "*");
}
lval* builtin_div(lenv* e, lval* a) {
    return lval_builtin_op(e, a, "/");
}
lval* builtin_mod(lenv* e, lval* a) {
    return lval_builtin_op(e, a, "%");
}
lval* builtin_head(lenv* e, lval* a) {
    LASSERT_NUM("head", a, 1);
    LASSERT_TYPE("head", a, 0, LVAL_QEXPR);
    LASSERT_NOT_EMPTY("head", a, 0);
    lval* v = lval_take(a, 0);
    while (v->count > 1) { lval_del(lval_pop(v, 1)); }
    return v;
}
lval* builtin_tail(lenv* e, lval* a) {
    LASSERT_NUM("tail", a, 1);
    LASSERT_TYPE("tail", a, 0, LVAL_QEXPR);
    LASSERT_NOT_EMPTY("tail", a, 0);
    lval* v = lval_take(a, 0);
    lval_del(lval_pop(v,0));
    return v;
}
lval* builtin_list(lenv* e, lval* a) {
    a->type = LVAL_QEXPR;
    return a;
}
lval* builtin_eval(lenv* e, lval* a) {
    LASSERT_NUM("eval", a, 1);
    LASSERT_TYPE("eval", a, 0, LVAL_QEXPR);
    lval* x = lval_take(a, 0);
    x->type = LVAL_SEXPR;
    return lval_eval(e, x);
}
lval* lval_join(lval* x, lval* y) {
    while (y->count > 0) {
        x = lval_add(x, lval_pop(y, 0));
    }
    lval_del(y);
    return x;
}
lval* builtin_join(lenv* e, lval* a) {
    for (int i = 0; i < a->count; i++) {
        LASSERT_TYPE("join", a, i, LVAL_QEXPR);
    }
    lval* x = lval_pop(a, 0);
    while (a->count > 0) {
        x = lval_join(x, lval_pop(a, 0));
    }
    lval_del(a);
    return x;
}
lval* builtin_var(lenv* e, lval* a, char* func) {
    LASSERT_TYPE(func, a, 0, LVAL_QEXPR);
    lval* syms = a->cell[0];
    for (int i = 0; i < syms->count; i++) {
        LASSERT(a, (syms->cell[i]->type == LVAL_SYM),
        "Function '%s' cannot define non-symbol. "
        "Got %s, Expected %s.", func,
        ltype_name(syms->cell[i]->type),
        ltype_name(LVAL_SYM));
    }

    LASSERT(a, (syms->count == a->count-1),
        "Function '%s' passed too many arguments for symbols. "
        "Got %i, Expected %i.", func, syms->count, a->count-1);
    for (int i = 0; i < syms->count; i++) {
        if (strcmp(func, "def") == 0) {
            lenv_def(e, syms->cell[i], a->cell[i+1]);
        };
        if (strcmp(func, "=") == 0) {
            lenv_put(e, syms->cell[i], a->cell[i+1]);
        };
    }
    lval_del(a);
    return lval_sexpr();
}
lval* builtin_def(lenv* e, lval* a) {
    return builtin_var(e, a, "def");
}
lval* builtin_put(lenv* e, lval* a) {
    return builtin_var(e, a, "=");
}
lval* builtin_lambda(lenv* e, lval* a) {
    LASSERT_NUM("\\", a, 2)
    LASSERT_TYPE("\\", a, 0, LVAL_QEXPR);
    LASSERT_TYPE("\\", a, 1, LVAL_QEXPR);
    for (int i = 0; i < a->cell[0]->count; i++) {
        LASSERT(a, a->cell[0]->cell[i]->type == LVAL_SYM,
        "Can not define non-symbol!\n Got %s, expected %s",
        ltype_name(a->cell[0]->cell[i]->type), ltype_name(LVAL_SYM));
    }
    lval* formals = lval_pop(a, 0);
    lval* body = lval_pop(a, 0);
    lval_del(a);
    return lval_lambda(formals, body);
}
lval* builtin_if(lenv* e, lval* a) {
    LASSERT_NUM("if", a, 3);
    LASSERT_TYPE("if", a, 0, LVAL_BOOL);
    LASSERT_TYPE("if", a, 1, LVAL_QEXPR);
    LASSERT_TYPE("if", a, 2, LVAL_QEXPR);
    lval* x;
    a->cell[1]->type = LVAL_SEXPR;
    a->cell[2]->type = LVAL_SEXPR;
    if (a->cell[0]->num) {
        x = lval_eval(e, lval_pop(a, 1));
    } else {
        x = lval_eval(e, lval_pop(a, 2));
    }
    lval_del(a);
    return x;
}

lval* lval_builtin_ord(lenv* e, lval* a, char* op) {
    LASSERT_NUM(op, a, 2);
    LASSERT_TYPE(op, a, 0, LVAL_NUM);
    LASSERT_TYPE(op, a, 1, LVAL_NUM);
    int r;
    int x1 = a->cell[0]->num;
    int x2 = a->cell[1]->num;

    if (strcmp(op, ">") == 0) {
        r = x1 > x2;
    } else if (strcmp(op, "<") == 0) {
        r = x1 < x2;
    } else if (strcmp(op, ">=") == 0) {
        r = x1 >= x2;
    } else if (strcmp(op, "<=") == 0) {
        r = x1 <= x2;
    }
    lval_del(a);
    return lval_bool(r);
}
lval* builtin_gt(lenv* e, lval* a) {
    return lval_builtin_ord(e, a, ">");
}
lval* builtin_lt(lenv* e, lval* a) {
    return lval_builtin_ord(e, a, "<");
}
lval* builtin_ge(lenv* e, lval* a) {
    return lval_builtin_ord(e, a, ">=");
}
lval* builtin_le(lenv* e, lval* a) {
    return lval_builtin_ord(e, a, "<=");
}

int lval_eq(lval* x, lval* y) {
    if (x->type != y->type) {
        return 0;
    }
    switch (x->type) {
        case LVAL_BOOL:
        case LVAL_NUM:
            return x->num == y->num;
        case LVAL_ERR:
            return (strcmp(x->err, y->err) == 0);
        case LVAL_SYM:
            return (strcmp(x->sym, y->sym) == 0);
        case LVAL_STR:
            return (strcmp(x->str, y->str) == 0);
        case LVAL_FUN:
            if (x->builtin || y->builtin) {
                return x->builtin == y->builtin;
            }
            return lval_eq(x->formals, y->formals) && lval_eq(x->body, y->body);
        case LVAL_QEXPR:
        case LVAL_SEXPR:
            if (x->count != y->count) {
                return 0;
            }
            for (int i = 0; i < x->count; i++) {
                if(!lval_eq(x->cell[i], y->cell[i])) {
                    return 0;
                }
            }
            return 1;

    }
    return 0;
}

lval* lval_builtin_cmp(lenv* e, lval* a, char* op) {
    LASSERT_NUM(op, a, 2);
    int r = 0;
    lval* x1 = a->cell[0];
    lval* x2 = a->cell[1];
    if (strcmp(op, "=") == 0) {
        r = lval_eq(x1, x2);
    } else {
        r = !lval_eq(x1, x2);
    }
    lval_del(a);
    return lval_bool(r);
}

lval* builtin_eq(lenv* e, lval* a) {
    return lval_builtin_cmp(e, a, "=");
}
lval* builtin_neq(lenv* e, lval* a) {
    return lval_builtin_cmp(e, a, "!=");
}

lval* builtin_neg(lenv* e, lval* a) {
    LASSERT_NUM("!", a, 1);
    LASSERT_TYPE("!", a, 0, LVAL_BOOL);
    int a_negated = !(a->cell[0]->num);
    lval_del(a);
    return lval_bool(a_negated);
}

lval* builtin_is_empty(lenv* e, lval* a) {
    LASSERT_NUM("is-empty", a, 1);
    LASSERT_TYPE("is-empty", a, 0, LVAL_QEXPR);
    int is_empty = 0;
    if (a->cell[0]->count == 0) {
        is_empty = 1;
    }
    lval_del(a);
    return lval_bool(is_empty);
}

lval* builtin_get_nth(lenv* e, lval* a) {
    LASSERT_NUM("nth", a, 2);
    LASSERT_TYPE("nth", a, 0, LVAL_NUM);
    LASSERT_TYPE("nth", a, 1, LVAL_QEXPR);
    int i = a->cell[0]->num;
    if (i < 1) {
        return lval_err("Index can not be lower than 1, got %li", i);
    }
    lval* target = a->cell[1];
    if (target->count < i) {
        return lval_err("Tried to access element %li but list has only %li elements", i, target->count);
    }
    lval* result = lval_copy(target->cell[i-1]);
    lval_del(a);
    return result;
}
lval* builtin_get_len(lenv* e, lval* a) {
    LASSERT_NUM("len", a, 1);
    LASSERT_TYPE("len", a, 0, LVAL_QEXPR);
    int len = a->cell[0]->count;
    lval_del(a);
    return lval_num(len);
}

lval* builtin_or(lenv* e, lval* a) {
    LASSERT_NUM("or", a, 2);
    LASSERT_TYPE("or", a, 0, LVAL_BOOL);
    LASSERT_TYPE("or", a, 1, LVAL_BOOL);
    long x1 = a->cell[0]->num;
    long x2 = a->cell[1]->num;
    int r = x1 || x2;
    lval_del(a);
    return lval_bool(r);
}
lval* builtin_and(lenv* e, lval* a) {
    LASSERT_NUM("or", a, 2);
    LASSERT_TYPE("or", a, 0, LVAL_BOOL);
    LASSERT_TYPE("or", a, 1, LVAL_BOOL);
    long x1 = a->cell[0]->num;
    long x2 = a->cell[1]->num;
    int r = x1 && x2;
    lval_del(a);
    return lval_bool(r);
}

lval* builtin_dump(lenv* e, lval* a) {
    for (int i = 0; i < a->count; i++) {
        lval_print(a->cell[i]);
        putchar(' ');
    }
    putchar('\n');
    lval_del(a);
    return lval_sexpr();
}

lval* builtin_print(lenv* e, lval* a) {
    LASSERT_NUM("print", a, 1);
    LASSERT_TYPE("print", a, 0, LVAL_STR);
    printf("%s", a->cell[0]->str);
    putchar('\n');
    lval_del(a);
    return lval_sexpr();
}

lval* builtin_error(lenv* e, lval* a) {
    LASSERT_NUM("print", a, 1);
    LASSERT_TYPE("print", a, 0, LVAL_STR);
    char* err_str = a->cell[0]->str;
    lval* err = lval_err(err_str);
    lval_del(a);
    return err;
}
lval* builtin_load(lenv* e, lval* a) {
    LASSERT_NUM("print", a, 1);
    LASSERT_TYPE("print", a, 0, LVAL_STR);
    FILE* f = fopen(a->cell[0]->str, "rb");
    if (f == NULL) {
        lval* err = lval_err("Could not load library %s", a->cell[0]->str);
        lval_del(a);
        return err;
    }
    fseek(f, 0, SEEK_END);
    long length = ftell(f);
    fseek(f, 0, SEEK_SET);
    char* input = calloc(length+1, 1);
    fread(input, 1, length, f);
    fclose(f);
    int pos = 0;
    lval* expr = lval_read_expr(input, &pos, '\0');
    free(input);
    if (expr->type != LVAL_ERR) {
        while(expr->count) {
            lval* x = lval_eval(e, lval_pop(expr, 0));
            if (x->type == LVAL_ERR) {
                lval_println(x);
            }
            lval_del(x);
        }
    } else {
        lval_println(expr);
    }
    lval_del(expr);
    lval_del(a);
    return lval_sexpr();
}

lval* builtin_num_tostr(lenv* e, lval* a) {
    LASSERT_NUM("num-tostr", a, 1);
    LASSERT_TYPE("num-tostr", a, 0, LVAL_NUM);
    char* str = malloc(sizeof(char)*20);
    sprintf(str, "%li", a->cell[0]->num);
    lval* result = lval_str(str);
    lval_del(a);
    free(str);
    return result;
}
lval* builtin_str_tonum(lenv* e, lval* a) {
    LASSERT_NUM("str-tonum", a, 1);
    LASSERT_TYPE("str-tonum", a, 0, LVAL_STR);
    long x = 0;
    sscanf(a->cell[0]->str, "%li", &x);
    lval_del(a);
    return lval_num(x);
}
lval* builtin_read(lenv* e, lval* a) {
    LASSERT_NUM("read", a, 1);
    LASSERT_TYPE("read", a, 0, LVAL_STR);
    char* input = readline(a->cell[0]->str);
    return lval_str(input);
}

void lenv_add_builtin(lenv* e, char* name, lbuiltin func) {
    lval* k = lval_sym(name);
    lval* v = lval_fun(func);
    lenv_put(e, k, v);
    lval_del(k);
    lval_del(v);
}
void lenv_add_builtins(lenv* e) {
    lenv_add_builtin(e, "list", builtin_list);
    lenv_add_builtin(e, "head", builtin_head);
    lenv_add_builtin(e, "tail", builtin_tail);
    lenv_add_builtin(e, "join", builtin_join);
    lenv_add_builtin(e, "eval", builtin_eval);
    lenv_add_builtin(e, "+", builtin_plus);
    lenv_add_builtin(e, "-", builtin_minus);
    lenv_add_builtin(e, "*", builtin_mult);
    lenv_add_builtin(e, "/", builtin_div);
    lenv_add_builtin(e, "mod", builtin_mod);
    lenv_add_builtin(e, "def", builtin_def);
    lenv_add_builtin(e, "let", builtin_put);
    lenv_add_builtin(e, "\\", builtin_lambda);
    lenv_add_builtin(e, "if", builtin_if);
    lenv_add_builtin(e, ">", builtin_gt);
    lenv_add_builtin(e, "<", builtin_lt);
    lenv_add_builtin(e, ">=", builtin_ge);
    lenv_add_builtin(e, "<=", builtin_le);
    lenv_add_builtin(e, "=", builtin_eq);
    lenv_add_builtin(e, "!=", builtin_neq);
    lenv_add_builtin(e, "!", builtin_neg);
    lenv_add_builtin(e, "is-empty", builtin_is_empty);
    lenv_add_builtin(e, "nth", builtin_get_nth);
    lenv_add_builtin(e, "len", builtin_get_len);
    lenv_add_builtin(e, "or", builtin_or);
    lenv_add_builtin(e, "and", builtin_and);
    lenv_add_builtin(e, "dump", builtin_dump);
    lenv_add_builtin(e, "print", builtin_print);
    lenv_add_builtin(e, "error", builtin_error);
    lenv_add_builtin(e, "load", builtin_load);
    lenv_add_builtin(e, "num-tostr", builtin_num_tostr);
    lenv_add_builtin(e, "str-tonum", builtin_str_tonum);
    lenv_add_builtin(e, "read", builtin_read);
    lenv_put(e, lval_sym("true"), lval_bool(1));
    lenv_put(e, lval_sym("false"), lval_bool(0));
}

lval* lval_call(lenv* e, lval* f, lval* a) {
    if (f->builtin) {
        return f->builtin(e, a);
    }

    int given = a->count;
    int total = f->formals->count;

    while (a->count) {
      if (f->formals->count == 0) {
        lval_del(a);
        return lval_err("Function passed too many arguments."
                        "Got %i, expected $i",
                        given, total);
      }
        lval* sym = lval_pop(f->formals, 0);
        if (strcmp(sym->sym, "&") == 0) {
            if (f->formals->count != 1) {
                lval_del(a);
                return lval_err("Function format invalid.\n \
                Symbol '&' not followed by single symbol.");
            }
            lval* nsym = lval_pop(f->formals, 0);
            lenv_put(f->env, nsym, builtin_list(e, a));
            lval_del(sym);
            lval_del(nsym);
            break;
        }
        lval* val = lval_pop(a, 0);
        lenv_put(f->env, sym, val);
        lval_del(sym);
        lval_del(val);
    }
    lval_del(a);
    if (f->formals->count > 0 &&
        strcmp(f->formals->cell[0]->sym, "&") == 0) {
        /* Check to ensure that & is not passed invalidly. */
        if (f->formals->count != 2) {
            return lval_err("Function format invalid. "
            "Symbol '&' not followed by single symbol.");
        }
        lval_del(lval_pop(f->formals, 0));
        lval* sym = lval_pop(f->formals, 0);
        lval* val = lval_qexpr();
        lenv_put(f->env, sym, val);
        lval_del(sym);
        lval_del(val);
    }

    if (f->formals->count == 0) {
        f->env->par = e;
        return builtin_eval(f->env, lval_add(lval_sexpr(), lval_copy(f->body)));
    } else {
        return lval_copy(f);
    }
}

lval* lval_eval_sexpr(lenv* e, lval* v) {

    /* Evaluate Children */
    for (int i = 0; i < v->count; i++) {
        v->cell[i] = lval_eval(e, v->cell[i]);
    }

    /* Error Checking */
    for (int i = 0; i < v->count; i++) {
        if (v->cell[i]->type == LVAL_ERR) { return lval_take(v, i); }
    }

    /* Empty Expression */
    if (v->count == 0) { return v; }

    /* Single Expression */
    if (v->count == 1) { return lval_take(v, 0); }

    /* Ensure First Element is Symbol */
    lval* f = lval_pop(v, 0);
    if (f->type != LVAL_FUN) {
    lval* err = lval_err(
        "S-Expression starts with incorrect type. "
        "Got %s, Expected %s.",
        ltype_name(f->type), ltype_name(LVAL_FUN));
    lval_del(f); lval_del(v);
    return err;
    }

    /* Call builtin with operator */
    lval *result = lval_call(e, f, v);
    lval_del(f);
    return result;
}

lval* lval_eval(lenv* e, lval* v) {
    if (v->type == LVAL_SYM) {
    lval* x = lenv_get(e, v);
    lval_del(v);
    return x;
    }
    // Eval Sexpressions
    if (v->type == LVAL_SEXPR) {
        return lval_eval_sexpr(e, v);
    }
    // All other eval types remain the same
    return v;
}

/* Reading */

lval* lval_read_sym(char* s, int* i) {
    /* Allocate Empty String */
    char* part = calloc(1,1);
    /* While valid identifier characters */
    while (strchr(
      "abcdefghijklmnopqrstuvwxyz"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      "0123456789_+-*\\/=<>!&", s[*i]) && s[*i] != '\0') {

        /* Append character to end of string */
        part = realloc(part, strlen(part)+2);
        part[strlen(part)+1] = '\0';
        part[strlen(part)+0] = s[*i];
        (*i)++;
    }
    /* Check if Identifier looks like number */
    int is_num = strchr("-0123456789", part[0]) != NULL;
    for (int j = 1; j < strlen(part); j++) {
        if (strchr("0123456789", part[j]) == NULL) { is_num = 0; break; }
    }
    if (strlen(part) == 1 && part[0] == '-') { is_num = 0; }

    /* Add Symbol or Number as lval */
    lval* x = NULL;
    if (is_num) {
        errno = 0;
        long v = strtol(part, NULL, 10);
        x = (errno != ERANGE) ? lval_num(v) : lval_err("Invalid Number %s", part);
    } else {
        x = lval_sym(part);
    }
    /* Free temp string */
    free(part);
    /* Return lval */
    return x;
}


lval* lval_read_str(char* s, int* i) {
    /* Allocate empty string */
    char* part = calloc(1,1);
    /* More forward one step past initial " character */
    (*i)++;
    while (s[*i] != '"') {
        char c = s[*i];
        /* If end of input then there is an unterminated string literal */
        if (c == '\0') {
            free(part);
            return lval_err("Unexpected end of input");
        }
        /* If backslash then unescape character after it */
        if (c == '\\') {
        (*i)++;
        /* Check next character is escapable */
        if (strchr(lval_str_unescapable, s[*i])) {
            c = lval_str_unescape(s[*i]);
        } else {
            free(part);
            return lval_err("Invalid escape sequence \\%c", s[*i]);
        }
        }
        /* Append character to string */
        part = realloc(part, strlen(part)+2);
        part[strlen(part)+1] = '\0';
        part[strlen(part)+0] = c;
        (*i)++;
    }
    /* Move forward past final " character */
    (*i)++;
    lval* x = lval_str(part);
    /* free temp string */
    free(part);
    return x;
}


lval* lval_read_expr(char* s, int* i, char end) {

    /* Either create new qexpr or sexpr */
    lval* x = (end == '}') ? lval_qexpr() : lval_sexpr();

    /* While not at end character keep reading lvals */
    while (s[*i] != end) {
        lval* y = lval_read(s, i);
        /* If an error then return this and stop */
        if (y->type == LVAL_ERR) {
            lval_del(x);
            return y;
        } else {
            lval_add(x, y);
        }
    }
    /* Move past end character */
    (*i)++;

    return x;

}

lval* lval_read(char* s, int* i) {
    /* Skip all trailing whitespace and comments */
    while (strchr(" \t\v\r\n;", s[*i]) && s[*i] != '\0') {
        if (s[*i] == ';') {
            while (s[*i] != '\n' && s[*i] != '\0') { (*i)++; }
        }
        (*i)++;
    }

    lval* x = NULL;

    /* If we reach end of input then we're missing something */
    if (s[*i] == '\0') {
        return lval_err("Unexpected end of input");
    }

    /* If next character is ( then read S-Expr */
    else if (s[*i] == '(') {
        (*i)++;
        x = lval_read_expr(s, i, ')');
    }
    /* If next character is { then read Q-Expr */
    else if (s[*i] == '{') {
        (*i)++;
        x = lval_read_expr(s, i, '}');
    }
    /* If next character is part of a symbol then read symbol */
    else if (strchr(
    "abcdefghijklmnopqrstuvwxyz"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "0123456789_+-*\\/=<>!&", s[*i])) {
        x = lval_read_sym(s, i);
    }
    /* If next character is " then read string */
    else if (strchr("\"", s[*i])) {
        x = lval_read_str(s, i);
    }

    /* Encountered some unexpected character */
    else {
        x = lval_err("Unexpected character %c", s[*i]);
    }

    /* Skip all trailing whitespace and comments */
    while (strchr(" \t\v\r\n;", s[*i]) && s[*i] != '\0') {
        if (s[*i] == ';') {
            while (s[*i] != '\n' && s[*i] != '\0') { (*i)++; }
        }
        (*i)++;
    }
    return x;
}

int main(int argc, char** argv) {
    lenv* e = lenv_new();
    lenv_add_builtins(e);

    if (argc ==1) {
        /* Print version and exit information */
        puts("Laika Version 0.0.0.0.1");
        puts("Press Ctrl+c to exit.\n");

        /* In a never ending loop */
        while (1) {
            /* Output our promt */
            char* input = readline("Laika > ");

            /* Add input to history */
            add_history(input);

            int pos = 0;
            lval* expr = lval_read_expr(input, &pos, '\0');
            lval* x = lval_eval(e, expr);
            lval_println(x);
            lval_del(x);

            free(input);
        }
    } else {
        for (int i = 1; i < argc; i++) {
            lval* args = lval_add(lval_sexpr(), lval_str(argv[i]));
            lval* x = builtin_load(e, args);
            if (x->type == LVAL_ERR) {
                lval_println(x);
            }
            lval_del(x);
        }
    }
    lenv_del(e);

    return 0;
}


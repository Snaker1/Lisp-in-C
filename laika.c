#include <stdio.h>

#include <stdlib.h>
#include "mpc.h"

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

/* Declare new lval struct*/
typedef struct lval {
  int type;
  long num;
  /* Error and Symbol types have some string data */
  char* err;
  char* sym;
  /* Count and Pointer to a list of "lval*"; */
  int count;
  struct lval** cell;
} lval;

/* Create enumeraton of possible lval types */
enum {LVAL_NUM, LVAL_ERR , LVAL_SYM, LVAL_SEXPR};

/* Create enumeraton of possible lval types */
enum {LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM};

lval* lval_eval(lval* v);
lval* lval_builtin_op(lval* a, char* op);
lval* lval_pop(lval* v, int i);
lval* lval_take(lval* v, int i);

/* Create a pointer to a new number type lval */
lval* lval_num(long x) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_NUM;
    v->num = x;
    return v;
}

/* Create error type lval */
lval* lval_err(char*  m) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_ERR;
    v->err = malloc(strlen(m) + 1);
    strcpy(v->err, m);
    return v;
}

/* Create symbol type lval */
lval* lval_sym(char*  s) {
    lval* v = malloc(sizeof(lval));
    v->type = LVAL_SYM;
    v->sym = malloc(strlen(s) + 1);
    strcpy(v->sym, s);
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

void lval_del(lval* v) {
    switch (v->type) {
	case LVAL_NUM: break;
	case LVAL_ERR: free(v->err); break;
	case LVAL_SYM: free(v->sym); break;
	case LVAL_SEXPR:
	   for (int i = 0; i < v->count; i++) {
	       lval_del(v->cell[i]);
	   }
	   free(v->cell);
       break;
    };
    free(v);
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

/* Print an lval */
void lval_print(lval* v) {
    switch(v->type) {
	// In case the type is a number print it
	// Then break out of the switch
	case LVAL_NUM: printf("%li", v->num); break;
	case LVAL_ERR: printf("Error: %s", v->err); break;
	case LVAL_SYM: printf("%s", v->sym); break;
	case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break;
    };
}
lval* lval_read_num(mpc_ast_t* t) {
    errno = 0;
    long x = strtol(t->contents, NULL, 10);
    return errno != ERANGE ?
	lval_num(x) : lval_err("Invalid number");
}
lval* lval_add(lval* v, lval* x) {
    v->count++;
    v->cell = realloc(v->cell, sizeof(lval*) * v->count);
    v->cell[v->count-1] = x;
    return v;
}
lval* lval_read(mpc_ast_t* t) {

  /* If Symbol or Number return conversion to that type */
  if (strstr(t->tag, "number")) { return lval_read_num(t); }
  if (strstr(t->tag, "symbol")) { return lval_sym(t->contents); }

  /* If root (>) or sexpr then create empty list */
  lval* x = NULL;
  if (strcmp(t->tag, ">") == 0) { x = lval_sexpr(); }
  if (strstr(t->tag, "sexpr"))  { x = lval_sexpr(); }

  /* Fill this list with any valid expression contained within */
  for (int i = 0; i < t->children_num; i++) {
    if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
    if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
    if (strcmp(t->children[i]->tag,  "regex") == 0) { continue; }
    x = lval_add(x, lval_read(t->children[i]));
  }

  return x;
}


// Print lval followed by newline

void lval_println(lval* v) {
    lval_print(v);
    putchar('\n');
}


lval* lval_pop(lval* v, int i) {
    // Find the item at i
    lval* x = v->cell[i];

    // Shift memory
    memmove(&v->cell[i], &v->cell[i+1],
	    sizeof(lval*) * v->count-i-1);
    // Decrease the count of items in the list
    v->count--;
    // Reallocate the memory use
    v->cell = realloc(v->cell, sizeof(lval*) * v->count);
    return x;
}
lval* lval_take(lval* v, int i) {
    lval* x = lval_pop(v, i);
    lval_del(v);
    return x;
}

lval* lval_builtin_op(lval* a, char* op) {
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

lval* lval_eval_sexpr(lval* v) {

  /* Evaluate Children */
  for (int i = 0; i < v->count; i++) {
    v->cell[i] = lval_eval(v->cell[i]);
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
  if (f->type != LVAL_SYM) {
    lval_del(f); lval_del(v);
    return lval_err("S-expression Does not start with symbol.");
  }

  /* Call builtin with operator */
  lval* result = lval_builtin_op(v, f->sym);
  lval_del(f);
  return result;
}

lval* lval_eval(lval* v) {
    // Eval Sexpressions
    if (v->type == LVAL_SEXPR) {
	return lval_eval_sexpr(v);
    }
    // All other eval types remain the same
    return v;
}

int main(int argc, char** argv) {

    /* Create some parsers */
    mpc_parser_t* Number = mpc_new("number");
    mpc_parser_t* Symbol = mpc_new("symbol");
    mpc_parser_t* Sexpr= mpc_new("sexpr");
    mpc_parser_t* Expr= mpc_new("expr");
    mpc_parser_t* Laika = mpc_new("laika");

    /* Define them with the following language */
    mpca_lang(MPCA_LANG_DEFAULT,
	    " \
	    number : /-?[0-9]+/; \
	    symbol : '+' | '-' | '*' | '/' | '%'; \
	    sexpr : '(' <expr>* ')'; \
	    expr : <number> | <symbol> | <sexpr>; \
	    laika: /^/ <expr>* /$/ ; \
	    ",
	    Number, Symbol, Sexpr, Expr, Laika);

    /* Print version and exit information */
    puts("Laika Version 0.0.0.0.1");
    puts("Press Ctrl+c to exit.\n");

    /* In a never ending loop */
    while (1) {

	/* Output our promt */
	char* input = readline("Laika > ");

	/* Add input to history */
	add_history(input);

	/* Attempt to parse the user input */
	mpc_result_t r;
	if (mpc_parse("<stdin>", input, Laika, &r)) {
	    lval* result = lval_eval(lval_read(r.output));
	    lval_println(result);
	    lval_del(result);
	    mpc_ast_delete(r.output);
	} else {
	    // Otherwise print the error
	    mpc_err_print(r.output);
	    mpc_err_delete(r.output);
	}

	
	/* Free retrieved input */ 
	free(input);

    }
    /* Undefine and delete our parsers */
    mpc_cleanup(5, Number, Symbol, Sexpr, Expr, Laika);

    return 0;
}


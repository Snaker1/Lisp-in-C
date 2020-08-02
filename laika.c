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
typedef struct {
    int type;
    long num;
    int err;
} lval;

/* Create enumeraton of possible lval types */
enum {LVAL_NUM, LVAL_ERR };

/* Create enumeraton of possible lval types */
enum {LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM};

/* Create number type lval */
lval lval_num(long x) {
    lval v;
    v.type = LVAL_NUM;
    v.num = x;
    return v;
}

/* Create error type lval */
lval lval_err(int  x) {
    lval v;
    v.type = LVAL_ERR;
    v.err = x;
    return v;
}


/* Print an lval */
void lval_print(lval v) {
    switch(v.type) {
	// In case the type is a number print it
	// Then break out of the switch
	case LVAL_NUM: printf("%li", v.num); break;

        // IN case the pyte is an error
	case LVAL_ERR:
        // Check what type of error it is and print it
	   if (v.err == LERR_DIV_ZERO) {
	       printf("Error: Division by zero!");
	   }
	   if (v.err == LERR_BAD_OP) {
	       printf("Error: Invalid operator!");
	   }
	   if (v.err == LERR_BAD_NUM) {
	       printf("Error: Invalid number!");
	   };
	   break;
    }
}
// Print lval followed by newline
void lval_println(lval v) {
    lval_print(v);
    putchar('\n');
}

lval eval_op(lval x, char* op, lval y) {
    // If either value is an error return it
    if (x.type == LVAL_ERR) {return x;}
    if (y.type == LVAL_ERR) {return y;}
    // Otherwise do maths
    if (strcmp(op, "+") == 0) {return lval_num(x.num + y.num);}
    if (strcmp(op, "-") == 0) {return lval_num(x.num - y.num);}
    if (strcmp(op, "*") == 0) {return lval_num(x.num * y.num);}
    if (strcmp(op, "/") == 0) {
	// If second operand is zero return error
	return y.num == 0
	    ? lval_err(LERR_DIV_ZERO)
	    : lval_num(x.num / y.num);
    }


    return lval_err(LERR_BAD_OP);
}

lval eval(mpc_ast_t* t) {

    /* If tagged as number return it directly */
    if (strstr(t->tag, "number")) {
	// Check if there is some error in the conversion
	errno = 0;
	long x = strtol(t->contents, NULL, 10);
        return errno != ERANGE ? lval_num(x) : lval_err(LERR_BAD_NUM); 
    }

    /* The operator is always second child */
    char* op = t->children[1]->contents;

    /* We store the third child in x */
    lval x = eval(t->children[2]);

    /* Iterate the remaining children and combining */
    int i = 3;
    while (strstr(t->children[i]->tag, "expr")) {
	x = eval_op(x, op, eval(t->children[i]));
	i++;
    }

    return x;
}


int main(int argc, char** argv) {

    /* Create some parsers */
    mpc_parser_t* Number = mpc_new("number");
    mpc_parser_t* Operator = mpc_new("operator");
    mpc_parser_t* Expr= mpc_new("expr");
    mpc_parser_t* Laika = mpc_new("laika");

    /* Define them with the following language */
    mpca_lang(MPCA_LANG_DEFAULT,
	    " \
	    number : /-?[0-9]+/; \
	    operator : '+' | '-' | '*' | '/'; \
	    expr : <number> | '(' <operator> <expr>+ ')'; \
	    laika: /^/ <operator> <expr>+ /$/ ; \
	    ",
	    Number, Operator, Expr, Laika);

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
	    lval result = eval(r.output);
	    lval_println(result);
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
    mpc_cleanup(4, Number, Operator, Expr, Laika);

    return 0;
}


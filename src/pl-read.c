/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include <stdlib.h>			// For strtod(3)

#include "Prolog.h"
#include "pl-stream.h"
#include "pl-ctype.h"
#include "pl-atom.h"
#include "pl-fun.h"
#include "pl-op.h"
#include "pl-string.h"
#include "pl-option.h"
#include "pl-buffer.h"
#include "pl-util.h"
#include "pl-status.h"
#include "pl-fli.h"
#include "pl-io.h"

typedef enum {
	T_RPAREN = ')',
	T_COMMA = ',',
	T_RSQUARE = ']',
	T_PIPE = '|',
	T_STOP = 256, T_OP, T_ATOM, T_FUN, T_INTG, T_FLT,
	T_STRING, T_VOID, T_VAR, T_EOF, T_ERROR
} tok_type;

typedef struct var_t var_t, *Var;
struct var_t {
	char *name;
	int times;
	cell_t *ref;
	Var next;
};

typedef struct {
	tok_type type;
	// long start;
	// long end;
	union {
		atom_t atom;
		long intg;
		double flt;
		char *string;
		cell_t *ref;
		Var var;
	} tok_val;
} token_t;

typedef struct node_t node_t;
struct node_t {				// long start;
	// long end;
	cell_t cell;
	short prec;
};

static int give_syntaxerrors = TRUE;

static token_t token;			// current_token
static int unget_token = 0;
static int must_be_op;

/**********************************************************************/
/* Look-ahead                                                         */
/**********************************************************************/

static int pushback[4];			// two char of look-ahead is enough
static int *pbp = pushback;

static					// inline
int Getc(pl_stream S)
{
	if (pbp > pushback)
		return (*--pbp);
	else
		return (Sgetc(S));
}

static					// inline
int Peekc(pl_stream S)
{
	if (pbp > pushback)
		return (pbp[-1]);
	else
		return (Speekc(S));
}

static					// inline
int UnGetc(int c)
{
	*pbp++ = c;
	return (c);
}

/**********************************************************************/
/* Syntax Errors                                                      */
/**********************************************************************/

static					// FIXME : obsolete
int syntaxerrors(int new_se)
{
	int old_se = give_syntaxerrors;
	give_syntaxerrors = new_se;
	return (old_se);
}

static const char *error_msg;

static void SyntaxError(const char *msg)
{
	error_msg = msg;
}

/**********************************************************************/
/* Utilities                                                          */
/**********************************************************************/

static					// inline
int strip_C_comment(pl_stream S)
{
	int c;

	Getc(S);			//  drop '*'

	if (PL__status.nested_com)	// TESTED : OK
	{
		int nesting = 1;

		c = Getc(S);
		for (;;) {
			switch (c) {
			case '*':
				if ((c = Getc(S)) == '/') {
					if (--nesting == 0)
						return (1);
					break;
				}
				continue;
			case '/':
				if ((c = Getc(S)) == '*') {
					nesting++;
					break;
				}
				continue;
			case EOF:
				SyntaxError("Unexpected EOF in bracketed comment");
				return (0);
			}
			c = Getc(S);
		}
	} else				// ISO bracketed comment
	{
		c = Getc(S);
		for (;;) {
			switch (c) {
			case '*':
				if ((c = Getc(S)) == '/')
					return (1);
				continue;
			case EOF:
				SyntaxError("Unexpected EOF");
				return (0);
			}
			c = Getc(S);
		}
	}
}

static					// inline
void strip_PL_comment(pl_stream S)
{
	int c;

	for (;;) {
		c = Getc(S);

		if (c == '\n')
			return;
		if (c == EOF) {
			UnGetc(c);
			return;
		}
	}
}

/**********************************************************************/
/* Variables stuff                                                    */
/**********************************************************************/

static Var varl_first = 0, varl_tail = 0, varl_free = 0;

static void clear_var_list(void)
{
	Var v = varl_first;

	if (!v)
		return;

	for (;; v = v->next) {
		if (v->name)
			free(v->name);	// FIXME : suppose that name was malloced
		if (v == varl_tail)
			break;
	}

	v->next = varl_free;
	varl_free = varl_first;
	varl_first = 0;
	return;
}

static term_t bind_vars(int single)
{
	Var v = varl_first;
	term_t l = &(ATOM(nil)->atom);

	if (!v)
		return (l);

	for (;; v = v->next) {
		if (v->name && !(single && (v->times != 1 || v->name[0] == '_'))) {
			HP[0].val = __fun(FUN(unify, 2));
			HP[1].val = __atom(PL_new_atom(v->name));
			HP[2].celp = v->ref;
			HP[3].val = __cons();
			HP[4].celp = HP;
			HP[5].celp = l;
			l = HP + 3;
			HP += 6;
		}
		if (v == varl_tail)
			break;
	}
	return (l);
}

static inline void warn_singletons(void)
{
	Var v = varl_first;
	int n = 0;
	pl_ubs_t *b = PL_find_ubs(BUF_DISCARDABLE);

	if (!v)
		return;

	for (;; v = v->next) {
		if (v->name && !(v->times != 1 || v->name[0] == '_')) {
			if (!n)
				n++;
			else {
				PL_add_ubs(b, ',');
				PL_add_ubs(b, ' ');
			}
			PL_add_x_ubs(b, v->name, strlen(v->name));
		}
		if (v == varl_tail)
			break;
	}

	if (n) {
		PL_warn("Singleton variable(s) : %s", PL_base_ubs(b));
	}
	return;
}

static Var New_Var(void)
{
	Var v;

	if (varl_free) {
		v = varl_free;
		varl_free = varl_free->next;
	} else {
		v = malloc(sizeof(var_t));
		if (!v) {
			fprintf(stderr, "Malloc error in pl-read.c: New_Var()\n");
			return (0);
		}
	}
	return (v);
}

static Var lookup_var(char *name)
{
	Var v = varl_first;

	for (v = varl_first; v; v = v->next) {
		if (v->name && streq(name, v->name)) {
			v->times++;
			return (v);
		}
	}

// Add a new var in head of list
	v = New_Var();
	v->next = varl_first;
	if (!varl_first)
		varl_tail = v;
	varl_first = v;

// adjust the fields
	v->name = name;
	v->times = 1;
	v->ref = new_var();
	return (v);
}

/**********************************************************************/
/* LEXER                                                              */
/**********************************************************************/

static inline int seeingString(pl_stream S)
{
	return (StreamType(S) == ST_RMEM);
}

static inline int DigitVal(int c)
{
	return (PL__DigitValue[c]);
}

static inline int hex_escape(pl_stream S)
{
	int c;
	int val;

	c = Getc(S);
	if (!isHexDigit(c)) {
		UnGetc(c);		// FIXME : this make  '\xg'
		return ('x');		//         the same as 'xg'
	}

	for (val = 0;;) {
		val = val * 16 + DigitVal(c);
		c = Getc(S);
		if (c == '\\')
			break;
		if (!isHexDigit(c)) {
			UnGetc(c);	// FIXME : this make the
			break;		// closing '\' facultative
		}
	}

	return (val);
}

static					// inline
int oct_escape(pl_stream S, int c)
{
	int val;

	for (val = 0;;) {
		val = val * 8 + DigitVal(c);
		c = Getc(S);
		if (c == '\\')
			break;
		if (!isOctDigit(c)) {
			UnGetc(c);	// FIXME : this make the
			break;		// closing '\' facultative
		}
	}

	return (val);
}

static					// inline
int read_quoted_char(pl_stream S)
{
	register int c;

	switch (c = Getc(S)) {
	case '`':
	case '"':
		break;
	case '\'':
		if (Peekc(S) == c)
			Getc(S);
		// else                  // FIXME : when commented
		//   return(-1);         // this make 0'' the same as 0'''
		break;
	case '\\':
		switch (c = Getc(S)) {
		case 'a':
			c = '\a';
			break;
		case 'b':
			c = '\b';
			break;
		case 'e':
			c = '\e';
			break;
		case 'f':
			c = '\f';
			break;
		case 'n':
			c = '\n';
			break;
		case 'r':
			c = '\r';
			break;
		case 't':
			c = '\t';
			break;
		case 'v':
			c = '\v';
			break;
			// case '\n': ....
		case '\'':
		case '"':
		case '`':
			break;
		case 'x':
			c = hex_escape(S);
			break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
			c = oct_escape(S, c);
			break;
		default:
			UnGetc(c);
			c = '\\';
			break;
			// default:   break;   // FIXME : non-ISO
		}
		break;
	case EOF:
		SyntaxError("Unexpected EOF in quoted string");
		UnGetc(c);
		return (-1);
	default:
		break;
	}

	return (c);
}

static					// inline
char *read_quoted_string(pl_stream S, int quote)
{
	register int c;
	pl_ubs_t *b = PL_find_ubs(BUF_DISCARDABLE);

	for (;;) {
		switch (c = Getc(S)) {
		case '\'':
		case '"':
		case '`':
			if (c == quote) {
				if (Peekc(S) == quote)
					Getc(S);
				else
					return (PL_base_ubs(b));
			}
			break;
		case '\\':
			switch (c = Getc(S)) {
			case 'a':
				c = '\a';
				break;
			case 'b':
				c = '\b';
				break;
			case '\n':	// ISO line continuation
				while (isSpace(c = Getc(S)) && c != '\n') ;
				UnGetc(c);
				continue;
			case 'f':
				c = '\f';
				break;
			case 'n':
				c = '\n';
				break;
			case 'r':
				c = '\r';
				break;
			case 't':
				c = '\t';
				break;
			case 'v':
				c = '\v';
				break;
			case '\'':
			case '"':
			case '`':
				break;
			case 'x':
				c = hex_escape(S);
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
				c = oct_escape(S, c);
				break;
			default:
				break;	// FIXME : non-ISO
			}
			break;
		case EOF:
			SyntaxError("Unexpected EOF in quoted string");
			UnGetc(c);
			return (0);
		default:
			break;		// FIXME : non-ISO
		}
		PL_add_ubs(b, c);
	}
}

static inline char *read_name(pl_stream S, int c)
// OK for non-quoted atoms and variables name
{
	pl_ubs_t b;
	PL_init_ubs(&b);

	for (; isAlphaNum_(c); c = Getc(S))
		PL_add_ubs(&b, c);

	UnGetc(c);
	return (PL_base_ubs(&b));
}

static inline char *read_symbol(pl_stream S, int c)
{
	pl_ubs_t *b = PL_find_ubs(BUF_DISCARDABLE);

	for (; isSymbol(c); c = Getc(S))
		PL_add_ubs(b, c);

	UnGetc(c);
	return (PL_base_ubs(b));
}

static					// inline
unsigned long str2long(char *str)
{
	unsigned long val = 0;

	while (*str)
		val = val * 10 + DigitVal(*str++);

	return (val);
}

#define ISO_NUM(S,P,B)	if (!P(Peekc(S))) \
                          goto case_default; \
                        while (P(c=Getc(S))) \
                        { val=val* B +DigitVal(c); } \
                        goto case_iso

static int read_number(pl_stream S, int c, pl_number_t * num)
{
	unsigned long val = 0;
	pl_ubs_t *b = PL_find_ubs(0);

	if (c == '0') {
		switch (c = Getc(S)) {
		case '\'':
			if ((val = read_quoted_char(S)) >= 0)
				goto intg;
			else
				goto error;
		case 'b':
			ISO_NUM(S, isBinDigit, 2);
		case 'o':
			ISO_NUM(S, isOctDigit, 8);
		case 'x':
			ISO_NUM(S, isHexDigit, 16);
case_default:
		default:
			UnGetc(c);
			c = '0';
			break;
case_iso:		UnGetc(c);
			goto intg;
		}
	}
// normal number or Edinburgh non-decimal number
// PRE : [0-9] is readed
	while (isDigit(c)) {
		PL_add_ubs(b, c);
		c = Getc(S);
	}

	if (c == '\'')			// Edinburgh non-decimal number
	{
		unsigned int base;
		unsigned int digit;

		val = str2long(PL_base_ubs(b));

		if (val < 2 || 36 < val)	// illegal radix
		{
			UnGetc('\'');	// return the radix
			goto intg;
		}

		base = val;
		digit = DigitVal(Peekc(S));
		if (digit >= base)	// illegal number
		{
			UnGetc('\'');	// unget '\''; return the base
			goto intg;
		}

		val = 0;
		while (1) {
			c = Getc(S);
			if ((digit = DigitVal(c)) >= base) {
				UnGetc(c);
				goto intg;
			}
			val = val * base + digit;
		}
	} else if (c == '.')		// floating number
	{
		if (!isDigit(Peekc(S))) {
			UnGetc('.');
			val = str2long(PL_base_ubs(b));
			goto intg;
		}
		PL_add_ubs(b, '.');

		while (isDigit(c = Getc(S)))	// read fraction
			PL_add_ubs(b, c);

		if (c == 'e' || c == 'E')	// read exponent
		{
			PL_add_ubs(b, c);
			c = Getc(S);
			if (c == '+' || c == '-') {
				PL_add_ubs(b, c);
				c = Getc(S);
			}
			UnGetc(c);
			if (!isDigit(c))
				goto error;
			while (isDigit(c = Getc(S)))
				PL_add_ubs(b, c);
		}
		UnGetc(c);
		num->val.flt = strtod(PL_base_ubs(b), 0);	// FIXME : overflow or underflow
		num->type = flt_tag;
		return (1);
	} else {
		UnGetc(c);
		val = str2long(PL_base_ubs(b));
		goto intg;
	}

intg:
	num->val.intg = val;
	num->type = int_tag;
	return (1);
error:
	return (0);
}

#undef	ISO_NUM

#define SkipSpaces(S,c)	do { c=Getc(S); } while (isSpace(c))

// POST : valid data in token
static tok_type get_token(pl_stream S)
{
	static atom_t atom;
	static pl_number_t num;
	int c;
	char *str;

	if (unget_token) {
		unget_token = 0;
		return (token.type);
	}

loop:
	SkipSpaces(S, c);

	switch (c) {
	case EOF:
		if (seeingString(S)) {
			UnGetc(' ');
			UnGetc('.');
			UnGetc(' ');
			goto loop;
		}
		token.type = T_EOF;	// FIXME
		break;
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
case_low:	str = read_name(S, c);
		atom = PL_new_atom(str);
		free(str);
		goto case_atom;
	case '_':
		if (!isAlphaNum(Peekc(S))) {
			token.type = T_VOID;
			token.tok_val.ref = new_void();
		} else
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':	{
				str = read_name(S, c);
				token.type = T_VAR;
				token.tok_val.var = lookup_var(str);
			}
		break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		if (read_number(S, c, &num)) {
			if (num.type == int_tag) {
				token.type = T_INTG;
				token.tok_val.intg = num.val.intg;
			} else {
				token.type = T_FLT;
				token.tok_val.flt = num.val.flt;
			}
			break;
		} else {
			return (0);
		}
	case '!':
	case ';':
		atom = PL_char_to_atom(c);
		goto case_atom;
	case '%':
		strip_PL_comment(S);
		c = ' ';
		goto loop;
	case '[':
		SkipSpaces(S, c);
		if (c == ']') {
			token.type = T_ATOM;
			token.tok_val.atom = ATOM(nil);
			break;
		} else {
			UnGetc(c);
			c = '[';	// ]
			goto case_punc;
		}
	case '{':
		SkipSpaces(S, c);
		if (c == '}') {
			token.type = T_ATOM;
			token.tok_val.atom = ATOM(curl);
			break;
		} else {
			UnGetc(c);
			c = '{';	// }
			goto case_punc;
		}
	case '(':
	case ')':
	case ']':			// {
	case '}':
	case ',':
	case '|':
case_punc:	token.tok_val.atom = PL_char_to_atom(c);
		token.type = c;
		break;
	case '\'':
		str = read_quoted_string(S, c);
		if (!str)
			goto error;
		atom = PL_new_atom(str);
		goto case_atom;
	case '\"':
		str = read_quoted_string(S, c);
		if (!str)
			goto error;
		if (PL__status.dbl_quotes == PL_new_atom("chars")) {
			token.type = T_STRING;
			token.tok_val.ref = PL_mk_char_list(str);
			break;
		} else if (PL__status.dbl_quotes == PL_new_atom("codes")) {
			token.type = T_STRING;
			token.tok_val.ref = PL_mk_code_list(str);
			break;
		} else if (PL__status.dbl_quotes == PL_new_atom("atom")) {
			atom = PL_new_atom(str);
			goto case_atom;
			break;
		} else
			goto error;
	case '`':
		str = read_quoted_string(S, c);
		if (!str)
			goto error;
		if (PL__status.bck_quotes == PL_new_atom("chars")) {
			token.type = T_STRING;
			token.tok_val.ref = PL_mk_char_list(str);
			break;
		} else if (PL__status.bck_quotes == PL_new_atom("codes")) {
			token.type = T_STRING;
			token.tok_val.ref = PL_mk_code_list(str);
			break;
		} else if (PL__status.bck_quotes == PL_new_atom("atom")) {
			atom = PL_new_atom(str);
			goto case_atom;
			break;
		} else
			goto error;

	case '/':
		if (Peekc(S) == '*') {
			if (!strip_C_comment(S))
				goto error;
			c = ' ';
			goto loop;
		} else
			goto case_sym;
	case '.':
		if (isSpace(Peekc(S))) {
			token.type = T_STOP;
			break;
		} else
			goto case_sym;
	case '\\':
	case '#':
	case '$':
	case '&':
	case '*':
	case '+':
	case '-':
	case ':':
	case '<':
	case '=':
	case '>':
	case '?':
	case '@':
	case '^':
	case '~':
case_sym:	str = read_symbol(S, c);
		atom = PL_new_atom(str);
		goto case_atom;
case_atom:	token.tok_val.atom = atom;
		if (Peekc(S) == '(')
			token.type = T_FUN;
		else if (PL_can_be_op(atom))
			token.type = T_OP;
		else
			token.type = T_ATOM;
		break;
	default:
		if (isAlphaNum_(c))
			goto case_low;
		if (isSpace(c))
			goto loop;
		if (isSymbol(c))
			goto case_sym;
		if (isSolo(c) || isPunct(c))
			goto case_punc;
	}

	return (token.type);

error:
	return (token.type = T_ERROR);
}

/**********************************************************************/
/* PARSER                                                             */
/**********************************************************************/

static int read_term(pl_stream S, int max, const char *stop, node_t * node);

static inline void mk_unary(atom_t atom, node_t * arg, node_t * node_out)
{
	if (atom == ATOM(minus) && is_number(&arg->cell)) {
		cell_t *c = &arg->cell;

		if (is_intg(c))
			get_intg(c) = -get_intg(c);
		else			// if (is_flt(c))
			get_flt(c) = -get_flt(c);

		node_out->cell = arg->cell;
	} else {
		fun_t fun = PL_new_functor(atom, 1);
		cell_t *addr = new_struct(fun, 1);

		addr[1] = arg->cell;
		node_out->cell.celp = addr;
	}
}

static inline void mk_binary(atom_t atom, node_t * left, node_t * right, node_t * node_out)
{
	fun_t fun = PL_new_functor(atom, 2);
	cell_t *addr = new_struct(fun, 2);

	addr[1] = left->cell;
	addr[2] = right->cell;
	node_out->cell.celp = addr;
}

static cell_t *read_fun(pl_stream S, atom_t functor, int level, node_t * node_out)
{
	node_t elem;
	cell_t *addr;

	if (!read_term(S, 1200, ",)", &elem))
		return (0);
	else
		level++;

	switch (token.type) {
	case ')':
		addr = new_struct(PL_new_functor(functor, level), level);
		addr[level] = elem.cell;
		node_out->prec = 0;
		node_out->cell.celp = addr;
		unget_token = 0;
		return (addr);
	case ',':
		get_token(S);
		if (!(addr = read_fun(S, functor, level, node_out)))
			return (0);
		addr[level] = elem.cell;
		return (addr);
	default:
		return (0);
	}
}

static int read_list(pl_stream S, node_t * node_out)
{
	cell_t *addr;
	node_t elem;

	addr = new_cons();
	node_out->prec = 0;
	node_out->cell.celp = addr;

loop:
	if (!read_term(S, 1200, "|,]", &elem))
		return (0);
	addr[1] = elem.cell;

	switch (token.type) {
	case ']':
		addr[2].celp = new_atom(ATOM(nil));
		unget_token = 0;
		return (1);
	case '|':
		get_token(S);
		if (!read_term(S, 1200, "]", &elem) || token.type != ']')
			return (0);
		addr[2] = elem.cell;
		unget_token = 0;
		return (1);
	case ',':
		get_token(S);
		addr = addr[2].celp = new_cons();
		goto loop;
	default:
		return (0);
	}
}

static inline int read_term_a(pl_stream S, int max, const char *stop, node_t * node_out)
{
	int type, prec;
	atom_t atom = token.tok_val.atom;

// Try prefix operator
	if (PL_is_op(OP_PREFIX, atom, &type, &prec) && max >= prec) {
		node_t node_r;

		if ((type == OP_FX && read_term(S, prec - 1, stop, &node_r)) ||
		    (type == OP_FY && read_term(S, prec, stop, &node_r))) {
			mk_unary(atom, &node_r, node_out);
			node_out->prec = prec;
			return (1);
		} else
			unget_token = 1;
	}
// Try functor
	if (token.type == T_FUN) {
		get_token(S);		// skip '('
		return (read_fun(S, atom, 0, node_out) != 0);
	}
// Otherwise simple atom
	{
		node_out->cell.val = __atom(atom);
		node_out->prec = 0;
		return (1);
	}
}

static inline int read_term_t_(pl_stream S, int max, const char *stop, node_t node_l, node_t * node_out)
{

loop:
	must_be_op = 1;
	get_token(S);
	if ((token.type < 256 && !strchr(stop, token.type)) ||	// solo or punc char
	    (token.type == T_FUN || token.type == T_OP)) {
		int type, prec, m;
		atom_t atom = token.tok_val.atom;

		if (PL_is_op(OP_INFIX, atom, &type, &prec)) {
			if (max >= prec)
				switch (type) {
				case OP_XFX:
					if (node_l.prec < prec) {
						m = prec - 1;
						goto infix;
					}
					break;
				case OP_XFY:
					if (node_l.prec < prec) {
						m = prec;
						goto infix;
					}
					break;
				case OP_YFX:
					if (node_l.prec <= prec && max > prec) {
						m = prec - 1;
						goto infix;
					}
					break;
				case OP_YFY:
					if (node_l.prec <= prec && max > prec) {
						m = prec;
						goto infix;
					}
					break;
infix:					{
						node_t node_r;

						if (!read_term(S, m, stop, &node_r))
							return (0);
						mk_binary(atom, &node_l, &node_r, &node_l);
						node_l.prec = prec;
						goto loop;
					}
				default:
					break;	// impossible error occur
				}
		}

		if (PL_is_op(OP_POSTFIX, atom, &type, &prec)) {
			if (max >= prec)
				switch (type) {
				case OP_XF:
					if (node_l.prec < prec)
						goto postfix;
					break;
				case OP_YF:
					if (node_l.prec <= prec && max > prec)
						goto postfix;
					break;
postfix:				mk_unary(atom, &node_l, &node_l);
					node_l.prec = prec;
					goto loop;
				default:
					break;	// impossible error occur
				}
		}
	}

	*node_out = node_l;
	unget_token = 1;
	return (1);
}

// POST: node->prec <= max
static int read_term(pl_stream S, int max, const char *stop, node_t * node)
{
	node_t node_s;
	int tok;

	must_be_op = 0;
	tok = get_token(S);
	if (token.type < 256 && strchr(stop, token.type))
		return (0);

	switch (tok) {
	case '(':
		if (read_term(S, 1200, ")", &node_s) &&	// '(' '('
		    token.type == ')') {
			unget_token = 0;
			goto case_simple;
		} else
			PL_warning("read_term : error in '(' term ')'");
	case ')':
		goto case_term_a;
	case '{':
		if (read_term(S, 1200, "}", &node_s) &&	// '{'
		    token.type == '}')	// '{'
		{
			cell_t *addr = new_struct(FUN(curl, 1), 1);
			addr[1] = node_s.cell;
			node_s.cell.celp = addr;
			unget_token = 0;
			goto case_simple;
		} else
			PL_warning("read_term : error in '{' term '}'");
	case '}':
		goto case_term_a;
	case '[':
		if (!read_list(S, &node_s))
			PL_warning("read_term : error in '[' ... ']'");
		else
			goto case_simple;
	case T_VOID:
		node_s.cell.celp = token.tok_val.ref;
		goto case_simple;
	case T_VAR:
		node_s.cell.celp = token.tok_val.var->ref;
		goto case_simple;
	case T_INTG:
		node_s.cell.val = __intg(token.tok_val.intg);
		goto case_simple;
	case T_FLT:
		node_s.cell.celp = new_flt(token.tok_val.flt);
		goto case_simple;
	case T_ATOM:
		node_s.cell.val = __atom(token.tok_val.atom);
		goto case_simple;
	case T_STRING:
		node_s.cell.celp = token.tok_val.ref;
		goto case_simple;
	case ']':
	case ',':
	case '|':
case_term_a:
	case T_OP:
	case T_FUN:
		if (!read_term_a(S, max, stop, &node_s))
			return (0);
		break;
	default:
		return (0);
case_simple:	node_s.prec = 0;
		must_be_op = 1;
	}
	// POST : token is the next token

	return (read_term_t_(S, max, stop, node_s, node));
}

/**********************************************************************/
/* Top function                                                       */
/**********************************************************************/
// singles : 0 -> no list; no message
// singles : 1 -> print warning message
// singles : & -> unify list of singletons
static int Read(pl_stream S, term_t term, term_t vars, term_t singles, term_t pos)
{
	node_t node;
	cell_t *addr;

	if (pos)			// FIXME : add position handling
		PL_warning("read_term/[2,3] : position(P) option not implememted");

	if (get_token(S) == T_EOF)
		addr = new_atom(ATOM(_end__of__file));
	else {
		unget_token = 1;
		if (!read_term(S, 1200, "", &node) || token.type != T_STOP)
			goto KO;
		get_token(S);
		HP[0] = node.cell;	// FIXME : can be a problem ???
		addr = HP;
		HP += 1;
	}

	if (!pl_unify(addr, term))
		goto FAIL;

	if (vars) {
		if (!pl_unify(vars, bind_vars(0)))
			goto FAIL;
	}

	if (singles) {
		if (singles == (term_t) 1)
			warn_singletons();
		else {
			if (!pl_unify(singles, bind_vars(1)))
				goto FAIL;
		}
	}

	clear_var_list();
	succeed;
KO:
	PL_warn("Read failed");
FAIL:
	clear_var_list();
	fail;
}

/**********************************************************************/
/* stuff for read_term/[2,3]                                          */
/**********************************************************************/

static term_t error;
static term_t varnames;
static term_t singletons;
static pl_opt_spec_t spec[] = { {ATOM(_syntax__errors), OPT_TERM, {.term = &error}},
{ATOM(_variable__names), OPT_TERM, {.term = &varnames}},
{ATOM(_singletons), OPT_TERM, {.term = &singletons}},
{0, 0, {0}}
};

static int PL_read_term(pl_stream S, term_t term, term_t options)
{
	int old_se, rval;

// test for eof
	if (Seof(S)) {
		term_t t = new_atom(ATOM(_end__of__file));
		// FIXME : process eof according to eof_action of the stream !.
		return (pl_unify(term, t));
	}
// Init options with default
	error = (term_t) ATOM(_fail);
	varnames = 0;
	singletons = 0;
	// position=0;
	// subposition=0;

// get options
	if (!PL_scan_options(options, spec))
		PL_warning("read_term/2 : illegal option list");
// process options
	if (error == (term_t) ATOM(_fail))
		old_se = syntaxerrors(1);
	else if (error == (term_t) ATOM(_quiet))
		old_se = syntaxerrors(0);
	else
		old_se = give_syntaxerrors;

// read the term
	rval = Read(S, term, varnames, singletons, 0);
	syntaxerrors(old_se);
	return (rval);
}

/**********************************************************************/
/* Prolog Connection                                                  */
/**********************************************************************/

int pl_read_variables(term_t t, term_t v)
{
	int rc;
	pl_stream S = PL_InStream();

	rc = Read(S, t, v, 0, 0);
	return (rc);
}

int pl_read_variables3(term_t s, term_t t, term_t v)
{
	int rc;
	pl_stream S = PL_Input_Stream(s);

	rc = Read(S, t, v, 0, 0);

	return (rc);
}

int pl_read(term_t t)
{
	pl_stream S = PL_InStream();

	return (Read(S, t, 0, 0, 0));
}

int pl_read2(term_t s, term_t t)
{
	pl_stream S = PL_Input_Stream(s);

	return (Read(S, t, 0, 0, 0));
}

int pl_read_clause(term_t t)
{
	pl_stream S = PL_InStream();

	// FIXME : singletons warning should depend of style_check(singletons)
	//         set to default
	return (Read(S, t, 0, (term_t) 1, 0));
}

int pl_read_clause2(term_t s, term_t t)
{
	pl_stream S = PL_Input_Stream(s);

	// FIXME : singletons warning should depend of style_check(singletons)
	//         set to default
	return (Read(S, t, 0, (term_t) 1, 0));
}

int pl_read_term(term_t term, term_t options)
{
	pl_stream S = PL_InStream();

	return (PL_read_term(S, term, options));
}

int pl_read_term3(term_t stream, term_t term, term_t options)
{
	int rval;
	pl_stream S = PL_Input_Stream(stream);

	if (!S)
		fail;
	rval = PL_read_term(S, term, options);
	return (rval);
}

/*******************************************************/

int pl_number_atom(term_t num, term_t a)
{
	const char *s;
	pl_stream S;

	if (PL_get_atom_chars(a, &s)) {
		S = Sopen_rmem(s, SM_READ, 0);
		if (!S) {		// err_msg
			fail;
		}

		if (!Read(S, num, 0, 0, 0) || !PL_is_number(num)) {	// err_msg
			Sclose(S);
			fail;
		}
		Sclose(S);
		succeed;
	}

	fail;
}

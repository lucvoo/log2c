/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#include "pl-ctype.h"

pl_ctype_t PL__char_type[] = {
	0,				// -1 (EOF)
	ct_space | ct_ctrl,		// '\x00'
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		//   .
	ct_space | ct_ctrl,		// '\x1F'
	ct_space,			// ' '
	ct_solo,			// '!'
	ct_dq,				// '"'
	ct_symbol,			// '#'
	ct_symbol,			// '$'
	ct_solo,			// '%'
	ct_symbol,			// '&'
	ct_sq,				// '\''
	ct_punc | ct_paren,		// '('
	ct_punc | ct_paren,		// ')'
	ct_symbol,			// '*'
	ct_symbol,			// '+'
	ct_punc,			// ','
	ct_symbol,			// '-'
	ct_symbol,			// '.'
	ct_symbol,			// '/'
	ct_digit | ct_hex | ct_oct | ct_bin,	// '0'
	ct_digit | ct_hex | ct_oct | ct_bin,	// '1'
	ct_digit | ct_hex | ct_oct,	// '2'
	ct_digit | ct_hex | ct_oct,	// '2'
	ct_digit | ct_hex | ct_oct,	// '4'
	ct_digit | ct_hex | ct_oct,	// '5'
	ct_digit | ct_hex | ct_oct,	// '6'
	ct_digit | ct_hex | ct_oct,	// '7'
	ct_digit | ct_hex,		// '8'
	ct_digit | ct_hex,		// '9'
	ct_symbol,			// ':'
	ct_solo,			// ';'
	ct_symbol,			// '<'
	ct_symbol,			// '='
	ct_symbol,			// '>'
	ct_symbol,			// '?'
	ct_symbol,			// '@'
	ct_upper | ct_hex,		// 'A'
	ct_upper | ct_hex,		// 'B'
	ct_upper | ct_hex,		// 'C'
	ct_upper | ct_hex,		// 'D'
	ct_upper | ct_hex,		// 'E'
	ct_upper | ct_hex,		// 'F'
	ct_upper,			// 'G'
	ct_upper,			// 'H'
	ct_upper,			// 'I'
	ct_upper,			// 'J'
	ct_upper,			// 'K'
	ct_upper,			// 'L'
	ct_upper,			// 'M'
	ct_upper,			// 'N'
	ct_upper,			// 'O'
	ct_upper,			// 'P'
	ct_upper,			// 'Q'
	ct_upper,			// 'R'
	ct_upper,			// 'S'
	ct_upper,			// 'T'
	ct_upper,			// 'U'
	ct_upper,			// 'V'
	ct_upper,			// 'W'
	ct_upper,			// 'X'
	ct_upper,			// 'Y'
	ct_upper,			// 'Z'
	ct_punc | ct_paren,		// '['
	ct_symbol,			// '\\'
	ct_punc | ct_paren,		// ']'
	ct_symbol,			// '^'
	ct_upper | ct_undscr,		// '_'  FIXME
	ct_bq,				// '`'
	ct_lower | ct_hex,		// 'a'
	ct_lower | ct_hex,		// 'b'
	ct_lower | ct_hex,		// 'c'
	ct_lower | ct_hex,		// 'd'
	ct_lower | ct_hex,		// 'e'
	ct_lower | ct_hex,		// 'f'
	ct_lower,			// 'g'
	ct_lower,			// 'h'
	ct_lower,			// 'i'
	ct_lower,			// 'j'
	ct_lower,			// 'k'
	ct_lower,			// 'l'
	ct_lower,			// 'm'
	ct_lower,			// 'n'
	ct_lower,			// 'o'
	ct_lower,			// 'p'
	ct_lower,			// 'q'
	ct_lower,			// 'r'
	ct_lower,			// 's'
	ct_lower,			// 't'
	ct_lower,			// 'u'
	ct_lower,			// 'v'
	ct_lower,			// 'w'
	ct_lower,			// 'x'
	ct_lower,			// 'y'
	ct_lower,			// 'z'
	ct_punc | ct_paren,		// '{'
	ct_punc,			// '|'
	ct_punc | ct_paren,		// '}'
	ct_symbol,			// '~'
	ct_space | ct_ctrl,		// '^?'
	ct_space, ct_ctrl,		// '\x80'
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   '\x8F'
	ct_space, ct_ctrl,		//   '\x90'
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   .
	ct_space, ct_ctrl,		//   '\x9F'
	ct_space,			//   '\xA0'
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   .
	ct_symbol,			//   '\xBF'
	ct_upper,			//   '\xC0'
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_symbol,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_upper,			//   .
	ct_lower,			//   '\xDF'
	ct_lower,			//   '\xE0'
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_symbol,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower,			//   .
	ct_lower			// '\xFF'
};

/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef PL_CTYPE_H_
#define PL_CTYPE_H_

#define ct_bits(n)	(1<<n)

typedef enum 
{ ct_lower = ct_bits(0),
  ct_upper = ct_bits(1),
  ct_digit = ct_bits(2),
  ct_solo  = ct_bits(3),
  ct_symbol= ct_bits(4),
  ct_sq    = ct_bits(5),
  ct_dq    = ct_bits(6),
  ct_bq    = ct_bits(7),
  ct_punc  = ct_bits(8),
  ct_space = ct_bits(9),
  ct_paren = ct_bits(10),
  ct_blank = ct_bits(11),
  ct_bin   = ct_bits(12),
  ct_oct   = ct_bits(13),
  ct_hex   = ct_bits(14),
  ct_ctrl  = ct_bits(15),
  ct_undscr= ct_bits(16),
} pl_ctype_t;

extern pl_ctype_t char_type[];	/* array of character types */

#define isSpace(c)	(char_type[(c) + 1] & ct_space)
#define isDigit(c)	(char_type[(c) + 1] & ct_digit)
#define isLower(c)	(char_type[(c) + 1] & ct_lower)
#define isUpper(c)	(char_type[(c) + 1] & ct_upper)
#define isSymbol(c)	(char_type[(c) + 1] & ct_symbol)
#define isPunct(c)	(char_type[(c) + 1] & ct_punc)
#define isSolo(c)	(char_type[(c) + 1] & ct_solo)
#define isLetter(c)	(char_type[(c) + 1] & (ct_upper | ct_lower))	// FIXME 
#define isAlpha(c)	(char_type[(c) + 1] & (ct_upper | ct_lower))	// FIXME
#define isAlpha_(c)	(char_type[(c) + 1] & (ct_upper | ct_lower | ct_undscr))	// FIXME
#define isAlphaNum(c)	(char_type[(c) + 1] & (ct_upper | ct_lower | ct_digit))
#define isAlphaNum_(c)	(char_type[(c) + 1] & (ct_upper | ct_lower | ct_digit | ct_undscr))
#define isBinDigit(c)	(char_type[(c) + 1] & ct_bin)
#define isOctDigit(c)	(char_type[(c) + 1] & ct_oct)
#define isHexDigit(c)	(char_type[(c) + 1] & ct_hex)
#define isControl(c)	(char_type[(c) + 1] & ct_ctrl)

#define toLower(c)	((isUpper(c) && c!='_')	\
                         ? ((c) - 'A' + 'a') : (c))
#define toUpper(c)	(isLower(c)		\
			 ? ((c) - 'a' + 'A') : (c))

#define MatchParen(c)	((c) == '(' ? ')' :\
			        '{' ? '}' :\
			        '[' ? ']' : 0)


#endif	// PL_CTYPE_H_

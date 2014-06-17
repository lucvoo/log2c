/************************************************************************/
/* Copyright (c) 1998-2014 Luc Van Oostenryck. All rights reserved.	*/
/*									*/
/* %@%LICENSE								*/
/*									*/
/************************************************************************/

#ifndef	_ATOMS_H_
#define _ATOMS_H_

#define ATOM_and 		 ATOM___2F_5C
#define ATOM_ar_equals 		 ATOM___3D_3A_3D
#define ATOM_ar_larger 		 ATOM___3E
#define ATOM_ar_larger_equal 	 ATOM___3E_3D
#define ATOM_ar_not_equal 	 ATOM___3D_5C_3D
#define ATOM_ar_smaller 	 ATOM___3C
#define ATOM_ar_smaller_equal 	 ATOM___3D_3C
#define ATOM_at_equals 		 ATOM___3D_40_3D
#define ATOM_at_larger 		 ATOM___40_3E
#define ATOM_at_larger_equal 	 ATOM___40_3E_3D
#define ATOM_at_not_equals 	 ATOM___5C_3D_40_3D
#define ATOM_at_smaller 	 ATOM___40_3C
#define ATOM_at_smaller_equal 	 ATOM___40_3D_3C
#define ATOM_backslash 		 ATOM___5C
#define ATOM_bar 		 ATOM___7C
#define ATOM_comma 		 ATOM___2C
#define ATOM_curl 		 ATOM___7B_7D
#define ATOM_div 		 ATOM___2F_2F
#define ATOM_divide 		 ATOM___2F
#define ATOM_dot 		 ATOM___2E
#define ATOM_doublestar 	 ATOM___2A_2A
#define ATOM_grammar 		 ATOM___2D_2D_3E
#define ATOM_hat 		 ATOM___5E
#define ATOM_ifthen 		 ATOM___2D_3E
#define ATOM_lshift 		 ATOM___3C_3C
#define ATOM_minus 		 ATOM___2D
#define ATOM_module 		 ATOM___3A
#define ATOM_namevar 		 ATOM___24VARNAME
#define ATOM_nil 		 ATOM___5B_5D
#define ATOM_not_provable 	 ATOM___5C_2B
#define ATOM_not_strick_equals 	 ATOM___5C_3D_3D
#define ATOM_not_unifiable 	 ATOM___5C_3D
#define ATOM_numbervar 		 ATOM___24VAR
#define ATOM_obtain 		 ATOM___3F
#define ATOM_or 		 ATOM___5C_2F
#define ATOM_percent 		 ATOM___25
#define ATOM_plus 		 ATOM___2B
#define ATOM_prove 		 ATOM___3A_2D
#define ATOM_query 		 ATOM___3F_2D
#define ATOM_rshift 		 ATOM___3E_3E
#define ATOM_semicolon 		 ATOM___3B
#define ATOM_softcut 		 ATOM___2A_2D_3E
#define ATOM_star 		 ATOM___2A
#define ATOM_str_pos 		 ATOM___24stream__position
#define ATOM_strick_equals 	 ATOM___3D_3D
#define ATOM_unify 		 ATOM___3D
#define ATOM_univ 		 ATOM___3D_2E_2E

extern struct atom ATOM__;
extern struct atom ATOM___2B_3E;
extern struct atom ATOM___3A_2B;
extern struct atom ATOM__alias;
extern struct atom ATOM__append;
extern struct atom ATOM__at;
extern struct atom ATOM__binary;
extern struct atom ATOM__bindvars;
extern struct atom ATOM__character__escapes;
extern struct atom ATOM__cpu__time;
extern struct atom ATOM__curly__notation;
extern struct atom ATOM__end__of__file;
extern struct atom ATOM__eof__action;
extern struct atom ATOM__eof__code;
extern struct atom ATOM__error;
extern struct atom ATOM__execute;
extern struct atom ATOM__exist;
extern struct atom ATOM__fail;
extern struct atom ATOM__false;
extern struct atom ATOM__fx;
extern struct atom ATOM__fy;
extern struct atom ATOM__heap__stack;
extern struct atom ATOM__ignore__ops;
extern struct atom ATOM__input;
extern struct atom ATOM__is;
extern struct atom ATOM__list__notation;
extern struct atom ATOM__local__stack;
extern struct atom ATOM__mark;
extern struct atom ATOM__mod;
extern struct atom ATOM__module__transparent;
extern struct atom ATOM__max__depth;
extern struct atom ATOM__namevars;
extern struct atom ATOM__none;
extern struct atom ATOM__not;
extern struct atom ATOM__numbervars;
extern struct atom ATOM__off;
extern struct atom ATOM__on;
extern struct atom ATOM__output;
extern struct atom ATOM__past;
extern struct atom ATOM__quiet;
extern struct atom ATOM__quoted;
extern struct atom ATOM__read;
extern struct atom ATOM__real__time;
extern struct atom ATOM__rem;
extern struct atom ATOM__reposition;
extern struct atom ATOM__reset;
extern struct atom ATOM__runtime;
extern struct atom ATOM__sheap__stack;
extern struct atom ATOM__singletons;
extern struct atom ATOM__space__args;
extern struct atom ATOM__syntax__errors;
extern struct atom ATOM__system;
extern struct atom ATOM__system__time;
extern struct atom ATOM__text;
extern struct atom ATOM__trail__stack;
extern struct atom ATOM__true;
extern struct atom ATOM__type;
extern struct atom ATOM__update;
extern struct atom ATOM__user;
extern struct atom ATOM__user__error;
extern struct atom ATOM__user__input;
extern struct atom ATOM__user__output;
extern struct atom ATOM__user__time;
extern struct atom ATOM__variable__names;
extern struct atom ATOM__write;
extern struct atom ATOM__xf;
extern struct atom ATOM__xfx;
extern struct atom ATOM__xfy;
extern struct atom ATOM__xor;
extern struct atom ATOM__yf;
extern struct atom ATOM__yfx;
extern struct atom ATOM__yfy;
extern struct atom ATOM_and;
extern struct atom ATOM_ar_equals;
extern struct atom ATOM_ar_larger;
extern struct atom ATOM_ar_larger_equal;
extern struct atom ATOM_ar_not_equal;
extern struct atom ATOM_ar_smaller;
extern struct atom ATOM_ar_smaller_equal;
extern struct atom ATOM_at_equals;
extern struct atom ATOM_at_larger;
extern struct atom ATOM_at_larger_equal;
extern struct atom ATOM_at_not_equals;
extern struct atom ATOM_at_smaller;
extern struct atom ATOM_at_smaller_equal;
extern struct atom ATOM_backslash;
extern struct atom ATOM_bar;
extern struct atom ATOM_comma;
extern struct atom ATOM_curl;
extern struct atom ATOM_div;
extern struct atom ATOM_divide;
extern struct atom ATOM_dot;
extern struct atom ATOM_doublestar;
extern struct atom ATOM_grammar;
extern struct atom ATOM_hat;
extern struct atom ATOM_ifthen;
extern struct atom ATOM_lshift;
extern struct atom ATOM_minus;
extern struct atom ATOM_module;
extern struct atom ATOM_namevar;
extern struct atom ATOM_nil;
extern struct atom ATOM_not_provable;
extern struct atom ATOM_not_strick_equals;
extern struct atom ATOM_not_unifiable;
extern struct atom ATOM_numbervar;
extern struct atom ATOM_or;
extern struct atom ATOM_percent;
extern struct atom ATOM_plus;
extern struct atom ATOM_prove;
extern struct atom ATOM_rshift;
extern struct atom ATOM_semicolon;
extern struct atom ATOM_softcut;
extern struct atom ATOM_star;
extern struct atom ATOM_strick_equals;
extern struct atom ATOM_unify;
extern struct atom ATOM_univ;

#define FUN_curl_1		 FUN___7B_7D_1
#define FUN_comma_2 		 FUN___2C_2
#define FUN_div_2 		 FUN___2F_2F_2
#define FUN_divide_2 		 FUN___2F_2
#define FUN_dot_2 		 FUN___2E_2
#define FUN_minus_1 		 FUN___2D_1
#define FUN_minus_2 		 FUN___2D_2
#define FUN_module_2 		 FUN___3A_2
#define FUN_plus_2 		 FUN___2B_2
#define FUN_star_2 		 FUN___2A_2
#define FUN_str_pos_3 		 FUN___24stream__position_3
#define FUN_unify_2 		 FUN___3D_2

extern struct functor FUN__alias_1;
extern struct functor FUN__dt_6;
extern struct functor FUN__eof__action_1;
extern struct functor FUN__end__of__stream_1;
extern struct functor FUN__file__name_1;
extern struct functor FUN__ip_4;
extern struct functor FUN__max_2;
extern struct functor FUN__min_2;
extern struct functor FUN__mod_2;
extern struct functor FUN__mode_1;
extern struct functor FUN__pipe_1;
extern struct functor FUN__position_1;
extern struct functor FUN__reposition_1;
extern struct functor FUN__type_1;
extern struct functor FUN_comma_2;
extern struct functor FUN_curl_1;
extern struct functor FUN_div_2;
extern struct functor FUN_divide_2;
extern struct functor FUN_dot_2;
extern struct functor FUN_minus_1;
extern struct functor FUN_minus_2;
extern struct functor FUN_module_2;
extern struct functor FUN_plus_2;
extern struct functor FUN_star_2;
extern struct functor FUN_str_pos_3;
extern struct functor FUN_unify_2;

#endif

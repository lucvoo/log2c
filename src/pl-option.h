/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef PL_OPTIONS_H_
#define PL_OPTIONS_H_

typedef enum { OPT_BOOL, OPT_INTG, OPT_ATOM, OPT_TERM } opt_type_t;

typedef union { long   *intg;
                bool   *bool;
          const char*  *str;
		atom_t *atom;
                cell_t *cell;
                term_t *term;
              } opt_val;

typedef struct { atom_t     name;
                 opt_type_t type;
		 opt_val    val;
               } opt_spec_t, *OptSpec;
		         
extern
int scan_options(term_t options, OptSpec spec);


#endif	// PL_OPTIONS_H_

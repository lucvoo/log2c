/****************************************************************/
/* Copyright (c) 1998 Luc Van Oostenryck. All rights reserved.	*/
/*								*/
/****************************************************************/

#ifndef PL_OPTIONS_H_
#define PL_OPTIONS_H_

typedef enum { OPT_BOOL, OPT_INTG, OPT_ATOM, OPT_TERM } pl_opt_type_t;

typedef union { long   *intg;
                bool   *bool;
          const char*  *str;
		atom_t *atom;
                cell_t *cell;
                term_t *term;
              } pl_opt_val;

typedef struct { const atom_t		name;
                 const pl_opt_type_t	type;
		       pl_opt_val	val;
               } pl_opt_spec_t, *pl_opt_spec;
		         
extern
int PL_scan_options(term_t options, pl_opt_spec spec);


#endif	// PL_OPTIONS_H_
